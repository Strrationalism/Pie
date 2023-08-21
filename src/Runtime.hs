{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Runtime
  ( runtime
  , getStrings
  , quotedIfNecessary
  , quotedWords ) where

import AST
import Control.Concurrent (newMVar, MVar, readMVar, takeMVar, putMVar)
import Control.Concurrent.MVar (swapMVar)
import Control.Monad (zipWithM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Fixed (mod')
import Data.Foldable (foldl', forM_)
import Data.List (dropWhileEnd)
import Data.Maybe (isJust)
import Data.Ord (clamp)
import Data.Text (Text, pack)
import Data.Traversable (forM)
import Error
import Eval
import GHC.Float.RealFracMethods (roundDoubleInt)
import GHC.IO.Exception (ExitCode(ExitFailure))
import qualified System.Environment
import qualified System.FilePattern.Directory
import System.Directory
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath hiding (splitPath)
import System.FilePattern ( (?==) )
import System.Process (readCreateProcessWithExitCode, shell)
import GHC.IO (unsafePerformIO)

type PieSyntax = [PieExpr] -> PieEval PieExpr
type PieFunc = [PieValue] -> PieEval PieValue'

invalidArg :: PieEval a
invalidArg = fail "Invalid Arguments."

list2String :: [PieValue] -> String
list2String = unwords . map valueToString

runtime :: PieEnv
runtime = fmap wrapLib (syntaxes ++ map (second wrapFunc) functions)
  where
    wrapFunc :: PieFunc -> PieSyntax
    wrapFunc f exprs = do
      args <- mapM evalExpr exprs
      result <- f args
      return $ PieExprAtom $ noErrorInfo result
    wrapLib :: (String, PieSyntax) -> (String, PieValue)
    wrapLib (name, f) = (name ,) $ noErrorInfo $ PieHaskellFunction name $
      \args context -> runEval (f args) context

-- Syntaxes

if' :: PieSyntax
if' [condition, a, b] = do
  c <- evalExpr condition
  case c of
    (UnError (PieBool True)) -> return a
    (UnError (PieBool False)) -> return b
    _ -> fail "Invalid if syntax."
if' [condition, a] = do
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition, a, PieExprAtom $ noErrorInfo PieNil ]
if' _ = fail "Invalid if syntax."

do' :: PieSyntax
do' = fmap PieExprAtom . evalStatements

let' :: PieSyntax
let' [] = fail "Invalid let syntax."
let' args =
  let bindings = init args
      expr = last args
      result = runWithDefinesSyntax bindings $ evalExpr expr
  in PieExprAtom <$> result

cond :: PieSyntax
cond [PieExprList [condition, body]] =
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition, body, PieExprAtom $ noErrorInfo PieNil ]
cond (PieExprList [condition, body] : others) =
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition
    , body
    , PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "cond") others]
cond _ = fail "Invalid cond syntax."

foreach :: PieSyntax
foreach (PieExprAtom (UnError (PieSymbol i)) : range : body) = do
  range' <- evalExpr range
  let f v = runWithNewVar i v $ evalStatements body
      nil = pure (PieExprAtom $ noErrorInfo PieNil)
  case range' of
    (UnError (PieList ls)) ->
      forM_ (map noErrorInfo ls) f
      >> nil
    (UnError (PieString str)) ->
      forM_ (map (noErrorInfo . PieNumber . fromIntegral . fromEnum) str) f
      >> nil
    (UnError PieNil) -> nil
    _ -> fail "Invalid foreach syntax."
foreach _ = fail "Invalid foreach syntax."

lambda :: PieSyntax
lambda (PieExprAtom (UnError (PieSymbol param)) : body) = do
  env' <- pieEvalContextEnv <$>  getContext
  pure $ PieExprAtom $ noErrorInfo $
    PieLambda Nothing (Left param) body env'
lambda (PieExprList params : body) = do
  env' <- pieEvalContextEnv <$>  getContext
  params' <- forM params $ \case
    (PieExprSymbol sym) -> pure sym
    _ -> invalidArg
  pure $ PieExprAtom $ noErrorInfo $
    PieLambda Nothing (Right params') body env'
lambda _ = invalidArg

catch :: PieSyntax
catch (x : xs) = do
  r <- runProtected $ evalStatements xs
  case r of
    Right x' -> pure $ PieExprAtom x'
    Left err ->
      pure $ PieExprList
        [x, PieExprAtom $ noErrorInfo $ PieString $ pieEvalErrorMessage err]
catch _ = invalidArg

rec' :: PieSyntax
rec' [x] = do
  c <- pieEvalContextEnv <$> getContext
  let mySelfBody = PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "rec") [x]
      mySelfFunc = noErrorInfo $ PieLambda Nothing (Right []) [mySelfBody] c
  x' <- evalExpr x
  pure $ PieExprList1 (PieExprAtom x') [PieExprAtom mySelfFunc]
rec' _ = invalidArg

syntaxes :: [(String, PieSyntax)]
syntaxes =
  [ ("if", if')
  , ("do", do')
  , ("let", let')
  , ("cond", cond)
  , ("foreach", foreach)
  , ("lambda", lambda)
  , ("catch", catch)
  , ("rec", rec') ]


-- Functions
{-# NOINLINE displayLock #-}
displayLock :: MVar ()
displayLock = unsafePerformIO $ newMVar ()

display :: PieFunc
display args = do
  enabled <- pieEvalContextPrintEnabled <$> getContext
  when enabled $ liftIO $ do
    takeMVar displayLock
    putStrLn (list2String args)
    putMVar displayLock ()
  >> pure PieNil

trace' :: PieFunc
trace' [x] = do
  enabled <- pieEvalContextPrintEnabled <$> getContext
  when enabled $ liftIO $ putStrLn $ valueToString x
  pure $ unError x
trace' _ = invalidArg

list :: PieFunc
list = pure . PieList . fmap unError

isTypeOf :: (PieValue' -> Bool) -> PieFunc
isTypeOf _ [] = pure $ PieBool False
isTypeOf c a = pure $ PieBool $ all (c . unError) a

numericOperator :: (Double -> Double -> Double) -> PieFunc
numericOperator _ [UnError (PieNumber x)] = pure $ PieNumber x
numericOperator f [UnError (PieNumber x), UnError (PieNumber y)] =
  pure $ PieNumber $ f x y
numericOperator f ((UnError (PieNumber x)) : (UnError (PieNumber y) : xs)) =
  numericOperator f (noErrorInfo (PieNumber $ f x y) : xs)
numericOperator _ _ = invalidArg

booleanOperator :: (Bool -> Bool -> Bool) -> PieFunc
booleanOperator f (UnError (PieBool x):xs) = do
  let getBool (UnError (PieBool a)) = pure a
      getBool _ = invalidArg
  booleans <- mapM getBool xs
  pure $ PieBool $ foldl' f x booleans
booleanOperator _ _ = invalidArg

comparisonOperator :: (PieValue' -> PieValue' -> PieEval Bool) -> PieFunc
comparisonOperator f (x1:x2:xs) =
  let xs' = map unError $ x1:x2:xs
      booleans = zipWithM f (init xs') (tail xs')
      result = fmap and booleans in PieBool <$> result
comparisonOperator _ _ = invalidArg

comparisonOperator' :: (Double -> Double -> Bool) -> PieFunc
comparisonOperator' f xs =
  flip comparisonOperator xs $ curry $
    \case (PieNumber a, PieNumber b) -> pure $ f a b
          _ -> invalidArg

not' :: PieFunc
not' [UnError (PieBool b)] = pure $ PieBool $ not b
not' _ = invalidArg

add :: PieFunc
add x@(UnError (PieNumber _):_) = numericOperator (+) x
add x@(UnError (PieList _):_) = do
  lists <- mapM (\case (UnError (PieList ls)) -> pure ls; (UnError PieNil) -> pure []; _ -> invalidArg) x
  pure $ PieList $ concat lists
add x@(UnError (PieString _):_) = do
  strings <- mapM (\case (UnError (PieString ls)) -> pure ls; (UnError PieNil) -> pure []; _ -> invalidArg) x
  pure $ PieString $ concat strings
add _ = invalidArg

isEmpty :: PieFunc
isEmpty xs = do
  booleans <- forM (map unError xs) $ \case
    PieNil -> pure True
    PieList x -> pure $ null x
    PieString x -> pure $ null x
    _ -> invalidArg
  pure $ PieBool $ and booleans

cons :: PieFunc
cons xs'@(_:_) =
  case (init xs', last xs') of
    (a, UnError (PieList xs)) -> pure $ PieList $ map unError a ++ xs
    (a, UnError (PieString xs)) -> do
      str <- forM a $ \case (UnError (PieNumber x)) -> pure x; _ -> invalidArg
      pure $ PieString $ map (toEnum . roundDoubleInt) str ++ xs
    (a, UnError PieNil) -> pure $ PieList $ map unError a
    _ -> invalidArg
cons _ = invalidArg

makeVar :: PieFunc
makeVar [UnError v] = PieVar <$> liftIO (newMVar v)
makeVar _ = invalidArg

getVar :: PieFunc
getVar [UnError (PieVar m)] = liftIO $ readMVar m
getVar _ = invalidArg

setVar :: PieFunc
setVar [UnError (PieVar m), UnError v] = liftIO $ swapMVar m v
setVar _ = invalidArg

shell'' :: PieFunc
shell'' args = do
  let shellCommand = list2String args
  (exitCode, out, err) <-
    liftIO $ readCreateProcessWithExitCode (shell shellCommand) ""
  case exitCode of
    ExitFailure exitCode' ->
      fail $
        "Command \"" ++ shellCommand ++ "\" failed, exit code: " ++
        show exitCode' ++ "\n\n" ++ "StdOut:\n" ++ makeIndent 1 ++ out ++
        "\n\n" ++ "StdErr:\n" ++ makeIndent 1 ++ err
    ExitSuccess -> pure $ PieString out

lines' :: PieFunc
lines' [UnError (PieString s)] = pure $ PieList $ map PieString $ lines s
lines' _ = invalidArg

unlines'' :: PieFunc
unlines'' [UnError(PieList s)] =
  pure $ PieString $ unlines' $ map (quotedIfNecessary . valueToString') s
unlines'' _ = undefined

quotedIfNecessary :: String -> String
quotedIfNecessary x
  | ' ' `notElem` x = x
  | head x /= '\"' || last x /= '\"' = "\"" ++ x ++ "\""
  | otherwise = x

unwords'' :: PieFunc
unwords'' [UnError (PieList ls)] =
  pure $ PieString $ unwords $ map (quotedIfNecessary . valueToString') ls
unwords'' _ = undefined

quotedWords :: String -> [String]
quotedWords = map (reverse . snd) . filter (/= (False, "")) . f False ""
  where f :: Bool -> String -> String -> [(Bool, String)]
        f False str (' ':r) = (False, str) : f False "" r
        f False str ('\"':r) = (False, str) : f True "" r
        f False str (x:r) = f False (x:str) r
        f True str ('\"':r) = (True, str) : f False "" r
        f True str (x:r) = f True (x:str) r
        f _ str [] = [(False, str)]

words' :: PieFunc
words' [UnError (PieString s)] =
  pure $ PieList $ map PieString $ quotedWords s
words' _ = invalidArg

decodeString' :: PieFunc
decodeString' x = pure $ PieList $ decodeString $ list2String x

encodeString :: [PieValue'] -> PieEval String
encodeString x = do
  doubles <- forM x $ \case
    PieNumber x' -> pure x'
    _ -> invalidArg
  pure $ map (toEnum . roundDoubleInt) doubles

decodeString :: String -> [PieValue']
decodeString = map $ PieNumber . fromIntegral . fromEnum

encodeString' :: PieFunc
encodeString' [UnError (PieList x)] = PieString <$> encodeString x
encodeString' _ = invalidArg

getStrings :: [PieValue] -> PieEval [String]
getStrings strings = forM (map unError strings) $ \case
  PieString s -> pure s
  _ -> invalidArg

matchFiles :: PieFunc
matchFiles (UnError (PieString base):xs) = do
  patterns <- getStrings xs
  r <- liftIO $ System.FilePattern.Directory.getDirectoryFiles base patterns
  pure $ PieList $ map PieString r
matchFiles _ = invalidArg

isMatchFilePattern :: PieFunc
isMatchFilePattern (UnError (PieString pattern):files) = do
  files' <- getStrings files
  pure $ PieBool $ all (pattern ?==) files'
isMatchFilePattern _ = invalidArg

ensureDir :: PieFunc
ensureDir dirs = do
  dirs' <- getStrings dirs
  liftIO $ forM_ dirs' $ createDirectoryIfMissing True
  pure PieNil

delete :: PieFunc
delete paths = do
  paths' <- getStrings paths
  liftIO $ forM_ paths' removePathForcibly
  pure PieNil

listDir :: PieFunc
listDir [] = listDir [noErrorInfo $ PieString "."]
listDir xs = do
  xs' <- getStrings xs
  r <- liftIO $ forM xs' listDirectory
  pure $ PieList $ map PieString $ concat r

tempDir :: PieFunc
tempDir [] = do
  tmpDir <- liftIO getTemporaryDirectory
  pure $ PieString tmpDir
tempDir _ = invalidArg

path :: (String -> IO String) ->PieFunc
path k subPaths = do
  let xs = map valueToString subPaths
  PieString <$> liftIO (k $ joinPath xs)

copy :: PieFunc
copy [UnError (PieString src), UnError (PieString dst)] =
  liftIO (copyFile src dst) >> pure PieNil
copy _ = invalidArg

pathExists :: (String -> IO Bool) -> PieFunc
pathExists c files = do
  f <- getStrings files
  liftIO (PieBool . and <$> mapM c f)

findExecutable' :: PieFunc
findExecutable' [UnError (PieString exe)] = do
  exeAbsPath <- liftIO $ findExecutable exe
  case exeAbsPath of
    Nothing -> fail $ "Can not find \"" ++ exe ++ "\""
    Just x -> pure $ PieString x
findExecutable' _ = invalidArg

executableExists :: PieFunc
executableExists xs = do
  exes <- getStrings xs
  x <- liftIO $ forM exes findExecutable
  pure $ PieBool $ all isJust x

readFile' :: PieFunc
readFile' [UnError (PieString f)] = PieString <$> liftIO (readFile f)
readFile' _ = invalidArg

writeFile' :: PieFunc
writeFile' [UnError (PieString path'''), UnError (PieString content)] =
  liftIO (writeFile path''' content) >> pure PieNil
writeFile' _ = invalidArg

env :: PieFunc
env [UnError (PieString name), UnError defaultValue] = do
  e <- liftIO $ System.Environment.lookupEnv name
  pure $ maybe defaultValue PieString e
env [UnError (PieString name)] = do
  e <- liftIO $ System.Environment.lookupEnv name
  case e of
    Nothing -> fail $ "Can not find environment variable \"" ++ name ++ "\"."
    Just x -> pure $ PieString x
env _ = undefined

envPath :: PieFunc
envPath [] = PieList <$> (map PieString <$> liftIO getSearchPath)
envPath _ = invalidArg

id' :: PieFunc
id' [UnError e] = pure e
id' _ = invalidArg

mapString :: (String -> String) -> PieFunc
mapString f [UnError (PieString s)] = pure $ PieString $ f s
mapString _ _ = invalidArg

changeExt :: PieFunc
changeExt [UnError (PieString ext), UnError (PieString name)] =
  pure $ PieString $ replaceExtension name ext
changeExt _ = invalidArg

splitPath :: PieFunc
splitPath [UnError (PieString p)] =
  pure $ PieList $ map PieString $ splitDirectories p
splitPath _ = invalidArg

pathEq :: PieFunc
pathEq [] = invalidArg
pathEq [_] = invalidArg
pathEq paths = do
  paths' <- getStrings paths
  pure $ PieBool $ and $
    zipWith equalFilePath (tail paths') (init paths')

relPath :: PieFunc
relPath [UnError (PieString base), UnError (PieString p)] =
  pure $ PieString $ makeRelative base p
relPath [p] = relPath [noErrorInfo $ PieString ".", p]
relPath _ = invalidArg

absPath :: PieFunc
absPath [UnError (PieString p)] = do
  p' <- liftIO $ makeAbsolute p
  pure $ PieString p'
absPath _ = invalidArg

isXXXPath :: (String -> Bool) -> PieFunc
isXXXPath f paths = do
  p <- getStrings paths
  pure $ PieBool $ all f p

mapList :: ([PieValue'] -> PieValue') -> PieFunc
mapList f [UnError (PieList ls)] = do
  pure $ f ls
mapList f [UnError (PieString ls)] = do
  let x = f $ map (PieNumber . fromIntegral . fromEnum) ls
  case x of
    PieList ls' -> PieString <$> encodeString ls'
    x' -> pure x'
mapList _ _ = invalidArg

mapListI :: (Int -> [PieValue'] -> PieValue') -> PieFunc
mapListI f [UnError (PieNumber n), ls] = mapList (f (roundDoubleInt n)) [ls]
mapListI _ _ = invalidArg

abs' :: PieFunc
abs' [UnError (PieNumber x)] = pure $ PieNumber $ if x >= 0 then x else -x
abs' _ = invalidArg

clamp' :: PieFunc
clamp' [UnError (PieNumber bottom), UnError (PieNumber top), UnError (PieNumber v)] =
  pure $ PieNumber $ clamp (bottom, top) v
clamp' _ = undefined

intRange :: PieFunc
intRange [UnError (PieNumber a'), UnError (PieNumber b')] =
  let a = roundDoubleInt a'
      b = roundDoubleInt b'
      r = if a > b then reverse [ b .. a ] else [ a .. b ]
  in pure $ PieList $ map (PieNumber . fromIntegral) r
intRange _ = invalidArg

concat' :: PieFunc
concat' [] = invalidArg
concat' strings@(UnError (PieString _) : _ : _) = do
  strings' <- getStrings strings
  pure $ PieString $ concat strings'
concat' lists@(UnError (PieList _) : _ : _) = do
  lists' <- forM (map unError lists) $ \case
    PieList ls -> pure ls
    _ -> invalidArg
  pure $ PieList $ concat lists'
concat' [UnError (PieList args)] = concat' $ map noErrorInfo args
concat' _ = invalidArg

slice :: PieFunc
slice [UnError (PieNumber s), UnError (PieNumber l), UnError x] = do
  let start = roundDoubleInt s
      len = roundDoubleInt l
      f = take len . drop start
  case x of
    PieList x' -> pure $ PieList $ f x'
    PieString x' -> pure $ PieString $ f x'
    _ -> invalidArg
slice _ = invalidArg

evalPieFunc :: Text -> PieFunc
evalPieFunc pieCode =
  let func = PieExprAtom $ evalPieCodeUnsafe pieCode runtime in
    \args -> unError <$> evalExpr (PieExprList1 func $ fmap PieExprAtom args)

evalPieLambda :: [String] -> String -> PieFunc
evalPieLambda params body = evalPieFunc $ pack $
  "(rec (lambda (self) (lambda (" ++ unwords params ++ ") " ++ body ++ ")))"

map' :: PieFunc
map' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) " ++
    "ls " ++
    "(cons " ++
      "(f (car ls)) " ++
      "((self) f (cdr ls))))"

filter' :: PieFunc
filter' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) " ++
    "ls " ++
    "(do " ++
      "(defines (x (car ls)) " ++
               "(xs ((self) f (cdr ls)))) " ++
      "(if (f x) (cons x xs) xs)))"

flatMap' :: PieFunc
flatMap' = evalPieLambda ["f", "ls"]
  "(concat (map f ls))"

exists' :: PieFunc
exists' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) false " ++
    "(if (f (car ls)) true ((self) f (cdr ls))))"

takeWhile' :: PieFunc
takeWhile' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) ls " ++
    "(if (f (car ls)) " ++
      "(cons (car ls) ((self) f (cdr ls))) " ++
      "(cond ((string? ls) \"\") ((list? ls) (list)) (true (invalid-arg))) ))"

skipWhile' :: PieFunc
skipWhile' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) ls " ++
    "(if (f (car ls)) ((self) f (cdr ls)) ls) )"

invoke :: PieFunc
invoke [UnError (PieList args)] =
  fmap unError <$> evalExpr $ PieExprList $ map (PieExprAtom . noErrorInfo) args
invoke _ = invalidArg

functions :: [(String, PieFunc)]
functions =
  [ ("display", display)
  , ("trace", trace')
  , ("error", fail . list2String)
  , ("list", list)
  , ("nil?", isTypeOf (== PieNil))
  , ("list?", isTypeOf $ \case PieList _ -> True; _ -> False)
  , ("number?", isTypeOf $ \case PieNumber _ -> True; _ -> False)
  , ("bool?", isTypeOf $ \case PieBool _ -> True; _ -> False)
  , ("string?", isTypeOf $ \case PieString _ -> True; _ -> False)
  , ("function?", isTypeOf $ \case PieLambda {} -> True; PieHaskellFunction _ _ -> True; _ -> False)
  , ("empty?", isEmpty)
  , ("+", add)
  , ("-", numericOperator (-))
  , ("*", numericOperator (*))
  , ("/", numericOperator (/))
  , ("%", numericOperator mod')
  , ("and", booleanOperator (&&))
  , ("or", booleanOperator (||))
  , ("not", not')
  , ("eq?", comparisonOperator $ \a b -> pure $ a == b)
  , ("ne?", comparisonOperator $ \a b -> pure $ a /= b)
  , (">", comparisonOperator' (>))
  , (">=", comparisonOperator' (>=))
  , ("<", comparisonOperator' (<))
  , ("<=", comparisonOperator' (<=))
  , ("string", pure . PieString . list2String)
  , ("string-quoted", pure . PieString . quotedIfNecessary . list2String)
  , ("invalid-arg", const invalidArg)
  , ("car", mapList head)
  , ("cdr", mapList $ PieList . tail)
  , ("cons", cons)
  , ("var", makeVar)
  , ("var?", isTypeOf $ \case PieVar _ -> True; _ -> False)
  , ("get-var", getVar)
  , ("set-var", setVar)
  , ("shell", shell'')
  , ("unlines", unlines'')
  , ("lines", lines')
  , ("unwords", unwords'')
  , ("words", words')
  , ("decode-string", decodeString')
  , ("encode-string", encodeString')
  , ("match-files", matchFiles)
  , ("match-files?", isMatchFilePattern)
  , ("ensure-dir", ensureDir)
  , ("delete", delete)
  , ("list-dir", listDir)
  , ("temp-dir", tempDir)
  , ("path", path pure)
  , ("copy", copy)
  , ("abs-path", path makeAbsolute)
  , ("normalize-path", path $ pure . normalise)
  , ("file-exists", pathExists doesFileExist)
  , ("dir-exists", pathExists doesDirectoryExist)
  , ("path-exists", pathExists doesPathExist)
  , ("find-exec", findExecutable')
  , ("exec-exists", executableExists)
  , ("write-file", writeFile')
  , ("read-file", readFile')
  , ("env", env)
  , ("env-path", envPath)
  , ("id", id')
  , ("ext", mapString takeExtension)
  , ("filename", mapString takeFileName)
  , ("filename-no-ext", mapString takeBaseName)
  , ("parent-dir", mapString takeDirectory)
  , ("change-ext", changeExt)
  , ("split-path", splitPath)
  , ("path-eq?", pathEq)
  , ("abs-path", absPath)
  , ("rel-path", relPath)
  , ("rel-path?", isXXXPath isRelative)
  , ("abs-path?", isXXXPath isAbsolute)
  , ("valid-path?", isXXXPath isValid)
  , ("length", mapList $ PieNumber . fromIntegral . length)
  , ("init", mapList $ PieList . init)
  , ("last", mapList last)
  , ("reverse", mapList $ PieList . reverse)
  , ("take", mapListI $ \i -> PieList . take i)
  , ("skip", mapListI $ \i -> PieList . drop i)
  , ("nth", mapListI $ flip (!!))
  , ("abs", abs')
  , ("clamp", clamp')
  , ("int-range", intRange)
  , ("concat", concat')
  , ("slice", slice)
  , ("string-trim-start", mapString $ dropWhile isSpace)
  , ("string-trim-end", mapString $ dropWhileEnd isSpace)
  , ("string-trim", mapString $ dropWhileEnd isSpace . dropWhile isSpace)
  , ("map", map')
  , ("filter", filter')
  , ("flat-map", flatMap')
  , ("exists", exists')
  , ("take-while", takeWhile')
  , ("skip-while", skipWhile')
  , ("invoke", invoke)
  ]
