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
import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Fixed (mod')
import Data.Foldable (foldl', forM_)
import Data.List ( dropWhileEnd, isPrefixOf )
import Data.Maybe (isJust, fromMaybe)
import Data.Ord (clamp)
import Data.Text ( Text, pack, unpack )
import qualified Data.Text.IO.Utf8 as Utf8 (writeFile, readFile)
import Data.Traversable (forM)
import Error
import Eval
import GHC.Float.RealFracMethods (roundDoubleInt)
import qualified System.Environment
import qualified System.FilePattern.Directory
import System.Directory
import System.FilePath hiding (splitPath)
import System.FilePattern ( (?==) )
import System.Process (callCommand)
import Utils (splitList, replaceList)
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
  liftIO $ do
    takeMVar displayLock
    putStrLn (list2String args)
    putMVar displayLock ()
  >> pure PieNil

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
  let unpackArgs (PieList s) = unwords $ map unpackArgs s
      unpackArgs (PieString s) = s
      unpackArgs s = show s
      shellCommand = unwords $ map (unpackArgs . unError) args
  liftIO $ callCommand shellCommand
  return PieNil

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
readFile' [UnError (PieString f)] = PieString . unpack <$> liftIO (Utf8.readFile f)
readFile' _ = invalidArg

writeFile' :: PieFunc
writeFile' [UnError (PieString path'''), UnError (PieString content)] =
  liftIO (Utf8.writeFile path''' $ pack content) >> pure PieNil
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

fold' :: PieFunc
fold' = evalPieLambda ["f", "state", "ls"] $
  "(if (empty? ls) state " ++
    "((self) f (f state (car ls)) (cdr ls)))"

reduce' :: PieFunc
reduce' = evalPieLambda ["f", "ls"] $
  "(if (empty? ls) (error \"reduce need non empty list as argument but passed an empty list.\") " ++
    "(fold f (car ls) (cdr ls)))"

invoke :: PieFunc
invoke [UnError (PieList args)] =
  fmap unError <$> evalExpr $ PieExprList $ map (PieExprAtom . noErrorInfo) args
invoke _ = invalidArg

getOpt :: PieFunc
getOpt [UnError (PieString str)] =
  getOpt [noErrorInfo (PieString str), noErrorInfo PieNil]
getOpt [UnError (PieString str), UnError v] =
  fromMaybe v . lookup str . pieEvalContextCmdArgs <$> getContext
getOpt _ = invalidArg

defaultWith :: PieFunc
defaultWith [UnError v, UnError PieNil] = pure v
defaultWith [_, UnError v] = pure v
defaultWith _ = invalidArg

parseNumber :: PieFunc
parseNumber [UnError (PieString s)] = pure $ PieNumber $ read s
parseNumber _ = invalidArg

isPrefixOf' :: PieFunc
isPrefixOf' [UnError (PieString prefix), UnError (PieString str)] =
  pure $ PieBool $ isPrefixOf prefix str
isPrefixOf' [UnError (PieList prefix), UnError (PieList str)] =
  pure $ PieBool $ isPrefixOf prefix str
isPrefixOf' _ = invalidArg

stringSplit :: PieFunc
stringSplit [UnError (PieString subStr), UnError (PieString str)] =
  pure $ PieList $ map PieString $ splitList subStr str
stringSplit _ = invalidArg

stringReplace :: PieFunc
stringReplace [UnError (PieString subStr), UnError (PieString rep), UnError (PieString s)] =
  pure $ PieString $ replaceList subStr rep s
stringReplace _ = invalidArg

functions :: [(String, PieFunc)]
functions =
  [ ("-", numericOperator (-))
  , ("*", numericOperator (*))
  , ("/", numericOperator (/))
  , ("%", numericOperator mod')
  , ("+", add)
  , ("<", comparisonOperator' (<))
  , ("<=", comparisonOperator' (<=))
  , (">", comparisonOperator' (>))
  , (">=", comparisonOperator' (>=))
  , ("abs-path?", isXXXPath isAbsolute)
  , ("abs-path", absPath)
  , ("abs-path", path makeAbsolute)
  , ("abs", abs')
  , ("and", booleanOperator (&&))
  , ("bool?", isTypeOf $ \case PieBool _ -> True; _ -> False)
  , ("car", mapList head)
  , ("cdr", mapList $ PieList . tail)
  , ("change-ext", changeExt)
  , ("clamp", clamp')
  , ("concat", concat')
  , ("cons", cons)
  , ("copy", copy)
  , ("decode-string", decodeString')
  , ("default-with", defaultWith)
  , ("delete", delete)
  , ("dir-exists", pathExists doesDirectoryExist)
  , ("display", display)
  , ("empty?", isEmpty)
  , ("encode-string", encodeString')
  , ("ensure-dir", ensureDir)
  , ("env-path", envPath)
  , ("env", env)
  , ("eq?", comparisonOperator $ \a b -> pure $ a == b)
  , ("error", fail . list2String)
  , ("exec-exists", executableExists)
  , ("exists", exists')
  , ("ext", mapString takeExtension)
  , ("file-exists", pathExists doesFileExist)
  , ("filename-no-ext", mapString takeBaseName)
  , ("filename", mapString takeFileName)
  , ("filter", filter')
  , ("find-exec", findExecutable')
  , ("flat-map", flatMap')
  , ("fold", fold')
  , ("function?", isTypeOf $ \case PieLambda {} -> True; PieHaskellFunction _ _ -> True; _ -> False)
  , ("get-opt", getOpt)
  , ("get-var", getVar)
  , ("id", id')
  , ("init", mapList $ PieList . init)
  , ("int-range", intRange)
  , ("invalid-arg", const invalidArg)
  , ("invoke", invoke)
  , ("is-prefix-of", isPrefixOf')
  , ("last", mapList last)
  , ("length", mapList $ PieNumber . fromIntegral . length)
  , ("lines", lines')
  , ("list-dir", listDir)
  , ("list?", isTypeOf $ \case PieList _ -> True; _ -> False)
  , ("list", list)
  , ("map", map')
  , ("match-files?", isMatchFilePattern)
  , ("match-files", matchFiles)
  , ("ne?", comparisonOperator $ \a b -> pure $ a /= b)
  , ("nil?", isTypeOf (== PieNil))
  , ("normalize-path", path $ pure . normalise)
  , ("not", not')
  , ("nth", mapListI $ flip (!!))
  , ("number?", isTypeOf $ \case PieNumber _ -> True; _ -> False)
  , ("or", booleanOperator (||))
  , ("parent-dir", mapString takeDirectory)
  , ("parse-number", parseNumber)
  , ("path-eq?", pathEq)
  , ("path-exists", pathExists doesPathExist)
  , ("path", path pure)
  , ("read-file", readFile')
  , ("reduce", reduce')
  , ("rel-path?", isXXXPath isRelative)
  , ("rel-path", relPath)
  , ("reverse", mapList $ PieList . reverse)
  , ("set-var", setVar)
  , ("shell", shell'')
  , ("skip-while", skipWhile')
  , ("skip", mapListI $ \i -> PieList . drop i)
  , ("slice", slice)
  , ("split-path", splitPath)
  , ("string-quoted", pure . PieString . quotedIfNecessary . list2String)
  , ("string-replace", stringReplace)
  , ("string-split", stringSplit)
  , ("string-trim-end", mapString $ dropWhileEnd isSpace)
  , ("string-trim-start", mapString $ dropWhile isSpace)
  , ("string-trim", mapString $ dropWhileEnd isSpace . dropWhile isSpace)
  , ("string?", isTypeOf $ \case PieString _ -> True; _ -> False)
  , ("string", pure . PieString . list2String)
  , ("take-while", takeWhile')
  , ("take", mapListI $ \i -> PieList . take i)
  , ("temp-dir", tempDir)
  , ("unlines", unlines'')
  , ("unwords", unwords'')
  , ("valid-path?", isXXXPath isValid)
  , ("var?", isTypeOf $ \case PieVar _ -> True; _ -> False)
  , ("var", makeVar)
  , ("words", words')
  , ("write-file", writeFile')
  ]
