{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Runtime ( runtime, quotedIfNecessary, quotedWords ) where

import AST
import Control.Concurrent (newMVar, readMVar)
import Control.Concurrent.MVar (swapMVar)
import Control.Monad (zipWithM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Fixed (mod')
import Data.Foldable (foldl', forM_)
import Data.Maybe (isJust)
import Data.Traversable (forM)
import Error
import Eval
import GHC.IO.Exception (ExitCode(ExitFailure))
import qualified System.Environment
import qualified System.FilePattern.Directory
import System.Directory
import System.Directory.Internal.Prelude (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath (joinPath, splitSearchPath)
import System.FilePattern ( (?==) )
import System.Process (readCreateProcessWithExitCode, shell)

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

syntaxes :: [(String, PieSyntax)]
syntaxes =
  [ ("if", if')
  , ("do", do')
  , ("let", let')
  , ("cond", cond)
  , ("foreach", foreach) ]


-- Functions

display :: PieFunc
display args = do
  enabled <- pieEvalContextPrintEnabled <$> getContext
  when enabled $ liftIO $ putStrLn (list2String args)
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

car :: PieFunc
car [UnError (PieList (x:_))] = pure x
car [UnError (PieString (x:_))] = pure $ PieNumber $ fromIntegral $ fromEnum x
car _ = invalidArg

cdr :: PieFunc
cdr [UnError (PieList (_:xs))] = pure $ PieList xs
cdr [UnError (PieString (_:xs))] = pure $ PieString xs
cdr _ = invalidArg

cons :: PieFunc
cons xs'@(_:_) =
  case (init xs', last xs') of
    (a, UnError (PieList xs)) -> pure $ PieList $ map unError a ++ xs
    (a, UnError (PieString xs)) -> do
      str <- forM a $ \case (UnError (PieNumber x)) -> pure x; _ -> invalidArg
      pure $ PieString $ map (toEnum . round) str ++ xs
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

decodeString :: PieFunc
decodeString x = pure $ PieList $
  map (PieNumber . fromIntegral . fromEnum) $ list2String x

encodeString :: PieFunc
encodeString [UnError (PieList x)] = do
  doubles <- forM x $ \case
    PieNumber x' -> pure x'
    _ -> invalidArg
  pure $ PieString $ map (toEnum . round) doubles
encodeString _ = invalidArg

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

path :: PieFunc
path subPaths = do
  let xs = map valueToString subPaths
  pure $ PieString $ joinPath xs

copy :: PieFunc
copy [UnError (PieString src), UnError (PieString dst)] =
  liftIO (copyFile src dst) >> pure PieNil
copy _ = invalidArg

absPath :: PieFunc
absPath subPaths = do
  let xs = map valueToString subPaths
  liftIO $ fmap PieString $ makeAbsolute $ joinPath xs

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
envPath [] = do
  x <- liftIO (splitSearchPath <$> getEnv "PATH")
  pure $ PieList $ map PieString x
envPath _ = invalidArg

-- Runtime Functions
  -- ext
  -- filename
  -- parent-dir
  -- change-ext

-- stdlib
  -- id
  -- minBy
  -- maxBy
  -- abs
  -- clamp
  -- concat
  -- nth
  -- skip
  -- take
  -- init
  -- last
  -- length
  -- map
  -- fold
  -- flatMap
  -- filter
  -- find
  -- exists
  -- range
  -- generate
  -- reduce
  -- reverse
  -- take-while
  -- skip-while
  -- slice
  -- unfold
  -- sort-with
  -- sort-by
  -- sort
  -- sort-desc
  -- try-catch
  -- is-blank-char
  -- string-trim-start
  -- string-trim-end
  -- string-trim



functions :: [(String, PieFunc)]
functions =
  [ ("display", display)
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
  , ("eq", comparisonOperator $ \a b -> pure $ a == b)
  , ("ne", comparisonOperator $ \a b -> pure $ a /= b)
  , (">", comparisonOperator' (>))
  , (">=", comparisonOperator' (>=))
  , ("<", comparisonOperator' (<))
  , ("<=", comparisonOperator' (<=))
  , ("string", pure . PieString . list2String)
  , ("string-quoted", pure . PieString . quotedIfNecessary . list2String)
  , ("invalid-arg", const invalidArg)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("var", makeVar)
  , ("get-var", getVar)
  , ("set-var", setVar)
  , ("shell", shell'')
  , ("unlines", unlines'')
  , ("lines", lines')
  , ("unwords", unwords'')
  , ("words", words')
  , ("decode-string", decodeString)
  , ("encode-string", encodeString)
  , ("match-files", matchFiles)
  , ("match-files?", isMatchFilePattern)
  , ("ensure-dir", ensureDir)
  , ("delete", delete)
  , ("list-dir", listDir)
  , ("temp-dir", tempDir)
  , ("path", path)
  , ("copy", copy)
  , ("abs-path", absPath)
  , ("file-exists", pathExists doesFileExist)
  , ("dir-exists", pathExists doesDirectoryExist)
  , ("path-exists", pathExists doesPathExist)
  , ("find-exec", findExecutable')
  , ("exec-exists", executableExists)
  , ("write-file", writeFile')
  , ("read-file", readFile')
  , ("env", env)
  , ("env-path", envPath)
  ]
