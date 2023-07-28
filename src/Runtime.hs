{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Runtime ( runtime ) where

import AST
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Error
import Eval
import Data.Fixed (mod')
import Data.Foldable (foldl', forM_)
import Control.Monad (zipWithM, when)
import Data.Traversable (forM)
import Control.Concurrent (newMVar, readMVar)
import Control.Concurrent.MVar (swapMVar)
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(ExitSuccess))
import GHC.IO.Exception (ExitCode(ExitFailure))

type PieSyntax = [PieExpr] -> PieEval PieExpr
type PieFunc = [PieValue] -> PieEval PieValue'

invalidArg :: PieEval a
invalidArg = fail "Invalid Arguments."

list2String :: [PieValue] -> String
list2String = unwords . map showAtom'
  where showAtom' (UnError (PieString x)) = x
        showAtom' x = showAtom x

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
isEmpty [UnError (PieString s)] = pure $ PieBool $ null s
isEmpty [UnError (PieList s)] = pure $ PieBool $ null s
isEmpty [UnError PieNil] = pure $ PieBool True
isEmpty _ = invalidArg

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

-- files
-- dirs
-- path
-- ext
-- filename
-- http
-- write-text
-- change-extension
-- file-exists
-- dir-exists
-- ensure-dir
-- copy-file
-- delete-file
-- delete-dir

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
  , ("to-string", pure . PieString . list2String)
  , ("invalid-arg", const invalidArg)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("var", makeVar)
  , ("get-var", getVar)
  , ("set-var", setVar)
  , ("shell", shell'')
  ]
