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
  lists <- mapM (\case (UnError (PieList ls)) -> pure ls; _ -> invalidArg) x
  pure $ PieList $ concat lists
add x@(UnError (PieString _):_) = do
  strings <- mapM (\case (UnError (PieString ls)) -> pure ls; _ -> invalidArg) x
  pure $ PieString $ concat strings
add _ = invalidArg

-- car (list/string)
-- cdr (list/string)
-- cons (list/string)

-- make-var
-- set-var
-- get-var
-- shell
-- shell'
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
  ]
