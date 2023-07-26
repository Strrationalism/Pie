{-# LANGUAGE DeriveFunctor #-}
module Eval where

import AST
import Error
import Control.Monad (ap)

-- Pie Eval Monad

newtype PieEval r = PieEval (PieEvalContext -> r) deriving (Functor)

instance Applicative PieEval where
  pure = PieEval . const
  ( <*> ) = ap

instance Monad PieEval where
  return = pure
  (PieEval x) >>= f = PieEval $ \ctx ->
    let (PieEval y) = f $ x ctx in y ctx

getContext :: PieEval PieEvalContext
getContext = PieEval id

tryLookupEnv :: String -> PieEval (Maybe PieValue)
tryLookupEnv name = lookup name . pieEvalContextEnv <$> getContext

lookupEnv :: String -> PieEval PieValue
lookupEnv name = do
  val <- tryLookupEnv name
  case val of
    Just x -> return x
    Nothing -> runtimeError $ "Could not find \'" ++ name ++ "\' in env."

runWithModifiedContext ::
  (PieEvalContext -> PieEvalContext) -> PieEval r -> PieEval r
runWithModifiedContext f (PieEval x) = PieEval $ \ctx -> x (f ctx)

unwrapEval :: PieEval r -> PieEvalContext -> r
unwrapEval (PieEval r) = r

runtimeError :: String -> PieEval a
runtimeError = error  -- TODO: print error information

-- Eval

eval' :: PieExpr -> PieEval PieValue
eval' (PieExprAtom (UnError (PieSymbol symbol))) = lookupEnv symbol
eval' (PieExprAtom x) = return x
eval' PieExprEmpty = return $ noErrorInfo PieNil
eval' _ = undefined  -- TODO
