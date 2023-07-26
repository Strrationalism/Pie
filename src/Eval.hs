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

lookupEnv :: WithErrorInfo String -> PieEval PieValue
lookupEnv (WithErrorInfo name errInfo) = do -- TODO: print err info
  val <- tryLookupEnv name
  case val of
    Just x -> return x
    Nothing ->
      runtimeError' errInfo $
        "Could not find " ++ "\'" ++ name ++ "\' in env."

runWithModifiedContext ::
  (PieEvalContext -> PieEvalContext) -> PieEval r -> PieEval r
runWithModifiedContext f (PieEval x) = PieEval $ \ctx -> x (f ctx)

unwrapEval :: PieEval r -> PieEvalContext -> r
unwrapEval (PieEval r) = r

runtimeError :: String -> PieEval a
runtimeError = runtimeError' Nothing

runtimeError' :: Maybe ErrorInfo -> String -> PieEval a
runtimeError' _ = error -- TODO

-- Eval

evalExpr :: PieExpr -> PieEval PieValue
evalExpr (PieExprAtom (WithErrorInfo (PieSymbol symbol) errInfo)) =
  lookupEnv $ WithErrorInfo symbol errInfo
evalExpr (PieExprAtom x) = return x
evalExpr PieExprEmpty = return $ noErrorInfo PieNil
evalExpr (PieExprList1 f args) = do
  (WithErrorInfo f' errorInfo) <- evalExpr f
  args <- mapM evalExpr args
  return undefined
evalExpr _ = undefined  -- TODO

