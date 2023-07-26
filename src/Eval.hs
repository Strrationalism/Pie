{-# LANGUAGE DeriveFunctor #-}
module Eval where

import AST
import Error
import Control.Monad (ap, forM_)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import System.Exit (exitWith, ExitCode (ExitFailure))

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

runWithCallStackFrame :: WithErrorInfo String -> PieEval r -> PieEval r
runWithCallStackFrame stackFrame =
  runWithModifiedContext $ \x ->
    x { pieEvalContextCallStack = stackFrame : pieEvalContextCallStack x }

runInEnv :: PieEnv -> PieEval r -> PieEval r
runInEnv env =
  runWithModifiedContext $ \x ->
    x { pieEvalContextEnv = env }

unwrapEval :: PieEval r -> PieEvalContext -> r
unwrapEval (PieEval r) = r

runtimeError :: String -> PieEval a
runtimeError = runtimeError' Nothing

runtimeError' :: Maybe ErrorInfo -> String -> PieEval a
runtimeError' errInfo msg = do
  callStack <- pieEvalContextCallStack <$> getContext
  unsafePerformIO $ do
    putStrLn msg
    case errInfo of
      Nothing -> pure ()
      Just x -> putStrLn >> putStrLn $ "File: " ++ show x ++ "."
    putStrLn ""
    putStrLn "Call Stack:"
    forM_ callStack $ \(WithErrorInfo funcName errInfo') -> putStrLn $
      funcName ++ maybe "" (\x -> " (" ++ show x ++ ")") errInfo'
    putStrLn ""
    exitWith $ ExitFailure (-1)

-- Eval

evalExpr :: PieExpr -> PieEval PieValue
evalExpr (PieExprAtom (WithErrorInfo (PieSymbol symbol) errInfo)) =
  lookupEnv $ WithErrorInfo symbol errInfo
evalExpr (PieExprAtom x) = return x
evalExpr PieExprEmpty = return $ noErrorInfo PieNil
evalExpr (PieExprList1 f args) = do
  (WithErrorInfo f' errInfo) <- evalExpr f
  case f' of
    PieLambda name params body env ->
      if length args /= length params
        then runtimeError' errInfo $
          "Invalid arguments for function" ++
          maybe "" (" " ++) name ++ "."
        else do
          args' <- mapM evalExpr args
          runInEnv (zip params args' ++ env) $
            runWithCallStackFrame (WithErrorInfo (fromMaybe "" name) errInfo) $
              evalExpr body
    PieHaskellFunction name f'' ->
      runWithCallStackFrame (WithErrorInfo name Nothing) $ do
        ctx <- getContext
        evalExpr $ f'' args ctx
    x -> runtimeError' errInfo $ show x ++ " is not callable."
evalExpr _ = undefined
