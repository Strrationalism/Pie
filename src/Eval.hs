{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Eval where

import AST
import Control.Monad (ap, forM_)
import Data.Maybe (fromMaybe)
import Error
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
evalExpr (PieExprList1Symbol "define" _) = runtimeError "Invalid define."
evalExpr (PieExprList1Symbol "defines" _) = runtimeError "Invalid defines."
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

pattern PieExprDefine :: String -> PieExpr -> PieExpr
pattern PieExprDefine v body <-
  PieExprList1Symbol "define" [PieExprAtom (UnError (PieSymbol v)), body]

pattern PieExprDefines :: [PieExpr] -> PieExpr
pattern PieExprDefines bindings <-
  PieExprList1Symbol "defines" bindings

runWithDefine :: String -> PieExpr -> PieEval r -> PieEval r
runWithDefine name expr cont = do
  val <- evalExpr expr
  env <- pieEvalContextEnv <$> getContext
  runInEnv ((name, val) : env) cont

runWithDefines :: [(String, PieExpr)] -> PieEval r -> PieEval r
runWithDefines [] k = k
runWithDefines ((name, val):xs) k =
  runWithDefine name val $ runWithDefines xs k

pattern PieExprBinding :: String -> PieExpr -> PieExpr
pattern PieExprBinding name body <- PieExprList1Symbol name [body]

runWithDefineSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefineSyntax xs k = do
  bindings <- mapM extractBinding xs
  runWithDefines bindings k
  where extractBinding (PieExprBinding name body) = pure (name, body)
        extractBinding x =
          runtimeError $ "Invalid binding expression: " ++ show x

evalStatements :: [PieExpr] -> PieEval PieValue
evalStatements [] = return $ noErrorInfo PieNil
evalStatements ((PieExprDefine name body):cont) =
  runWithDefine name body $ evalStatements cont
evalStatements ((PieExprDefines bindings):k) =
  runWithDefineSyntax bindings $ evalStatements k
evalStatements ((PieExprList1Symbol "define" _):_) =
  runtimeError "Invalid define."
evalStatements ((PieExprList1Symbol "defines" _): _) =
  runtimeError "Invalid defines."
evalStatements [x] = evalExpr x
evalStatements (x:xs) = evalExpr x >> evalStatements xs
