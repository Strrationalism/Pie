{-# LANGUAGE DeriveFunctor #-}
module Eval
  ( PieEval ( PieEval )
  , evalExpr
  , evalStatements
  , getContext
  , lookupEnv
  , runInEnv
  , runWithDefinesSyntax
  , runWithDefineSyntax
  , runWithNewVar
  , fail
  , runtimeError'
  , tryLookupEnv
  , runEval ) where

import AST
import Control.Monad (ap)
import Data.Maybe (fromMaybe, catMaybes)
import Error
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldl')
import Data.Data (Typeable)
import Control.Exception (Exception, throw)

-- Pie Eval Monad

newtype PieEval r = PieEval (PieEvalContext -> IO r) deriving (Functor)

instance Applicative PieEval where
  pure = PieEval . const . pure
  ( <*> ) = ap

instance Monad PieEval where
  return = pure
  (PieEval x) >>= f = PieEval $ \ctx -> do
    y <- x ctx
    let (PieEval y') = f y
    y' ctx

instance MonadIO PieEval where
  liftIO = PieEval . const

data PieEvalError = PieEvalError
  { pieEvalErrorContext :: PieEvalContext
  , pieEvalErrorMessage :: String
  , pieEvalErrorFileInfo :: Maybe ErrorInfo }
  deriving (Typeable)

instance Show PieEvalError where
  show err = unlines $ foldl' (\a b -> a ++ [""] ++ b) [] $ catMaybes infos
    where infos = [ errInfo, fileInfo, callStackInfo ]
          errInfo = Just [ "Error:", makeIndent 1 ++ pieEvalErrorMessage err ]
          fileInfo = flip fmap (pieEvalErrorFileInfo err) $ \e ->
            [ "File:", makeIndent 1 ++ show e ]
          callStack = pieEvalContextCallStack $ pieEvalErrorContext err
          callStackInfo = Just $ ("Call Stacks:" :) $ flip map callStack $
            \(WithErrorInfo func errInfo') ->
              makeIndent 1 ++
              func ++ maybe "" (\x -> "\t(" ++ show x ++ ")") errInfo'

instance Exception PieEvalError

getContext :: PieEval PieEvalContext
getContext = PieEval pure

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

runWithNewVar :: String -> PieValue -> PieEval r -> PieEval r
runWithNewVar name val =
  runWithModifiedContext $ \x ->
    x { pieEvalContextEnv = (name, val) : pieEvalContextEnv x }

runEval :: PieEval r -> PieEvalContext -> IO r
runEval (PieEval r) = r

instance MonadFail PieEval where
  fail = runtimeError' Nothing

runtimeError' :: Maybe ErrorInfo -> String -> PieEval a
runtimeError' errInfo msg = do
  ctx <- getContext
  let err = PieEvalError { pieEvalErrorContext = ctx
                         , pieEvalErrorMessage = msg
                         , pieEvalErrorFileInfo = errInfo }
  liftIO $ throw err

-- Eval

evalExpr :: PieExpr -> PieEval PieValue
evalExpr (PieExprAtom (WithErrorInfo (PieSymbol symbol) errInfo)) =
  lookupEnv $ WithErrorInfo symbol errInfo
evalExpr (PieExprAtom x) = return x
evalExpr PieExprEmpty = return $ noErrorInfo PieNil
evalExpr (PieExprList1Symbol "define" _) = fail "Invalid define."
evalExpr (PieExprList1Symbol "defines" _) = fail "Invalid defines."
evalExpr (PieExprList1WithErrorInfo f errInfo args) = do
  f' <- evalExpr f
  case unError f' of
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
      runWithCallStackFrame (WithErrorInfo name errInfo) $ do
        ctx <- getContext
        a <- liftIO $ f'' args ctx
        evalExpr a
    x -> runtimeError' errInfo $ show x ++ " is not callable."
evalExpr _ = undefined

getSymbol :: PieExpr -> PieEval String
getSymbol (PieExprAtom (UnError (PieSymbol x))) = pure x
getSymbol x = fail $ "Expected a symbol, got " ++ prettyPrintExpr x ++ "."

runWithDefineSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefineSyntax [PieExprAtom (UnError (PieSymbol name)), body] c =
  evalExpr body >>= \body' -> runWithNewVar name body' c
runWithDefineSyntax [PieExprList1Symbol funcName params, body] c = do
  env <- pieEvalContextEnv <$> getContext
  params' <- mapM getSymbol params
  let recSelf = (funcName, noErrorInfo func)
      func = PieLambda (Just funcName) params' body (recSelf:env)
  runWithNewVar funcName (noErrorInfo func) c
runWithDefineSyntax _ _ = fail "Invalid define syntax."

runWithDefinesSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefinesSyntax [] = id
runWithDefinesSyntax (PieExprList xs:y) =
  runWithDefineSyntax xs . runWithDefinesSyntax y
runWithDefinesSyntax _ = const $ fail "Invalid define syntax."

evalStatements :: [PieExpr] -> PieEval PieValue
evalStatements [] = return $ noErrorInfo PieNil
evalStatements ((PieExprList1Symbol "define" binding):k) =
  runWithDefineSyntax binding $ evalStatements k
evalStatements ((PieExprList1Symbol "defines" bindings):k) =
  runWithDefinesSyntax bindings $ evalStatements k
evalStatements [x] = evalExpr x
evalStatements (x:xs) = evalExpr x >> evalStatements xs

