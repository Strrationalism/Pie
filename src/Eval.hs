{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Eval
  ( PieEval ( PieEval )
  , PieEvalError ( pieEvalErrorMessage )
  , PieEvalContext (..)
  , evalExpr
  , evalStatements
  , getContext
  , lookupEnv
  , runInEnv
  , runWithDefinesSyntax
  , runWithDefineSyntax
  , runWithNewVar
  , runWithNewVars
  , runWithModifiedContext
  , fail
  , runtimeError'
  , tryLookupEnv
  , runEval
  , runProtected
  , evalPieCode
  , evalPieCodeUnsafe ) where

import AST
import Control.Monad (ap, when)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Error
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldl')
import Data.Data (Typeable)
import Control.Exception
import Data.Text (Text)
import Parser (parseFromText)
import System.IO.Unsafe (unsafePerformIO)
import {-# SOURCE #-} Task
import {-# SOURCE #-} Tops
import Data.IORef
import Data.Functor (void)

data PieEvalContext = PieEvalContext
  { pieEvalContextEnv :: PieEnv
  , pieEvalContextCallStack :: [WithErrorInfo String]
  , pieEvalContextPrintEnabled :: Bool
  , pieEvalContextTasks :: Maybe (IORef [PieTaskObj])
  , pieEvalContextCmdArgs :: [(String, PieValue')]
  , pieEvalContextTaskRunner :: [PieTaskObj] -> PieEval [PieEvalError] }

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
              func ++ maybe "" (\x -> pad func ++ "\t(" ++ show x ++ ")") errInfo'
          pad str = take (printCallStackFuncPadding - length str) $ cycle " "
          printCallStackFuncPadding = 8

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

runWithNewVars :: PieEnv -> PieEval r -> PieEval r
runWithNewVars [] = id
runWithNewVars ((a, b):xs) = runWithNewVar a b . runWithNewVars xs

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
evalExpr (PieExprList1AtomWithErrorInfo f errInfo args) = do
  f' <- evalExpr f
  case unError f' of
    PieLambda name params body env -> do
      args' <- mapM evalExpr args
      newEnv <-
          case params of
            Right params' ->
              if length args /= length params'
                then runtimeError' errInfo $
                  "Invalid arguments for function" ++
                  maybe "" (" " ++) name ++ "."
                else pure $ zip params' args' ++ env
            Left param -> pure $
              (param, noErrorInfo $ PieList $ map unError args') : env
      runInEnv newEnv $ runWithCallStackFrame
          (WithErrorInfo (fromMaybe "<lambda>" name) errInfo)
          (evalStatements body)
    PieHaskellFunction name f'' ->
      runWithCallStackFrame (WithErrorInfo name errInfo) $ do
        ctx <- getContext
        a <- liftIO $ f'' args ctx
        evalExpr a
    PieTopAction {} -> do
      taskSlot <- pieEvalContextTasks <$> getContext
      when (isNothing taskSlot) $
        fail "Do not call action in \"make\" body."
      args' <- mapM evalExpr args
      void $ runAction f' $ map unError args'
      return $ noErrorInfo PieNil
    PieTopTask task -> do
      args' <- mapM evalExpr args
      (taskObj, ret) <- applyTask task $ map unError args'
      taskSlot <- pieEvalContextTasks <$> getContext
      taskSlot' <- maybe (fail "Do not call task in \"make\" body.") pure taskSlot
      liftIO $ modifyIORef taskSlot' (taskObj :)
      pure $ noErrorInfo ret
    x -> runtimeError' errInfo $ show x ++ " is not callable."
evalExpr (PieExprList1 f args) = do
  f' <- PieExprAtom <$> evalExpr f
  evalExpr (PieExprList1 f' args)
evalExpr x = fail $ prettyPrintExpr x

processDefineSyntax :: [PieExpr] -> PieEval (String, PieValue)
processDefineSyntax [PieExprAtom (UnError (PieSymbol name)), body] =
  evalExpr body >>= \body' -> pure (name, body')
processDefineSyntax [PieExprList1Symbol funcName params, body] = do
  env <- pieEvalContextEnv <$> getContext
  params' <- mapM getSymbol params
  let recSelf = (funcName, noErrorInfo func)
      func = PieLambda (Just funcName) (Right params') [body] (recSelf:env)
  pure (funcName, noErrorInfo func)
processDefineSyntax _ = fail "Invalid define syntax."

runWithDefineSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefineSyntax expr c = do
  (name, val) <- processDefineSyntax expr
  runWithNewVar name val c

processDefinesSyntax :: [PieExpr] -> PieEval PieEnv
processDefinesSyntax [] = pure []
processDefinesSyntax (PieExprList xs:y) = do
  xs' <- processDefineSyntax xs
  uncurry runWithNewVar xs' $ do
    ys' <- processDefinesSyntax y
    pure $ xs' : ys'
processDefinesSyntax _ = fail "Invalid defines syntax."

runWithDefinesSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefinesSyntax xs k = do
  xs' <- processDefinesSyntax xs
  runWithNewVars xs' k

evalStatements :: [PieExpr] -> PieEval PieValue
evalStatements [] = return $ noErrorInfo PieNil
evalStatements ((PieExprList1Symbol "define" binding):k) =
  runWithDefineSyntax binding $ evalStatements k
evalStatements ((PieExprList1Symbol "defines" bindings):k) =
  runWithDefinesSyntax bindings $ evalStatements k
evalStatements [x] = evalExpr x
evalStatements (x:xs) = evalExpr x >> evalStatements xs

runProtected :: PieEval r -> PieEval (Either PieEvalError r)
runProtected k = do
  ctx <- getContext
  liftIO $ fmap Right (runEval k ctx)
    `catch` (\(e :: PieEvalError) -> pure $ Left e)
    `catch` \(e :: SomeException) -> pure $ Left $ PieEvalError
      { pieEvalErrorContext = ctx
      , pieEvalErrorMessage = displayException e
      , pieEvalErrorFileInfo = Nothing }

evalPieCode :: Text -> PieEval PieValue
evalPieCode pieCode = do
  let expr = either undefined id $ parseFromText "<haskell>" pieCode
  evalStatements expr

evalPieCodeUnsafe :: Text -> PieEnv -> PieValue
evalPieCodeUnsafe pieCode env =
  unsafePerformIO $ runEval (evalPieCode pieCode) $
    PieEvalContext
      { pieEvalContextEnv = env
      , pieEvalContextCallStack = []
      , pieEvalContextPrintEnabled = True
      , pieEvalContextTasks = Nothing
      , pieEvalContextCmdArgs = []
      , pieEvalContextTaskRunner = undefined }
