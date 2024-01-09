{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Tops ( parseExports, runAction, findDefaultAction ) where

import AST
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class ( liftIO )
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Data.List (find)
import Error
import Eval
import Parser ( parseFromFile )
import Runtime (getStrings, runtime)
import System.Directory (makeAbsolute)
import System.Exit ( exitFailure )
import System.FilePath ( equalFilePath, takeDirectory, joinPath )
import Data.Functor (void)
import Task (PieTaskDefinition(..), parsePieTask, PieTaskObj)
import Data.Maybe (listToMaybe, fromMaybe)

pattern PieTopDefinition ::
  String -> Maybe ErrorInfo -> [PieExpr] -> PieExpr
pattern PieTopDefinition sym err xs <-
  PieExprList1 (PieExprAtom (WithErrorInfo (PieSymbol sym) err)) xs

data PieImportState = PieImportState
  { pieImportStateRoute :: [FilePath]
  , pieImportStateAlreadyImported :: IORef [(FilePath, PieEnv)] }

runAction :: PieValue -> [PieValue'] -> PieEval [PieTaskObj]
runAction (UnError (PieTopAction name body params env)) args = do
  when (length params /= length args) $
    fail $
      "Arguments count is not equals parameters when calling action " ++
      show name
  tasks <- liftIO $ newIORef []
  void $ runInEnv (zip params (map noErrorInfo args) ++ runtime) $
    flip runWithModifiedContext (evalStatements body) $ \ctx ->
      ctx { pieEvalContextEnv = env , pieEvalContextTasks = Just tasks}
  liftIO $ readIORef tasks
runAction (WithErrorInfo x err) _ =
  runtimeError' err $ "\'" ++ show x ++ "\' is not an action."

loadExports :: PieImportState -> [PieExpr] -> PieEval PieEnv
loadExports _ [] = do
  initObjects <- filter ((== "_init") . fst) . pieEvalContextEnv <$> getContext
  mapM_ (`runAction` []) (reverse $ map snd initObjects)
  pure []
loadExports impState (PieTopDefinition "import" err imports : next) = do
  let currentFileName = head $ pieImportStateRoute impState
  imports' <- mapM evalExpr imports
  imports'' <- getStrings imports'
  forM_ imports'' $ \importPath ->
    when (any (importPathEquals importPath) $ pieImportStateRoute impState) $
      runtimeError' err $
        "Import cycle between \"" ++ importPath ++
        "\" and \"" ++ currentFileName ++ "\"."
  env <- forM imports'' $ importExports impState
  runWithNewVars (concat env) $ loadExports impState next
loadExports i (PieTopDefinition "define" _ d : next) =
  runWithDefineSyntax d $ loadExports i next
loadExports i (PieTopDefinition "defines" _ d : next) =
  runWithDefinesSyntax d $ loadExports i next
loadExports i (PieTopDefinition "task" err' t : next) = do
  task <- parsePieTask t
  runWithNewVar (pieTaskDefinitionName task) (WithErrorInfo (PieTopTask task) err') $
    loadExports i next
loadExports i (PieTopDefinition "action" e (PieExprSymbol n : b) : k) = do
  env <- pieEvalContextEnv <$> getContext
  let action = PieTopAction n b [] env
  runWithNewVar n (WithErrorInfo action e) (loadExports i k)
loadExports i (PieTopDefinition "action" e (PieExprList1Symbol n params : b) : k) = do
  env <- pieEvalContextEnv <$> getContext
  params' <- forM params getSymbol
  let action = PieTopAction n b params' env
  runWithNewVar n (WithErrorInfo action e) (loadExports i k)
loadExports i (PieTopDefinition "export" err e : next) = do
  exports <- forM e $ \case
    PieExprAtom (WithErrorInfo (PieSymbol "_init") err') ->
      runtimeError' err' "Don't export _init object."
    PieExprAtom (WithErrorInfo (PieSymbol x) e') -> pure $ WithErrorInfo x e'
    _ -> runtimeError' err "Export syntax invalid."
  exports' <- forM exports $ \c -> (unError c ,) <$> lookupEnv c
  next' <- loadExports i next
  pure $ next' ++ reverse exports'
loadExports _ (wtf:_) =
  fail $ "Unknown top-level definition:\n" ++ prettyPrintExpr wtf

importPathEquals :: FilePath -> FilePath -> Bool
importPathEquals = equalFilePath

importExports :: PieImportState -> FilePath -> PieEval PieEnv
importExports importState relativePath = do
  liftIO $ putStrLn relativePath
  curAlready <- liftIO $ readIORef $
    pieImportStateAlreadyImported importState
  let lastFile = listToMaybe $ pieImportStateRoute importState
      lastFileDir = takeDirectory $ fromMaybe "./" lastFile
      relativeToPwdPath = joinPath [lastFileDir, relativePath]
  path' <- liftIO $  makeAbsolute relativeToPwdPath
  case find (importPathEquals path' . fst) curAlready of
    Just x -> pure $ snd x
    Nothing -> do
      file <- liftIO $ parseFromFile path'
      case file of
        Right x -> do
          e <-
            runInEnv runtime $
              flip loadExports x $ importState
                { pieImportStateRoute = path' : pieImportStateRoute importState }
          liftIO $ writeIORef (pieImportStateAlreadyImported importState) $
            (path', e) : curAlready
          return e
        Left x -> liftIO (putStrLn x >> exitFailure)

parseExports :: FilePath -> PieEval PieEnv
parseExports path = do
  a <- liftIO $ newIORef []
  flip importExports path $ PieImportState
    { pieImportStateAlreadyImported = a
    , pieImportStateRoute = [] }

findDefaultAction :: PieEnv -> Maybe (String, PieValue)
findDefaultAction = find (\case (_, UnError (PieTopAction {})) -> True; _ -> False)
