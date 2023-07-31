{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Tops ( parseExports ) where

import AST
import Error
import Eval
import Parser ( parseFromFile )
import Control.Monad.IO.Class ( liftIO )
import System.Exit ( exitFailure )
import System.FilePath (equalFilePath, takeDirectory, joinPath)
import Control.Monad (forM, forM_, when)
import Runtime (getStrings, runtime)
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Data.List (find)

pattern PieTopDefinition ::
  String -> Maybe ErrorInfo -> [PieExpr] -> PieExpr
pattern PieTopDefinition sym err xs <-
  PieExprList1 (PieExprAtom (WithErrorInfo (PieSymbol sym) err)) xs

data PieImportState = PieImportState
  { pieImportStateRoute :: [FilePath]
  , pieImportStateAlreadyImported :: IORef [(FilePath, PieEnv)] }

loadExports :: PieImportState -> [PieExpr] -> PieEval PieEnv
loadExports _ [] = pure []
loadExports impState (PieTopDefinition "import" err imports : next) = do
  let currentFileName = head $ pieImportStateRoute impState
  imports' <- mapM evalExpr imports
  imports'' <- getStrings imports'
  forM_ imports'' $ \importPath ->
    when (any (importPathEquals importPath) $ pieImportStateRoute impState) $
      runtimeError' err $
        "Import cycle on \"" ++ importPath ++
        "\" and \"" ++ currentFileName ++ "\"."
  env <- forM imports'' $ importExports impState
  runWithNewVars (concat env) $ loadExports impState next
loadExports i (PieTopDefinition "define" _ d : next) =
  runWithDefineSyntax d $ loadExports i next
loadExports i (PieTopDefinition "defines" _ d : next) =
  runWithDefinesSyntax d $ loadExports i next
loadExports _ (PieTopDefinition "task" _ _ : _) =
  undefined
loadExports i (PieTopDefinition "action" e (PieExprSymbol n : b) : k) = do
  env <- pieEvalContextEnv <$> getContext
  let action = PieTopAction n b env
  runWithNewVar n (WithErrorInfo action e) (loadExports i k)
loadExports i (PieTopDefinition "export" err e : next) = do
  exports <- forM e $ \case
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
importExports importState path = do
  curAlready <- liftIO $ readIORef $
    pieImportStateAlreadyImported importState
  case find (importPathEquals path . fst) curAlready of
    Just x -> pure $ snd x
    Nothing -> do
      let lastFile = head $ pieImportStateRoute importState
          lastFileDir = takeDirectory lastFile
      file <- liftIO $ parseFromFile $ joinPath [lastFileDir, path]
      case file of
        Right x -> do
          e <-
            runInEnv runtime $
              flip loadExports x $ importState
                { pieImportStateRoute = path : pieImportStateRoute importState }
          liftIO $ writeIORef (pieImportStateAlreadyImported importState) $
            (path, e) : curAlready
          return e
        Left x -> liftIO (putStrLn x >> exitFailure)

parseExports :: FilePath -> PieEval PieEnv
parseExports path = do
  a <- liftIO $ newIORef []
  flip importExports path $ PieImportState
    { pieImportStateAlreadyImported = a
    , pieImportStateRoute = [] }
