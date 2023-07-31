{-# LANGUAGE PatternSynonyms #-}
module Tops ( parseExports ) where

import AST
import Error
import Eval ( PieEval, evalExpr, runWithNewVars )
import Parser ( parseFromFile )
import Control.Monad.IO.Class ( liftIO )
import System.Exit ( exitFailure )
import System.FilePath (equalFilePath)
import Control.Monad (forM)
import Runtime (getStrings)

pattern PieTopDefinition ::
  String -> Maybe ErrorInfo -> [PieExpr] -> PieExpr
pattern PieTopDefinition sym err xs <-
  PieExprList1 (PieExprAtom (WithErrorInfo (PieSymbol sym) err)) xs

type PieImportRoute = [FilePath]

loadExports :: PieImportRoute -> [PieExpr] -> PieEval PieEnv
loadExports _ [] = pure []
loadExports importRoute (PieTopDefinition "import" _ imports : next) = do
  imports' <- mapM evalExpr imports
  imports'' <- getStrings imports'
  env <- forM imports'' $ parseExports importRoute
  runWithNewVars (concat env) $ loadExports importRoute next
loadExports _ (PieTopDefinition "define" define err : next) =
  undefined
loadExports _ (PieTopDefinition "defines" defines err : next) =
  undefined
loadExports _ (PieTopDefinition "task" task err : next) =
  undefined
loadExports _ (PieTopDefinition "action" action err : next) =
  undefined
loadExports _ (PieTopDefinition "export" exports err : next) =
  undefined
loadExports _ (PieTopDefinition "init" initAction err : next) =
  undefined
loadExports _ (wtf:_) =
  fail $ "Unknown top-level definition:\n" ++ prettyPrintExpr wtf

parseExports :: PieImportRoute -> FilePath -> PieEval PieEnv
parseExports importPath path =
  if any (equalFilePath path) importPath
    then
      fail $
        "Import cycle on " ++ show (head importPath)
        ++ " and " ++ show path ++ "."
    else do
      file <- liftIO $ parseFromFile path
      case file of
        Right x -> loadExports (path:importPath) x
        Left x -> liftIO (putStrLn x >> exitFailure)
