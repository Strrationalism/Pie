{-# LANGUAGE PatternSynonyms #-}
module Tops ( parseExports ) where

import AST
import Error
import Eval ( PieEval )
import Parser ( parseFromFile )
import Control.Monad.IO.Class ( liftIO )
import System.Exit ( exitFailure )

pattern PieTopDefinition ::
  String -> Maybe ErrorInfo -> [PieExpr] -> PieExpr
pattern PieTopDefinition sym err xs <-
  PieExprList1 (PieExprAtom (WithErrorInfo (PieSymbol sym) err)) xs

type PieImportPath = [FilePath]

loadExports :: PieImportPath -> [PieExpr] -> PieEval PieEnv
loadExports _ [] = pure []
loadExports _ (PieTopDefinition "import" imports err : next) =
  undefined
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

parseExports :: PieImportPath -> FilePath -> PieEval PieEnv
parseExports importPath path = do
  file <- liftIO $ parseFromFile path
  case file of
    Right x -> loadExports importPath x
    Left x -> liftIO (putStrLn x >> exitFailure)
