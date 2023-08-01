{-# LANGUAGE LambdaCase #-}
module Task
  ( PieTaskDefinition ( pieTaskDefinitionName )
  , parsePieTask
  ) where

import AST
import Eval
import Control.Monad (forM, (>=>))
import Error

data PieTaskDefinition = PieTaskDefinition
  { pieTaskDefinitionInFiles :: [FilePath]
  , pieTaskDefinitionOutFiles :: [FilePath]
  , pieTaskDefinitionMakeBody :: Maybe ([PieExpr], PieEnv)
  , pieTaskDefinitionReturns :: Maybe ([PieExpr], PieEnv)
  , pieTaskDefinitionParams :: [String]
  , pieTaskDefinitionName :: String }

getStringRecursion :: [PieValue'] -> PieEval [String]
getStringRecursion exprs = do
  s <- forM exprs $ \case
    PieString s -> pure [s]
    PieList l -> getStringRecursion l
    x -> fail $ "Failed to get string: " ++ show x
  pure $ concat s

parsePieTaskPart :: [PieExpr] -> PieEval PieTaskDefinition
parsePieTaskPart (PieExprList1Symbol "in" inputs : next) = do
  inputs' <- forM inputs $ fmap unError . evalExpr
  inputs'' <- getStringRecursion inputs'
  r <- parsePieTaskPart next
  pure $ r { pieTaskDefinitionInFiles = inputs'' ++ pieTaskDefinitionInFiles r }
parsePieTaskPart (PieExprList1Symbol "out" outputs : next) = do
  outputs' <- forM outputs $ fmap unError . evalExpr
  outputs'' <- getStringRecursion outputs'
  r <- parsePieTaskPart next
  pure $ r { pieTaskDefinitionOutFiles = outputs'' ++ pieTaskDefinitionOutFiles r }
parsePieTaskPart (PieExprList1Symbol "define" define : next) =
  runWithDefineSyntax define $ parsePieTask next
parsePieTaskPart (PieExprList1Symbol "defines" defines : next) =
  runWithDefinesSyntax defines $ parsePieTask next
parsePieTaskPart (PieExprList1Symbol "make" makeBody : next) = undefined
parsePieTaskPart (PieExprList1Symbol "return" returnBody : next) = undefined
parsePieTaskPart _ = undefined

parsePieTask :: [PieExpr] -> PieEval PieTaskDefinition
parsePieTask (nameAndArg : parts) =
  parsePieTaskPart undefined
parsePieTask _ = fail ""
