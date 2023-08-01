module Task
  ( PieTask ( pieTaskName )
  , parsePieTask
  ) where

import AST
import Eval

data PieTask = PieTask
  { pieTaskInFiles :: [FilePath]
  , pieTaskOutFiles :: [FilePath]
  , pieTaskMakeBody :: ([PieExpr], PieEnv)
  , pieTaskReturns :: ([PieExpr], PieEnv)
  , pieTaskParams :: [String]
  , pieTaskName :: String }

parsePieTask :: [PieExpr] -> PieEval PieTask
parsePieTask = undefined
