module Eval
  ( PieEvalContext
  , PieEval
  , runInEnv
  , runWithDefineSyntax
  , runWithDefinesSyntax
  , evalStatements
  ) where

import {-# SOURCE #-} AST
import Control.Monad.IO.Class (MonadIO)

data PieEvalContext
data PieEval x
instance (Applicative PieEval)
instance (Monad PieEval)
instance (MonadFail PieEval)
instance (MonadIO PieEval)

runInEnv :: PieEnv -> PieEval r -> PieEval r
runWithDefineSyntax :: [PieExpr] -> PieEval r -> PieEval r
runWithDefinesSyntax :: [PieExpr] -> PieEval r -> PieEval r
evalStatements :: [PieExpr] -> PieEval PieValue

