module Eval where

import AST
import Error

data EvalContext = EvalContext
  { env :: [(String, PieValue)]
  , callStack :: [ErrorInfo]
  }

