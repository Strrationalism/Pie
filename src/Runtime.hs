module Runtime where

import AST
import Eval

type PieRuntimeFunc = [PieExpr] -> PieEval PieExpr

