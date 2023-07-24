{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where
import Error (WithErrorInfo)
import Data.SExpresso.SExpr (Sexp)

data PieValue' = PieNumber Double
               | PieBool Bool
               | PieString String
               | PieSymbol String
               deriving (Show, Eq)

type PieValue = WithErrorInfo PieValue'

type PieExpr = Sexp PieValue
