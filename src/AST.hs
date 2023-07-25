{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where
import Error (WithErrorInfo (WithErrorInfo))
import Data.SExpresso.SExpr (Sexp, SExpr (SAtom, SList))

-- Types

data PieValue' = PieNumber Double
               | PieBool Bool
               | PieString String
               | PieSymbol String
               deriving (Show, Eq)

type PieValue = WithErrorInfo PieValue'

type PieExpr = Sexp PieValue

-- Patterns

pattern PieExprAtom :: PieValue -> PieExpr
pattern PieExprAtom x = SAtom x

pattern PieExprList :: [PieExpr] -> PieExpr
pattern PieExprList x = SList () x

pattern PieExprSymbol :: String -> PieExpr
pattern PieExprSymbol sym <- PieExprAtom (WithErrorInfo (PieSymbol sym) _)

pattern PieExprEmpty :: PieExpr
pattern PieExprEmpty = PieExprList []

pattern PieExprList1 :: PieExpr -> [PieExpr] -> PieExpr
pattern PieExprList1 x xs = PieExprList (x:xs)

pattern PieExprList1Symbol :: String -> [PieExpr] -> PieExpr
pattern PieExprList1Symbol x xs <- PieExprList1 (PieExprSymbol x) xs

