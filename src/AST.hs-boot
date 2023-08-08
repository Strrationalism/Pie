{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where

import Error
import Data.SExpresso.SExpr

type PieEnv = [(String, PieValue)]
type PieExpr = SExpr () PieValue

data PieValue'
type PieValue = WithErrorInfo PieValue'

