{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where

import Data.SExpresso.SExpr (Sexp, SExpr (SAtom, SList))
import Error (WithErrorInfo (WithErrorInfo), unError)

-- Types

type PieEnv = [(String, PieValue)]

data PieEvalContext = EvalContext
  { pieEvalContextEnv :: PieEnv
  , pieEvalContextCallStack :: [WithErrorInfo String]
  }

data PieValue' = PieNumber Double
               | PieBool Bool
               | PieString String
               | PieSymbol String
               | PieNil
               | PieLambda (Maybe String) [String] PieExpr PieEnv
               | PieHaskellFunction
                  String
                  ([PieExpr] -> PieEvalContext -> PieExpr)

instance Eq PieValue' where
  PieNumber n == PieNumber m = n == m
  PieBool a == PieBool b = a == b
  PieString a == PieString b = a == b
  PieSymbol a == PieSymbol b = a == b
  PieNil == PieNil = True
  PieLambda a b c d == PieLambda x y z w = a == x && b == y && c == z && d == w
  PieHaskellFunction a _ == PieHaskellFunction b _ = a == b
  _ == _ = False

instance Show PieValue' where
  show (PieNumber x) = show x
  show (PieBool True) = "true"
  show (PieBool False) = "false"
  show (PieString s) = show s
  show (PieSymbol s) = s
  show PieNil = "()"
  show (PieLambda _ a b _) =
    "(lambda (" ++ unwords a ++ ") " ++ prettyPrintExpr b ++ ")"
  show (PieHaskellFunction x _) = "(lambda <" ++ x ++ ">)"

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

-- Pretty Print

showAtom :: PieValue -> String
showAtom = show . unError

makeIndent :: Int -> String
makeIndent indent = replicate (indent * indentSize) ' '
  where indentSize = 2

prettyPrintExprs' :: Int -> [PieExpr] -> String
prettyPrintExprs' i = unlines' . map (prettyPrintExpr' i)
  where unlines' :: [String] -> String
        unlines' [] = []
        unlines' [a] = a
        unlines' (x:xs) = x ++ "\n" ++ unlines' xs

prettyPrintExpr' :: Int -> PieExpr -> String
prettyPrintExpr' i (PieExprAtom atom) = makeIndent i ++ showAtom atom
prettyPrintExpr' i PieExprEmpty = makeIndent i ++ "()"
prettyPrintExpr' i (PieExprList [PieExprAtom x]) =
  makeIndent i ++ "(" ++ showAtom x ++ ")"
prettyPrintExpr' i (PieExprList [PieExprAtom a, PieExprAtom b]) =
  makeIndent i ++
  "(" ++ showAtom a ++ " " ++ showAtom b ++ ")"
prettyPrintExpr' i (PieExprList [PieExprAtom a, PieExprAtom b, PieExprAtom c]) =
  makeIndent i ++
  "(" ++ showAtom a ++ " " ++
  showAtom b ++ " " ++
  showAtom c ++ ")"
prettyPrintExpr' i (PieExprList1 (PieExprAtom x) ls) =
  makeIndent i ++
  "(" ++ showAtom x ++ "\n" ++ prettyPrintExprs' (i + 1) ls ++ ")"
prettyPrintExpr' i (PieExprList ls) =
  (makeIndent i ++ "(\n") ++ prettyPrintExprs' (i + 1) ls ++ makeIndent i ++ ")"
prettyPrintExpr' _ _ = undefined

prettyPrintExpr :: PieExpr -> String
prettyPrintExpr = prettyPrintExpr' 0

prettyPrintExprs :: [PieExpr] -> String
prettyPrintExprs = unlines . map prettyPrintExpr

prettyPrintExprsTop :: [PieExpr] -> String
prettyPrintExprsTop = unlines . map ((++ "\n") . prettyPrintExpr)
