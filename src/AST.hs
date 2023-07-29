{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where

import Data.SExpresso.SExpr (SExpr (SAtom, SList))
import Error (WithErrorInfo (WithErrorInfo), unError, ErrorInfo)
import GHC.Float.RealFracMethods (properFractionDoubleInt)
import Control.Concurrent (MVar, readMVar)
import GHC.IO (unsafePerformIO)

-- Types

type PieEnv = [(String, PieValue)]

data PieEvalContext = PieEvalContext
  { pieEvalContextEnv :: PieEnv
  , pieEvalContextCallStack :: [WithErrorInfo String]
  , pieEvalContextPrintEnabled :: Bool }

data PieValue' = PieNumber Double
               | PieBool Bool
               | PieString String
               | PieSymbol String
               | PieList [PieValue']
               | PieNil
               | PieVar (MVar PieValue')
               | PieLambda
                  (Maybe String)
                  (Either String [String])
                  [PieExpr]
                  PieEnv
               | PieHaskellFunction
                  String
                  ([PieExpr] -> PieEvalContext -> IO PieExpr)

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
  show (PieNumber x) =
    case properFractionDoubleInt x of
      (x', 0) -> show x'
      _ -> show x
  show (PieList ls) = "(" ++ unwords (map show ls) ++ ")"
  show (PieBool True) = "true"
  show (PieBool False) = "false"
  show (PieString s) = show s
  show (PieSymbol s) = s
  show (PieVar m) = "(var " ++ show (unsafePerformIO (readMVar m)) ++ ")"
  show PieNil = "()"
  show (PieLambda _ a b _) =
    "(lambda " ++ either (++ " ") (\a' -> "(" ++ unwords a' ++ ") ") a ++
    prettyPrintExprs b ++ ")"
  show (PieHaskellFunction x _) = "(lambda <" ++ x ++ ">)"

type PieValue = WithErrorInfo PieValue'

type PieExpr = SExpr () PieValue

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

pattern PieExprList1AtomWithErrorInfo ::
  PieExpr -> Maybe ErrorInfo -> [PieExpr] -> PieExpr
pattern PieExprList1AtomWithErrorInfo x err xs <-
  PieExprList (x@(PieExprAtom (WithErrorInfo _ err)) : xs)

pattern PieExprList1Symbol :: String -> [PieExpr] -> PieExpr
pattern PieExprList1Symbol x xs <- PieExprList1 (PieExprSymbol x) xs

-- Pretty Print

valueToString' :: PieValue' -> String
valueToString' (PieString s) = s
valueToString' s = show s

valueToString :: PieValue -> String
valueToString = valueToString' . unError

makeIndent :: Int -> String
makeIndent indent = replicate (indent * indentSize) ' '
  where indentSize = 2

unlines' :: [String] -> String
unlines' [] = []
unlines' [a] = a
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

prettyPrintExprs' :: Int -> [PieExpr] -> String
prettyPrintExprs' i = unlines' . map (prettyPrintExpr' i)

prettyPrintExpr' :: Int -> PieExpr -> String
prettyPrintExpr' i (PieExprAtom atom) = makeIndent i ++ valueToString atom
prettyPrintExpr' i PieExprEmpty = makeIndent i ++ "()"
prettyPrintExpr' i (PieExprList [PieExprAtom x]) =
  makeIndent i ++ "(" ++ valueToString x ++ ")"
prettyPrintExpr' i (PieExprList [PieExprAtom a, PieExprAtom b]) =
  makeIndent i ++
  "(" ++ valueToString a ++ " " ++ valueToString b ++ ")"
prettyPrintExpr' i (PieExprList [PieExprAtom a, PieExprAtom b, PieExprAtom c]) =
  makeIndent i ++
  "(" ++ valueToString a ++ " " ++
  valueToString b ++ " " ++
  valueToString c ++ ")"
prettyPrintExpr' i (PieExprList1 (PieExprAtom x) ls) =
  makeIndent i ++
  "(" ++ valueToString x ++ "\n" ++ prettyPrintExprs' (i + 1) ls ++ ")"
prettyPrintExpr' i (PieExprList ls) =
  (makeIndent i ++ "(\n") ++ prettyPrintExprs' (i + 1) ls ++ makeIndent i ++ ")"
prettyPrintExpr' _ _ = undefined

prettyPrintExpr :: PieExpr -> String
prettyPrintExpr = prettyPrintExpr' 0

prettyPrintExprs :: [PieExpr] -> String
prettyPrintExprs = unlines . map prettyPrintExpr

prettyPrintExprsTop :: [PieExpr] -> String
prettyPrintExprsTop = unlines . map ((++ "\n") . prettyPrintExpr)
