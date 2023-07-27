{-# LANGUAGE TupleSections #-}
module Runtime ( runtime ) where

import AST
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Error
import Eval

type PieSyntax = [PieExpr] -> PieEval PieExpr
type PieFunc = [PieValue] -> PieEval PieValue'

invalidArg :: PieEval a
invalidArg = fail "Invalid Arguments."

list2String :: [PieValue] -> String
list2String = unwords . map showAtom'
  where showAtom' (UnError (PieString x)) = x
        showAtom' x = showAtom x

runtime :: PieEnv
runtime = fmap wrapLib (syntaxes ++ map (second wrapFunc) functions)
  where
    wrapFunc :: PieFunc -> PieSyntax
    wrapFunc f exprs = do
      args <- mapM evalExpr exprs
      result <- f args
      return $ PieExprAtom $ noErrorInfo result
    wrapLib :: (String, PieSyntax) -> (String, PieValue)
    wrapLib (name, f) = (name ,) $ noErrorInfo $ PieHaskellFunction name $
      \args context -> runEval (f args) context

-- Syntaxes

if' :: PieSyntax
if' [condition, a, b] = do
  c <- evalExpr condition
  case c of
    (UnError (PieBool True)) -> return a
    (UnError (PieBool False)) -> return b
    _ -> invalidArg
if' _ = invalidArg

do' :: PieSyntax
do' = fmap PieExprAtom . evalStatements

syntaxes :: [(String, PieSyntax)]
syntaxes =
  [ ("if", if')
  , ("do", do') ]


-- Functions

display :: PieFunc
display args = liftIO $ putStrLn (list2String args) >> pure PieNil

error' :: PieFunc
error' = fail . list2String

functions :: [(String, PieFunc)]
functions =
  [ ("display", display)
  , ("error", error') ]
