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
invalidArg = runtimeError "Invalid Arguments."

list2String :: [PieValue] -> String
list2String = unwords . map showAtom

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
      \args context -> unwrapEval (f args) context

-- Syntaxes

if' :: PieSyntax
if' [condition, a, b] = do
  c <- evalExpr condition
  case c of
    (UnError (PieBool True)) -> return a
    (UnError (PieBool False)) -> return b
    _ -> invalidArg
if' _ = invalidArg

syntaxes :: [(String, PieSyntax)]
syntaxes =
  [ ("if", if') ]


-- Functions

display :: PieFunc
display args = liftIO $ putStrLn (list2String args) >> pure PieNil

functions :: [(String, PieFunc)]
functions =
  [ ("display", display) ]
