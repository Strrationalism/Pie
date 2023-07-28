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
    _ -> fail "Invalid if syntax."
if' [condition, a] = do
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition, a, PieExprAtom $ noErrorInfo PieNil ]
if' _ = fail "Invalid if syntax."

do' :: PieSyntax
do' = fmap PieExprAtom . evalStatements

let' :: PieSyntax
let' [] = fail "Invalid let syntax."
let' args =
  let bindings = init args
      expr = last args
      result = runWithDefinesSyntax bindings $ evalExpr expr
  in PieExprAtom <$> result

cond :: PieSyntax
cond [PieExprList [condition, body], else'] =
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition, body, else' ]
cond (PieExprList [condition, body] : others) =
  pure $ PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "if")
    [ condition
    , body
    , PieExprList1 (PieExprAtom $ noErrorInfo $ PieSymbol "cond") others]
cond _ = fail "Invalid cond syntax."

syntaxes :: [(String, PieSyntax)]
syntaxes =
  [ ("if", if')
  , ("do", do')
  , ("let", let')
  , ("cond", cond) ]


-- Functions

display :: PieFunc
display args = liftIO $ putStrLn (list2String args) >> pure PieNil

error' :: PieFunc
error' = fail . list2String

list :: PieFunc
list = pure . PieList . fmap unError

-- make-var
-- set-var!
-- get-var!
-- shell
-- shell'
-- files
-- dirs
-- path
-- ext
-- filename
-- http
-- eval
-- invoke
-- +/-/*///%
-- car (list/string)
-- cdr (list/string)
-- cons (list/string)
-- and/or/not
-- nil?/string?/number?/list?/function?
-- write-text
-- change-extension
-- file-exists
-- dir-exists
-- ensure-dir
-- copy-file
-- delete-file
-- delete-dir

functions :: [(String, PieFunc)]
functions =
  [ ("display", display)
  , ("error", error')
  , ("list", list) ]
