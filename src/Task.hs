{-# LANGUAGE LambdaCase #-}
module Task
  ( PieTaskDefinition ( .. )
  , PieTaskObj (..)
  , parsePieTask
  , applyTask
  ) where

import AST
import Eval
import Control.Monad (forM, when)
import Error (noErrorInfo, unError)
import System.Directory (makeAbsolute)
import Control.Monad.IO.Class (liftIO)
import System.FilePath (normalise)

data PieTaskDefinition = PieTaskDefinition
  { pieTaskDefinitionBody :: [PieExpr]
  , pieTaskDefinitionParams :: [String]
  , pieTaskDefinitionName :: String }

getStringRecursion :: [PieValue'] -> PieEval [String]
getStringRecursion exprs = do
  s <- forM exprs $ \case
    PieString s -> pure [s]
    PieList l -> getStringRecursion l
    x -> fail $ "Failed to get string: " ++ show x
  pure $ concat s

getAbsPaths :: [String] -> PieEval [String]
getAbsPaths x = liftIO $ mapM (fmap normalise . makeAbsolute) x

evalAbsPaths :: [PieExpr] -> PieEval [String]
evalAbsPaths x = do
  x' <- mapM evalExpr x
  strings <- getStringRecursion $ fmap unError x'
  getAbsPaths strings

parsePieTask :: [PieExpr] -> PieEval PieTaskDefinition
parsePieTask (PieExprList1 name params : body) = do
  name' <- getSymbol name
  params' <- mapM getSymbol params
  pure $ PieTaskDefinition
    { pieTaskDefinitionParams = params'
    , pieTaskDefinitionName = name'
    , pieTaskDefinitionBody = body }
parsePieTask (PieExprSymbol s : body) = do
  pure $ PieTaskDefinition
    { pieTaskDefinitionParams = []
    , pieTaskDefinitionName = s
    , pieTaskDefinitionBody = body }
parsePieTask _ = fail "Invalid task definition."

data PieTaskObj = PieTaskObj
  { pieTaskObjInFiles :: [FilePath]
  , pieTaskObjOutFiles :: [FilePath]
  , pieTaskObjDefinition :: PieTaskDefinition
  , pieTaskObjMakeBodies :: [([PieExpr], PieEnv)] }

applyTaskParts :: [PieExpr] -> PieValue' -> PieEval (PieTaskObj, PieValue')
applyTaskParts [] retValue =
  let taskObj = PieTaskObj
        { pieTaskObjInFiles = []
        , pieTaskObjOutFiles = []
        , pieTaskObjDefinition = undefined
        , pieTaskObjMakeBodies = [] } in pure (taskObj, retValue)
applyTaskParts (PieExprList1Symbol "define" define : next) retValue =
  runWithDefineSyntax define $ applyTaskParts next retValue
applyTaskParts (PieExprList1Symbol "defines" defines : next) retValue =
  runWithDefinesSyntax defines $ applyTaskParts next retValue
applyTaskParts (PieExprList1Symbol "do" s : next) _ =
  evalStatements s >>= applyTaskParts next . unError
applyTaskParts (PieExprList1Symbol "in" s : next) ret = do
  paths <- evalAbsPaths s
  (next', ret') <- applyTaskParts next ret
  let next'' = next' { pieTaskObjInFiles = paths ++ pieTaskObjInFiles next' }
  pure (next'', ret')
applyTaskParts (PieExprList1Symbol "out" s : next) ret = do
  paths <- evalAbsPaths s
  (next', ret') <- applyTaskParts next ret
  let next'' = next' { pieTaskObjOutFiles = paths ++ pieTaskObjOutFiles next' }
  pure (next'', ret')
applyTaskParts (PieExprList1Symbol "make" s : next) ret = do
  env <- pieEvalContextEnv <$> getContext
  (next', ret') <- applyTaskParts next ret
  let next'' = next' { pieTaskObjMakeBodies = (s, env) : pieTaskObjMakeBodies next' }
  pure (next'', ret')
applyTaskParts (wtf : _) _ = fail $
  "Unknown definition in task:" ++ makeIndent 2 ++ prettyPrintExpr wtf

applyTask :: PieTaskDefinition -> [PieValue'] -> PieEval (PieTaskObj, PieValue')
applyTask def args = do
  let params = pieTaskDefinitionParams def
  when (length params /= length args) $
    fail $
      "Invalid argument to call task " ++ show (pieTaskDefinitionName def) ++ "."
  runWithNewVars (zip params (map noErrorInfo args)) $ do
    (x, y) <- applyTaskParts (pieTaskDefinitionBody def) PieNil
    pure (x { pieTaskObjDefinition = def }, y)
