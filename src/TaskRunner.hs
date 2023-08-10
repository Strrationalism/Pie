module TaskRunner
  ( topoSort
  , runTaskBatch
  , singleThreaded
  , multiThreaded
  ) where

import Task
import Data.List (intersectBy, partition)
import System.FilePath (equalFilePath)
import AST (makeIndent)
import Control.Exception
import Eval
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)

hasDependency :: PieTaskObj -> [PieTaskObj] -> Bool
hasDependency x ls =
  flip any ls $ \y ->
    let inFiles = pieTaskObjInFiles x
        outFiles = pieTaskObjOutFiles y
    in not $ null $ intersectBy equalFilePath inFiles outFiles

topoSort' :: [PieTaskObj] -> Either String ([PieTaskObj], [PieTaskObj])
topoSort' ls =
  case flip partition ls $ not . flip hasDependency ls of
    ([], _:_) -> Left $
      "Dependent cycle:"
      ++ unlines (fmap ((makeIndent 1 ++). pieTaskDefinitionName . pieTaskObjDefinition) ls)
    (a, b) -> Right (a, b)

topoSort :: [PieTaskObj] -> Either String [[PieTaskObj]]
topoSort ls = do
  (cur, next) <- topoSort' ls
  if null next
    then pure [cur]
    else do
      next' <- topoSort next
      pure $ cur : next'

type BatchRunner = [PieTaskObj] -> PieEval [SomeException]

runTask :: PieTaskObj -> PieEval (Maybe SomeException)
runTask = undefined

runTaskBatch :: BatchRunner -> [[PieTaskObj]] -> PieEval [SomeException]
runTaskBatch _ [] = pure []
runTaskBatch f (x:xs) = do
  e <- f x
  case e of [] -> runTaskBatch f xs
            e' -> pure e'

singleThreaded :: BatchRunner
singleThreaded xs = catMaybes <$> mapM runTask xs

multiThreaded :: BatchRunner
multiThreaded xs = do
  ctx <- getContext
  x <- liftIO $ mapM (flip runEval ctx . runTask) xs
  pure $ catMaybes x
