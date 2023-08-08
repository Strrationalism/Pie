module TaskRunner (topoSort) where

import Task
import Data.List (intersectBy, partition)
import System.FilePath (equalFilePath)
import AST (makeIndent)

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
