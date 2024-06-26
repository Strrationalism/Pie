{-# OPTIONS_GHC -Wno-unused-imports #-}
module TaskRunner
  ( topoSort
  , runTaskBatch
  , singleThreaded
  , multiThreaded
  , runTaskBatch'
  ) where

import Task
import Data.List (intersectBy, partition)
import System.FilePath (equalFilePath)
import AST (makeIndent)
import Eval
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, foldM)
import System.Directory (doesFileExist, getModificationTime)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Foldable (foldl')
import Control.Concurrent.ParallelIO (parallel)
import Utils (allM)
import Data.HashSet (HashSet, fromList, member)

type DependencySet = HashSet FilePath

toDependencySet :: [PieTaskObj] -> DependencySet
toDependencySet ls = fromList $ ls >>= pieTaskObjOutFiles

hasDependency :: PieTaskObj -> DependencySet -> Bool
hasDependency task deps = any (`member` deps) $ pieTaskObjInFiles task

topoSort' :: [PieTaskObj] -> Either String ([PieTaskObj], [PieTaskObj])
topoSort' ls =
  case flip partition ls $ not . flip hasDependency (toDependencySet ls) of
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

type BatchRunner = [PieTaskObj] -> PieEval [PieEvalError]

taskOptimizable :: PieTaskObj -> PieEval Bool
taskOptimizable obj = allM id
  [ atLeastOneInputFiles,
    outputFileExists,
    outFileUpdated ]
  where
    outFiles = pieTaskObjOutFiles obj
    inFiles = pieTaskObjInFiles obj
    atLeastOneInputFiles :: PieEval Bool
    atLeastOneInputFiles = return $ not $ null inFiles
    outputFileExists :: PieEval Bool
    outputFileExists = allM (liftIO . doesFileExist) outFiles
    newestInModifyTime :: PieEval UTCTime
    newestInModifyTime = fmap maximum $ Control.Monad.forM inFiles $ liftIO . getModificationTime
    oldestOutModifyTime :: PieEval UTCTime
    oldestOutModifyTime = fmap minimum $ Control.Monad.forM outFiles $ liftIO . getModificationTime
    outFileUpdated :: PieEval Bool
    outFileUpdated = do
      oldestOutModifyTime' <- oldestOutModifyTime
      newestInModifyTime' <- newestInModifyTime
      return $ oldestOutModifyTime' > newestInModifyTime'

runTask :: PieTaskObj -> PieEval (Maybe PieEvalError)
runTask obj = do
  optimizable <- taskOptimizable obj
  if optimizable then pure Nothing else
    Control.Monad.foldM runBody Nothing $ pieTaskObjMakeBodies obj
  where runBody (Just x) _ = pure $ Just x
        runBody Nothing (body, env) = do
          r <- runProtected $ runInEnv env $ evalStatements body
          case r of Left exn -> pure $ Just exn
                    Right _ -> pure Nothing

runTaskBatch :: BatchRunner -> [[PieTaskObj]] -> PieEval [PieEvalError]
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
  x <- liftIO $ parallel $ map (flip runEval ctx . runTask) xs
  pure $ catMaybes x

runTaskBatch' :: BatchRunner -> [PieTaskObj] -> PieEval [PieEvalError]
runTaskBatch' runner tasks =
  case topoSort tasks of
    Left e -> runtimeError' Nothing e
    Right r -> runTaskBatch runner r

