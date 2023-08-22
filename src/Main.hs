module Main (main) where

import Eval
import Runtime
import Tops
import Control.Monad.IO.Class (liftIO)
import TaskRunner (topoSort)
import Control.Monad (forM_)
import Task
import System.Directory.Internal.Prelude (getArgs)

{- runFromFile :: FilePath -> IO ()
runFromFile path = do
  result <- parseFromFile path
  case result of
    Left x -> putStrLn x
    Right y -> void $ runEval (evalStatements y) $
                PieEvalContext runtime [] True

 -}

main2 :: IO ()
main2 = do
  exports <- runEval (parseExports "D:/Repos/build.pie") $
    PieEvalContext runtime [] True Nothing []
  case findDefaultAction exports of
    Nothing -> fail "Can not find default action."
    Just x -> flip runEval (PieEvalContext runtime [] True Nothing [])$ do
      x' <- runAction (snd x) []
      let x'' = either error id (topoSort x')
      forM_ x'' $ \x''' -> do
        liftIO $ putStrLn "- Batch -"
        forM_ x''' $ \taskObj ->
          liftIO $ do
            putStr $ pieTaskDefinitionName $ pieTaskObjDefinition taskObj
            putStr " "
            putStr $ unwords (pieTaskObjInFiles taskObj)
            putStr " => "
            putStrLn $ unwords (pieTaskObjOutFiles taskObj)

main :: IO ()
main = getArgs >>= print
