module Main (main) where

import Eval
import Runtime
import Tops
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Task (PieTaskDefinition(pieTaskDefinitionName), PieTaskObj (pieTaskObjDefinition))

{- runFromFile :: FilePath -> IO ()
runFromFile path = do
  result <- parseFromFile path
  case result of
    Left x -> putStrLn x
    Right y -> void $ runEval (evalStatements y) $
                PieEvalContext runtime [] True

 -}

main :: IO ()
main = do
  exports <- runEval (parseExports "D:/Repos/build.pie") $
    PieEvalContext runtime [] True Nothing
  case findDefaultAction exports of
    Nothing -> fail "Can not find default action."
    Just x -> flip runEval (PieEvalContext runtime [] True Nothing)$ do
      x' <- runAction (snd x) []
      forM_ x' $ \taskObj ->
        liftIO $ do
          putStr $ pieTaskDefinitionName $ pieTaskObjDefinition taskObj

