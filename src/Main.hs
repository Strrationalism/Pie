module Main (main) where

import Parser
import AST
import Eval
import Runtime
import Control.Monad (void)
import Tops

runFromFile :: FilePath -> IO ()
runFromFile path = do
  result <- parseFromFile path
  case result of
    Left x -> putStrLn x
    Right y -> void $ runEval (evalStatements y) $
                PieEvalContext runtime [] True


main :: IO ()
main = do
  x <- runEval (parseExports "D:/Repos/build.pie") $
    PieEvalContext runtime [] True
  pure ()
