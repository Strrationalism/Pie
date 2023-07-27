module Main (main) where

import Parser
import AST
import Eval
import Runtime
import Control.Monad (void)


printFromFile :: FilePath -> IO ()
printFromFile path = do
  result <- parseFromFile path
  case result of Left x -> putStrLn x
                 Right y -> putStrLn $ prettyPrintExprsTop y

runFromFile :: FilePath -> IO ()
runFromFile path = do
  result <- parseFromFile path
  case result of
    Left x -> putStrLn x
    Right y -> void $ runEval (evalStatements y) $ PieEvalContext runtime []


main :: IO ()
main = runFromFile "D:/Repos/build.pie"
