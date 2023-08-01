module Main (main) where

import Eval
import Runtime
import Tops

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
  _ <- runEval (parseExports "D:/Repos/build.pie") $
    PieEvalContext runtime [] True
  pure ()
