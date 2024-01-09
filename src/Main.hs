module Main (main) where

import Eval
import Runtime
import Tops
import Control.Monad.IO.Class (liftIO)
import TaskRunner
import Option
import Data.Bifunctor
import AST
import Control.Monad
import System.Environment
import Error
import System.Directory.Internal.Prelude (exitFailure)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs >>= parseOptions
  let context = PieEvalContext
        { pieEvalContextEnv = runtime
        , pieEvalContextCallStack = []
        , pieEvalContextPrintEnabled = True
        , pieEvalContextTasks = Nothing
        , pieEvalContextCmdArgs =
           map (second $ maybe PieNil PieString) (pieOptionOptions args)
        , pieEvalContextTaskRunner =
            if pieOptionSingleThread args
                then singleThreaded
                else multiThreaded
        }

  flip runEval context $ do
    buildFileExists <- liftIO $ doesFileExist "./build.pie"
    unless buildFileExists $ liftIO $ do
      putStrLn "\"./build.pie\" not exists."
      exitFailure
    exports <- parseExports "./build.pie"
    let action =
          if null $ pieOptionActionName args
            then snd <$> findDefaultAction exports
            else lookup (pieOptionActionName args) exports
    case action of
      Just action'@(UnError (PieTopAction {})) -> do
        runAction action' (map PieString $ pieOptionActionArgs args)
      _ -> runtimeError' Nothing $
        "Action " ++ pieOptionActionName args ++ " not exported."
