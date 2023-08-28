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

main :: IO ()
main = do
  args <- parseOptions <$> getArgs
  let context = PieEvalContext
        { pieEvalContextEnv = runtime
        , pieEvalContextCallStack = []
        , pieEvalContextPrintEnabled = True
        , pieEvalContextTasks = Nothing
        , pieEvalContextCmdArgs =
           map (second $ maybe PieNil PieString) (pieOptionOptions args)
        }

  flip runEval context $ do
    exports <- parseExports "./build.pie"
    let action =
          if null $ pieOptionActionName args
            then snd <$> findDefaultAction exports
            else lookup (pieOptionActionName args) exports
    case action of
      Just action'@(UnError (PieTopAction {})) -> do
        tasks <- runAction action' (map PieString $ pieOptionActionArgs args)
        let runner =
              if pieOptionSingleThread args
                then singleThreaded
                else multiThreaded
        errs <- runTaskBatch' runner tasks
        unless (null errs) $ liftIO $
          forM_ errs print
      _ -> runtimeError' Nothing $
        "Action " ++ pieOptionActionName args ++ " not exported."
