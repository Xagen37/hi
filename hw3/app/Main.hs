module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Set (fromList)
import HW3.Action
    ( HIO(..), HiPermission(..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec(errorBundlePretty)

-- | REPL for testing and running Hi language.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing     -> return ()
        Just "quit" -> return ()
        Just input  -> do
          let expr = parse input
          case expr of
            Left err     -> outputStrLn $ errorBundlePretty err
            Right hiExpr -> do
              evaluated <- liftIO $ runHIO (eval hiExpr) (fromList [AllowRead, AllowWrite, AllowTime])
              case evaluated of
                Left hiErr  -> outputStrLn $ show hiErr
                Right hiVal -> outputStrLn $ show $ prettyValue hiVal
          loop
