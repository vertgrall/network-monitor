module Main where

import NetworkMonitor.CLI (Command (..), Options (..), parseOptions, runCommand)
import NetworkMonitor.Menu (runMenu)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runMenu
    else do
      opts <- parseOptions
      case optCommand opts of
        Menu -> runMenu
        _ -> runCommand opts
