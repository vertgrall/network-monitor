module Main where

import NetworkMonitor.CLI
  ( Command (..)
  , Options (..)
  , mergeSessionOptions
  , parseOptions
  , runCommand
  )
import NetworkMonitor.Menu (runMenu)
import NetworkMonitor.Session (loadSession)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runMenu
    else do
      session <- loadSession
      optsRaw <- parseOptions
      let opts = mergeSessionOptions session optsRaw
      case optCommand opts of
        Menu -> runMenu
        _ -> runCommand opts
