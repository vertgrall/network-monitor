module NetworkMonitor.Log
  ( appendSessionLog
  , sessionLogPath
  ) where

import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)

sessionLogPath :: IO FilePath
sessionLogPath = do
  home <- getHomeDirectory
  pure (home ++ "/.config/new-tower/sessions.log")

appendSessionLog :: String -> IO ()
appendSessionLog msg = do
  home <- getHomeDirectory
  let dir = home ++ "/.config/new-tower"
      path = dir ++ "/sessions.log"
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  h <- openFile path AppendMode
  hPutStrLn h ("[" ++ show now ++ "] " ++ msg)
  hClose h
