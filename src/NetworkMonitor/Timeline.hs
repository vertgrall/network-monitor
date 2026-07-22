module NetworkMonitor.Timeline
  ( appendTimeline
  , readRecentTimeline
  ) where

import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.IO (IOMode (AppendMode, ReadMode), hClose, hGetContents, hPutStrLn, openFile)

appendTimeline :: String -> IO ()
appendTimeline msg = do
  home <- getHomeDirectory
  let dir = home ++ "/.config/new-tower"
      path = dir ++ "/timeline.log"
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  h <- openFile path AppendMode
  hPutStrLn h ("[" ++ show now ++ "] " ++ msg)
  hClose h

readRecentTimeline :: Int -> IO [String]
readRecentTimeline n = do
  home <- getHomeDirectory
  let path = home ++ "/.config/new-tower/timeline.log"
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      h <- openFile path ReadMode
      body <- lines <$> hGetContents h
      hClose h
      pure (take n (reverse body))
