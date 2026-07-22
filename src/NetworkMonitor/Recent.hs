module NetworkMonitor.Recent
  ( recordRecent
  , readRecent
  ) where

import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, hPutStrLn, openFile)

recordRecent :: String -> IO ()
recordRecent label = do
  home <- getHomeDirectory
  let dir = home ++ "/.config/new-tower"
      path = dir ++ "/recent.txt"
  createDirectoryIfMissing True dir
  prev <- readRecentFile path 4
  now <- getCurrentTime
  let entry = show now ++ "  " ++ label
      next = take 5 (entry : prev)
  h <- openFile path WriteMode
  mapM_ (hPutStrLn h) next
  hClose h

readRecent :: Int -> IO [String]
readRecent n = do
  home <- getHomeDirectory
  readRecentFile (home ++ "/.config/new-tower/recent.txt") n

readRecentFile :: FilePath -> Int -> IO [String]
readRecentFile path n = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      h <- openFile path ReadMode
      body <- lines <$> hGetContents h
      hClose h
      pure (take n body)
