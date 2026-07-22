module NetworkMonitor.Export
  ( exportFlowsCsv
  , exportInboundCsv
  ) where

import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import NetworkMonitor.Flow (Flow (..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile)

exportFlowsCsv :: [Flow] -> IO FilePath
exportFlowsCsv flows = writeCsv "flows" (flowHeader : map flowRow flows)

exportInboundCsv :: [Flow] -> IO FilePath
exportInboundCsv flows = writeCsv "inbound" (flowHeader : map flowRow flows)

flowHeader :: String
flowHeader = "host,remote_port,local_port,state,process"

flowRow :: Flow -> String
flowRow f =
  intercalate
    ","
    [ flowHost f
    , show (flowPort f)
    , show (flowLocalPort f)
    , flowState f
    , maybe "" id (flowProcess f)
    ]

writeCsv :: String -> [String] -> IO FilePath
writeCsv kind rows = do
  home <- getHomeDirectory
  let dir = home ++ "/.config/new-tower/exports"
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  let path = dir ++ "/" ++ kind ++ "-" ++ sanitize (show now) ++ ".csv"
  h <- openFile path WriteMode
  mapM_ (hPutStrLn h) rows
  hClose h
  pure path
  where
    sanitize = map (\c -> if c == ' ' then '-' else c)
