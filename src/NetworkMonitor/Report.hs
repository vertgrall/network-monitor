module NetworkMonitor.Report
  ( writeSessionReport
  ) where

import Data.Time.Clock (getCurrentTime)
import NetworkMonitor.Flow (HostEmit (..))
import NetworkMonitor.Format (formatRate)
import NetworkMonitor.Intel (IntelSnapshot (..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile)

writeSessionReport :: IntelSnapshot -> [HostEmit] -> [String] -> IO FilePath
writeSessionReport intel emits alerts = do
  home <- getHomeDirectory
  let dir = home ++ "/.config/new-tower/reports"
  createDirectoryIfMissing True dir
  now <- getCurrentTime
  let path = dir ++ "/report-" ++ show now ++ ".txt"
  h <- openFile path WriteMode
  mapM_ (hPutStrLn h)
    ( [ "NT Sentinel Session Report"
      , "Generated: " ++ show now
      , ""
      , "Network Intel"
      , "  Public IP: " ++ intelPublicIp intel
      , "  Gateway:   " ++ intelGateway intel
      , "  Wi-Fi:     " ++ intelWifi intel
      , "  VPN:       " ++ intelVpn intel
      , ""
      , "Alerts"
      ]
      ++ map stripAnsi alerts
      ++ [ ""
         , "Top Remote Emitters"
         ]
      ++ [ "  " ++ emitHost e ++ "  emit " ++ formatRate (emitTxRate e) ++ "  recv " ++ formatRate (emitRxRate e)
         | e <- take 20 emits
         ]
      ++ [ "" ]
      )
  hClose h
  pure path
  where
    stripAnsi s =
      let noEsc = dropAnsi s
       in noEsc
    dropAnsi [] = []
    dropAnsi ('\ESC' : _ : rest) = dropAnsi (dropCode rest)
    dropAnsi (c : cs) = c : dropAnsi cs
    dropCode ('m' : rest) = rest
    dropCode (_ : rest) = dropCode rest
    dropCode [] = []
