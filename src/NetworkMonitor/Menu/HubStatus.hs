module NetworkMonitor.Menu.HubStatus (hubStatusLines) where

import NetworkMonitor.History (readRecentEmitSamples, renderHistorySpark)
import NetworkMonitor.Probe (PingResult (..), defaultGateway, pingHost)
import NetworkMonitor.Recent (readRecent)
import NetworkMonitor.Stats (readConnections)
import NetworkMonitor.Session (Session (..))

hubStatusLines :: Session -> IO [String]
hubStatusLines session = do
  gateway <- defaultGateway
  pingLine <-
    case gateway of
      Nothing -> pure "  Gateway : unknown"
      Just host -> do
        result <- pingHost host 1
        pure $
          case result of
            Right r -> "  Gateway : " ++ host ++ "  " ++ show (round (pingAvgMs r)) ++ " ms"
            Left _ -> "  Gateway : " ++ host ++ "  unreachable"
  conns <- length <$> readConnections
  hist <- readRecentEmitSamples 12
  recent <- readRecent 3
  pure $
    [ pingLine
    , "  Active connections : " ++ show conns
    , "  Emit trend (recent) : " ++ renderHistorySpark hist
    ]
      ++ (if null recent then [] else "  Recent:" : map ("    " ++) recent)
      ++ [""]
