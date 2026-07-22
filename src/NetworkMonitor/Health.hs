module NetworkMonitor.Health
  ( HealthReport (..)
  , computeHealth
  , renderHealthLines
  ) where

import NetworkMonitor.Format (padL, padR)

data HealthReport = HealthReport
  { healthScore :: !Int
  , healthGateway :: !String
  , healthConnections :: !Int
  , healthNotes :: ![String]
  }
  deriving (Eq, Show)

computeHealth :: Maybe Double -> Maybe Double -> Int -> Int -> HealthReport
computeHealth mPingMs mLossPct connCount ifaceErrors =
  let pingScore =
        case mPingMs of
          Nothing -> 40
          Just ms
            | ms < 30 -> 30
            | ms < 80 -> 25
            | ms < 150 -> 15
            | otherwise -> 5
      lossScore =
        case mLossPct of
          Nothing -> 20
          Just loss
            | loss <= 1 -> 20
            | loss <= 5 -> 10
            | otherwise -> 0
      connScore
        | connCount > 500 = 10
        | connCount > 200 = 20
        | otherwise = 30
      errScore
        | ifaceErrors > 100 = 0
        | ifaceErrors > 0 = 10
        | otherwise = 20
      score = min 100 (max 0 (pingScore + lossScore + connScore + errScore))
      notes =
        [ n
        | (cond, n) <-
            [ (maybe False (> 150) mPingMs, "High gateway latency")
            , (maybe False (> 5) mLossPct, "Packet loss detected")
            , (connCount > 300, "Many active connections")
            , (ifaceErrors > 0, "Interface errors present")
            ]
        , cond
        ]
   in HealthReport
        { healthScore = score
        , healthGateway =
            case mPingMs of
              Nothing -> "unreachable"
              Just ms -> show (round ms) ++ " ms"
        , healthConnections = connCount
        , healthNotes = if null notes then ["Network looks healthy"] else notes
        }

renderHealthLines :: HealthReport -> [String]
renderHealthLines r =
  [ ""
  , "  Health score : " ++ show (healthScore r) ++ " / 100"
  , "  Gateway      : " ++ healthGateway r
  , "  Connections  : " ++ show (healthConnections r)
  , ""
  , "  Notes:"
  ]
    ++ ["    • " ++ n | n <- healthNotes r]
    ++ [""]

healthBar :: Int -> String
healthBar score =
  let filled = score `div` 10
   in replicate filled '#' ++ replicate (10 - filled) '-'
