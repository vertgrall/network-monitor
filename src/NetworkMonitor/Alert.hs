module NetworkMonitor.Alert
  ( detectNewFlows
  , blockedHosts
  , suspiciousFlows
  , thresholdExceeded
  , renderAlertLines
  ) where

import Data.List (isInfixOf)
import NetworkMonitor.Flow (Flow (..), flowKey)
import NetworkMonitor.Services (serviceName)

detectNewFlows :: Maybe [Flow] -> [Flow] -> [Flow]
detectNewFlows Nothing _ = []
detectNewFlows (Just prev) cur =
  let prevKeys = map flowKey prev
   in filter (\f -> flowKey f `notElem` prevKeys) cur

blockedHosts :: [String] -> [Flow] -> [Flow]
blockedHosts blocklist flows =
  [ f
  | f <- flows
  , let h = flowHost f
  , any (\b -> not (null b) && (b == h || b `isInfixOf` h)) blocklist
  ]

suspiciousFlows :: [Flow] -> [Flow]
suspiciousFlows flows =
  filter
    ( \f ->
        flowPort f `notElem` commonPorts
          && flowState f == "ESTABLISHED"
          && flowHost f /= "127.0.0.1"
    )
    flows
  where
    commonPorts = [80, 443, 53, 5223, 8080, 8443]

thresholdExceeded :: Double -> [Flow] -> Double -> Bool
thresholdExceeded limit _ totalRate = totalRate >= limit

renderAlertLines :: Bool -> [Flow] -> [Flow] -> [Flow] -> [String]
renderAlertLines colorOn newFlows blocked suspicious =
  let items =
        [ alert colorOn ("NEW CONNECTION: " ++ flowSummary f) | f <- take 3 newFlows]
          ++ [ alert colorOn ("BLOCKLIST HIT: " ++ flowHost f) | f <- take 3 blocked]
          ++ [ alert colorOn ("UNUSUAL PORT " ++ serviceName (flowPort f) ++ " -> " ++ flowHost f) | f <- take 3 suspicious]
   in if null items then [] else items ++ [""]
  where
    flowSummary f =
      maybe "?" id (flowProcess f)
        ++ " -> "
        ++ flowHost f
        ++ ":"
        ++ serviceName (flowPort f)

    alert False s = "  !! " ++ s
    alert True s = "\ESC[91m\ESC[1m  !! " ++ s ++ "\ESC[0m"
