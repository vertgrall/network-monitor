module NetworkMonitor.Inbound
  ( isInboundFlow
  , filterInboundFlows
  , renderInboundPanel
  ) where

import NetworkMonitor.DNS (lookupHostLabel)
import NetworkMonitor.Flow (Flow (..))
import NetworkMonitor.Format (padL, padR)
import NetworkMonitor.HostIntel (formatGeoShort, lookupGeo)

isInboundFlow :: Flow -> Bool
isInboundFlow f =
  flowPort f >= 1024
    && (flowLocalPort f <= 1024 || flowLocalPort f `elem` commonLocalServicePorts)
  where
    commonLocalServicePorts = [3000, 3001, 4000, 5000, 5173, 5432, 5900, 6379, 8000, 8080, 8443, 8888]

filterInboundFlows :: String -> Int -> [Flow] -> [Flow]
filterInboundFlows state limit flows =
  take limit $
    filter isInboundFlow $
      filter ((== state) . flowState) flows

renderInboundRow :: Bool -> Bool -> Flow -> IO String
renderInboundRow resolveDns geoLookup f = do
  hostLabel <- if resolveDns then lookupHostLabel (flowHost f) else pure (flowHost f)
  geo <-
    if geoLookup
      then maybe "" formatGeoShort <$> lookupGeo (flowHost f)
      else pure ""
  let intel = if null geo then take 22 hostLabel else take 22 geo
  pure $
    "  "
      ++ padR 18 (flowHost f)
      ++ padR 22 intel
      ++ padL 6 (show (flowPort f))
      ++ padR 14 (":" ++ show (flowLocalPort f))
      ++ padR 12 (take 12 (maybe "?" id (flowProcess f)))

renderInboundPanel :: Bool -> Bool -> [Flow] -> IO [String]
renderInboundPanel resolveDns geoLookup flows = do
  rows <- mapM (renderInboundRow resolveDns geoLookup) flows
  pure $
    [ ""
    , "  "
        ++ padR 18 "REMOTE"
        ++ padR 22 "HOST / GEO"
        ++ padL 6 "RPORT"
        ++ padR 14 "LOCAL"
        ++ padR 12 "PROCESS"
    ]
      ++ if null rows
        then ["  (no inbound connections detected)"]
        else rows
      ++ [""]
