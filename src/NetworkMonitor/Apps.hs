module NetworkMonitor.Apps
  ( AppEmit (..)
  , computeAppEmits
  , renderAppLines
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import NetworkMonitor.Animate (blockBar)
import NetworkMonitor.Flow (HostEmit (..))
import NetworkMonitor.Format (formatRate, padL, padR)

data AppEmit = AppEmit
  { appName :: !String
  , appTxRate :: !Double
  , appRxRate :: !Double
  , appConns :: !Int
  , appHosts :: !String
  }
  deriving (Eq, Show)

computeAppEmits :: [HostEmit] -> [AppEmit]
computeAppEmits emits =
  sortBy (comparing (Down . appTxRate)) $
    Map.elems $
      Map.fromListWith
        merge
        [ (emitProcess e, toApp e)
        | e <- emits
        , emitProcess e /= "?"
        ]
  where
    toApp e =
      AppEmit
        { appName = emitProcess e
        , appTxRate = emitTxRate e
        , appRxRate = emitRxRate e
        , appConns = emitConns e
        , appHosts = emitHost e
        }

    merge a b =
      a
        { appTxRate = appTxRate a + appTxRate b
        , appRxRate = appRxRate a + appRxRate b
        , appConns = appConns a + appConns b
        , appHosts =
            if emitHostIn a (appHosts b)
              then appHosts a
              else take 24 (appHosts a ++ "," ++ appHosts b)
        }

    emitHostIn a hosts = appHosts a `elem` splitHosts hosts

    splitHosts = map (take 16) . filter (not . null) . splitOnComma

    splitOnComma s = case break (== ',') s of
      (a, ',' : rest) -> a : splitOnComma rest
      (a, _) -> [a]

renderAppLines :: Int -> Bool -> [AppEmit] -> [String]
renderAppLines tick _colorOn apps =
  if null apps
    then ["  (no per-app traffic yet)"]
    else
      [ "  "
          ++ padR 12 (appName a)
          ++ padL 10 (formatRate (appTxRate a))
          ++ padL 10 (formatRate (appRxRate a))
          ++ padL 4 (show (appConns a))
          ++ "  "
          ++ padR 18 (appHosts a)
          ++ "  "
          ++ blockBar tick (appTxRate a) (peak apps) 8
      | a <- take 10 apps
      ]
  where
    peak xs = maximum (1 : map appTxRate xs)
