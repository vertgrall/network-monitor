module NetworkMonitor.Hosts
  ( topRemoteHosts
  , foreignAddress
  , hostPortFromEndpoint
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import NetworkMonitor.Stats (Connection (..), readConnections)

topRemoteHosts :: Int -> String -> IO [(String, Int)]
topRemoteHosts limit state = do
  conns <- readConnections
  let grouped =
        Map.fromListWith (+) $
          [ (addr, 1)
          | c <- conns
          , connState c == state
          , Just addr <- [foreignAddress (connForeign c)]
          ]
      ranked = sortBy (comparing (Down . snd)) (Map.toList grouped)
  pure (take limit ranked)

foreignAddress :: String -> Maybe String
foreignAddress s =
  hostPortFromEndpoint s >>= \(host, _) -> Just host

hostPortFromEndpoint :: String -> Maybe (String, Int)
hostPortFromEndpoint s =
  case splitHostPortFromEnd (dropWhile isSpace s) of
    (host, Just port) | not (null host) -> Just (host, port)
    _ -> Nothing

splitHostPortFromEnd :: String -> (String, Maybe Int)
splitHostPortFromEnd s =
  let (revPort, revRest) = break (== '.') (reverse s)
   in case revRest of
        ('.' : revHost) ->
          let portStr = reverse revPort
              host = reverse revHost
           in if all isDigit portStr && not (null host)
                then (host, readMaybeInt portStr)
                else (s, Nothing)
        _ -> (s, Nothing)

readMaybeInt :: String -> Maybe Int
readMaybeInt str =
  case reads str of
    [(n, _)] -> Just n
    _ -> Nothing
