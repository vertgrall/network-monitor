module NetworkMonitor.Listen
  ( Listener (..)
  , readListeners
  , renderListenerLines
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing)
import NetworkMonitor.Flow (Flow (..), readFlows)
import NetworkMonitor.Format (padL, padR)
import NetworkMonitor.Services (serviceName)

data Listener = Listener
  { listenEndpoint :: !String
  , listenPort :: !Int
  , listenProcess :: !String
  , listenState :: !String
  }
  deriving (Eq, Show)

readListeners :: IO [Listener]
readListeners = do
  flows <- readFlows
  pure $
    sortBy (comparing listenPort) $
      [ Listener
          { listenEndpoint = flowLocal f
          , listenPort = flowLocalPort f
          , listenProcess = maybe "?" id (flowProcess f)
          , listenState = flowState f
          }
      | f <- flows
      , flowState f == "LISTEN"
      ]

renderListenerLines :: [Listener] -> [String]
renderListenerLines ls =
  if null ls
    then ["  (no LISTEN sockets found)"]
    else
      [ "  "
          ++ padR 24 (listenEndpoint l)
          ++ padL 6 (serviceName (listenPort l))
          ++ padR 12 (listenProcess l)
          ++ "  "
          ++ listenState l
      | l <- take 30 ls
      ]
