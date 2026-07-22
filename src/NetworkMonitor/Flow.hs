module NetworkMonitor.Flow
  ( Flow (..)
  , HostEmit (..)
  , flowKey
  , readFlows
  , computeHostEmits
  , flowKey
  , renderFlowPanel
  , renderFlowExtras
  ) where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace)
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import NetworkMonitor.Animate (blockBar, spinnerAt)
import NetworkMonitor.Format (formatRate, padL, padR)
import NetworkMonitor.Hosts (hostPortFromEndpoint)
import NetworkMonitor.Services (serviceName)
import System.Info (os)
import System.Process (readProcess)

data Flow = Flow
  { flowLocal :: !String
  , flowForeign :: !String
  , flowHost :: !String
  , flowPort :: !Int
  , flowLocalPort :: !Int
  , flowState :: !String
  , flowSendQ :: !Int
  , flowRecvQ :: !Int
  , flowRxBytes :: !(Maybe Int64)
  , flowTxBytes :: !(Maybe Int64)
  , flowProcess :: !(Maybe String)
  }
  deriving (Eq, Show)

data HostEmit = HostEmit
  { emitHost :: !String
  , emitTxRate :: !Double
  , emitRxRate :: !Double
  , emitConns :: !Int
  , emitProcess :: !String
  , emitPorts :: !String
  , emitSendQ :: !Int
  }
  deriving (Eq, Show)

flowKey :: Flow -> String
flowKey f = flowLocal f ++ "->" ++ flowForeign f

readFlows :: IO [Flow]
readFlows = do
  procs <- readProcessMap
  out <- readProcess "netstat" (netstatArgs ++ ["-p", "tcp"]) ""
  pure $ map (attachProcess procs) (parseFlowLines out)

netstatArgs :: [String]
netstatArgs =
  case os of
    "linux" -> ["-an"]
    _ -> ["-anv"]

attachProcess :: Map String String -> Flow -> Flow
attachProcess procs f =
  f
    { flowProcess =
        Map.lookup (flowEndpointKey f) procs
          <|> Map.lookup (remoteEndpoint f) procs
          <|> lookupBySuffix procs (flowHost f ++ ":" ++ show (flowPort f))
    }

remoteEndpoint :: Flow -> String
remoteEndpoint f =
  case hostPortFromEndpoint (flowForeign f) of
    Just (host, port) -> host ++ ":" ++ show port
    Nothing -> flowForeign f

lookupBySuffix :: Map String String -> String -> Maybe String
lookupBySuffix procs needle =
  case [(k, v) | (k, v) <- Map.toList procs, needle `isSuffixOf` k] of
    ((_, cmd) : _) -> Just cmd
    _ -> Nothing

flowEndpointKey :: Flow -> String
flowEndpointKey f =
  let local = endpointForLsof (flowLocal f) f
      remote = endpointForLsof (flowForeign f) f
   in local ++ "->" ++ remote

endpointForLsof :: String -> Flow -> String
endpointForLsof endpoint flow =
  case hostPortFromEndpoint endpoint of
    Just (host, port) -> host ++ ":" ++ show port
    Nothing ->
      case flowHostPortFromEndpoint endpoint of
        Just (host, port) -> host ++ ":" ++ show port
        Nothing -> endpoint

readProcessMap :: IO (Map String String)
readProcessMap =
  case os of
    "linux" -> readProcessMapLinux
    _ -> readProcessMapDarwin

readProcessMapDarwin :: IO (Map String String)
readProcessMapDarwin = do
  out <- readProcess "lsof" ["-iTCP", "-n", "-P"] ""
  pure $
    foldl
      ( \acc line ->
          case parseLsofLine line of
            Nothing -> acc
            Just (cmd, local, remote) ->
              Map.insert (local ++ "->" ++ remote) cmd $
                Map.insert remote cmd acc
      )
      Map.empty
      (lines out)
  where
    parseLsofLine line
      | "COMMAND" `isPrefixOf` line = Nothing
      | otherwise =
          case words line of
            cmd : _ ->
              case [w | w <- words line, "->" `isInfixOf` w] of
                (ep : _) ->
                  case break (== '>') ep of
                    (local, '>' : remote) -> Just (cmd, local, remote)
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing

readProcessMapLinux :: IO (Map String String)
readProcessMapLinux = do
  out <- readProcess "ss" ["-H", "-t", "-n", "-p"] ""
  pure $
    Map.fromList
      [ (key, cmd)
      | line <- lines out
      , Just (key, cmd) <- [parseSsLine line]
      ]
  where
    parseSsLine line =
      case words line of
        _ : local : remote : _ ->
          case span (/= ',') (dropWhile (/= '"') line) of
            (_, rest) | "users:" `isPrefixOf` dropWhile isSpace rest ->
              let cmd = takeWhile (/= '"') (dropWhile (/= '"') (dropWhile (/= '(') line))
               in if null cmd then Nothing else Just (local ++ "->" ++ remote, cmd)
            _ -> Nothing
        _ -> Nothing

parseFlowLines :: String -> [Flow]
parseFlowLines out =
  [ flow
  | line <- lines out
  , Just flow <- [parseFlowLine line]
  , flowHost flow /= "127.0.0.1"
  , flowHost flow /= "::1"
  , not (isPrefixOf "127." (flowHost flow))
  ]

parseFlowLine :: String -> Maybe Flow
parseFlowLine line
  | "tcp" `isPrefixOf` line =
      case words line of
        _ : recvQ : sendQ : local : remote : state : rest ->
          do
            (host, port) <- hostPortFromEndpoint remote
            localPort <- localPortFromEndpoint local
            let (rx, tx) = parseByteCounts rest
             in Just
                  Flow
                    { flowLocal = local
                    , flowForeign = remote
                    , flowHost = host
                    , flowPort = port
                    , flowLocalPort = localPort
                    , flowState = state
                    , flowRecvQ = readField recvQ
                    , flowSendQ = readField sendQ
                    , flowRxBytes = rx
                    , flowTxBytes = tx
                    , flowProcess = Nothing
                    }
        _ -> Nothing
  | otherwise = Nothing

parseByteCounts :: [String] -> (Maybe Int64, Maybe Int64)
parseByteCounts (rx : tx : _) =
  ( readMaybeInt64 rx
  , readMaybeInt64 tx
  )
parseByteCounts _ = (Nothing, Nothing)

localPortFromEndpoint :: String -> Maybe Int
localPortFromEndpoint s = hostPortFromEndpoint s >>= \(_, port) -> Just port

flowHostPortFromEndpoint :: String -> Maybe (String, Int)
flowHostPortFromEndpoint = hostPortFromEndpoint

readField :: (Read a, Num a) => String -> a
readField s
  | s == "-" = 0
  | otherwise =
      case reads (dropWhile isSpace s) of
        [(n, _)] -> n
        _ -> 0

readMaybeInt64 :: String -> Maybe Int64
readMaybeInt64 s =
  case reads (dropWhile isSpace s) of
    [(n, _)] -> Just n
    _ -> Nothing

computeHostEmits ::
  Double ->
  String ->
  Int ->
  [Flow] ->
  Maybe (Map String (Int64, Int64)) ->
  ([HostEmit], Map String (Int64, Int64))
computeHostEmits interval state limit flows prevTotals =
  let filtered = filter ((== state) . flowState) flows
      totals = foldl accumulateTotals Map.empty filtered
      emits =
        [ hostEmit interval prevTotals host rx tx (flowsToHost host filtered)
        | (host, (rx, tx)) <- Map.toList totals
        ]
      ranked = take limit (sortBy (comparing (Down . emitTxRate)) emits)
   in (ranked, totals)
  where
    flowsToHost host = filter ((== host) . flowHost)

    accumulateTotals acc f =
      let key = flowHost f
          (rx, tx) = bytesForFlow f
       in Map.insertWith addPair key (rx, tx) acc

    hostEmit interval prev host rx tx hostFlows =
      let (txRate, rxRate) = deltaRates interval prev host rx tx
          ports =
            take 16 $
              intercalate
                ","
                (nubOrd [serviceLabel (flowPort f) | f <- hostFlows])
       in HostEmit
            { emitHost = host
            , emitTxRate = txRate
            , emitRxRate = rxRate
            , emitConns = length hostFlows
            , emitProcess = dominantProcess hostFlows
            , emitPorts = ports
            , emitSendQ = sum (map flowSendQ hostFlows)
            }

    dominantProcess hostFlows =
      case [p | f <- hostFlows, Just p <- [flowProcess f], p /= "?"] of
        (p : _) -> p
        _ -> "?"

    addPair (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

    serviceLabel p = serviceName p

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = Map.keys (Map.fromList [(x, ()) | x <- xs])

bytesForFlow :: Flow -> (Int64, Int64)
bytesForFlow f =
  ( maybe 0 id (flowRxBytes f)
  , maybe 0 id (flowTxBytes f)
  )

deltaRates ::
  Double ->
  Maybe (Map String (Int64, Int64)) ->
  String ->
  Int64 ->
  Int64 ->
  (Double, Double)
deltaRates interval prev key rx tx =
  case prev >>= Map.lookup key of
    Just (prevRx, prevTx) ->
      ( fromIntegral (max 0 (tx - prevTx)) / interval
      , fromIntegral (max 0 (rx - prevRx)) / interval
      )
    Nothing -> (0, 0)

renderFlowPanel :: Int -> Bool -> Double -> [HostEmit] -> [Flow] -> [String]
renderFlowPanel tick colorOn interval emits activeFlows =
  let header =
        [ ""
        , styleHeader colorOn $
            intercalate
              "  "
              [ padR 22 "REMOTE HOST"
              , padL 10 "EMIT/s"
              , padL 10 "RECV/s"
              , padL 4 "CNX"
              , padR 10 "PROCESS"
              , padR 8 "PORTS"
              ]
        ]
      hostRows =
        if null emits
          then ["  (no active " ++ show interval ++ "s traffic to remote hosts yet)"]
          else
            [ "  "
                ++ styleHost colorOn (padR 22 (emitHost e))
                ++ styleEmit colorOn (padL 10 (formatRate (emitTxRate e)))
                ++ padL 10 (formatRate (emitRxRate e))
                ++ padL 4 (show (emitConns e))
                ++ "  "
                ++ padR 10 (take 10 (emitProcess e))
                ++ padR 8 (take 8 (emitPorts e))
                ++ "  "
                ++ blockBar tick (emitTxRate e) (peakTx emits) 10
            | e <- emits
            ]
      flowHeader =
        [ ""
        , styleHeader colorOn "  Active flows (your machine -> remote host):"
        ]
      flowRows =
        take 12 $
          [ "  "
              ++ padR 14 (":" ++ show (flowLocalPort f))
              ++ "  ->  "
              ++ padR 24 (flowHost f ++ ":" ++ serviceName (flowPort f))
              ++ queueLabel f
              ++ "  "
              ++ padR 12 (maybe "?" id (flowProcess f))
              ++ "  "
              ++ take 12 (flowState f)
          | f <- sortBy (comparing (Down . flowSendQ)) activeFlows
          , flowState f == "ESTABLISHED"
          ]
      legend =
        [ ""
        , "  "
            ++ spinnerAt tick
            ++ " EMIT/s = bytes sent to host  |  RECV/s = bytes received  |  "
            ++ show (length activeFlows)
            ++ " tracked flows"
        , ""
        ]
      peakTx es = maximum (1 : map emitTxRate es)
      queueLabel f
        | flowSendQ f > 0 = padL 8 ("↑" ++ show (flowSendQ f) ++ "B q")
        | flowRecvQ f > 0 = padL 8 ("↓" ++ show (flowRecvQ f) ++ "B q")
        | otherwise = padL 8 "idle"
   in header ++ hostRows ++ flowHeader ++ flowRows ++ legend

renderFlowExtras :: [String] -> [String] -> [String] -> [String]
renderFlowExtras alertLines appLines dnsLines =
  (if null alertLines then [] else alertLines)
    ++ (if null dnsLines then [] else ["" , "  DNS names:"] ++ dnsLines)
    ++ (if null appLines then [] else ["" , "  Per-app emitters:"] ++ appLines)

styleHeader :: Bool -> String -> String
styleHeader False s = s
styleHeader True s = "\ESC[1m\ESC[96m" ++ s ++ "\ESC[0m"

styleHost :: Bool -> String -> String
styleHost False s = s
styleHost True s = "\ESC[97m" ++ s ++ "\ESC[0m"

styleEmit :: Bool -> String -> String
styleEmit False s = s
styleEmit True s = "\ESC[92m" ++ s ++ "\ESC[0m"

intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs
