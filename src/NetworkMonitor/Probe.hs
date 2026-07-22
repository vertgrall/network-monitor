module NetworkMonitor.Probe
  ( PingResult (..)
  , pingHost
  , portCheck
  , dnsLookup
  , defaultGateway
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf)
import System.Info (os)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

data PingResult = PingResult
  { pingTarget :: !String
  , pingTransmitted :: !Int
  , pingReceived :: !Int
  , pingLossPct :: !Double
  , pingMinMs :: !Double
  , pingAvgMs :: !Double
  , pingMaxMs :: !Double
  }
  deriving (Eq, Show)

pingHost :: String -> Int -> IO (Either String PingResult)
pingHost host count = do
  let args =
        case os of
          "linux" -> ["-c", show count, "-W", "2", host]
          _ -> ["-c", show count, "-W", "2000", host]
  (code, out, err) <- readProcessWithExitCode "ping" args ""
  let body = out ++ err
  case code of
    ExitSuccess ->
      case parsePing body host of
        Just r -> pure (Right r)
        Nothing -> pure (Left ("Could not parse ping output for " ++ host))
    _ ->
      case parsePing body host of
        Just r -> pure (Right r)
        Nothing -> pure (Left ("Ping failed: " ++ take 120 (filter (/= '\n') body)))

parsePing :: String -> String -> Maybe PingResult
parsePing out host =
  let ls = lines out
      txRx = findTxRx ls
      stats = findStats ls
   in case (txRx, stats) of
        (Just (tx, rx), Just (mn, avg, mx)) ->
          let loss =
                if tx > 0
                  then fromIntegral (tx - rx) / fromIntegral tx * 100
                  else 100
           in
            Just
              PingResult
                { pingTarget = host
                , pingTransmitted = tx
                , pingReceived = rx
                , pingLossPct = loss
                , pingMinMs = mn
                , pingAvgMs = avg
                , pingMaxMs = mx
                }
        _ -> Nothing
  where
    findTxRx ls =
      case [l | l <- ls, "packets transmitted" `isInfixOf` l] of
        (l : _) ->
          let ws = words l
           in case (readMaybe (head ws), safeIndex 3 ws >>= readMaybe) of
                (Just tx, Just rx) -> Just (tx, rx)
                _ -> Nothing
        _ -> Nothing

    safeIndex i xs
      | i < length xs = Just (xs !! i)
      | otherwise = Nothing

    findStats ls =
      case [l | l <- ls, "min/avg/max" `isInfixOf` l] of
        (l : _) -> parseMinAvgMax l
        _ -> Nothing

    parseMinAvgMax l =
      let val =
            case break (== '=') l of
              (_, '=' : rest) -> takeWhile (`notElem` " ms\n\t") (dropWhile isSpace rest)
              _ -> ""
          nums =
            [ n
            | w <- words (map (\c -> if c == '/' then ' ' else c) val)
            , n <- maybeToList (readMaybe w)
            ]
       in case take 3 nums of
            [a, b, c] -> Just (a, b, c)
            _ -> Nothing

    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    readMaybe :: Read a => String -> Maybe a
    readMaybe s =
      case reads (dropWhile isSpace s) of
        [(n, _)] -> Just n
        _ -> Nothing

portCheck :: String -> Int -> IO (Either String Bool)
portCheck host port = do
  let (cmd, args) =
        case os of
          "linux" -> ("nc", ["-z", "-w", "3", host, show port])
          _ -> ("nc", ["-z", "-G", "3", host, show port])
  (code, _, _) <- readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> pure (Right True)
    _ -> pure (Left ("Port " ++ show port ++ " closed or unreachable on " ++ host))

dnsLookup :: String -> IO (Either String [String])
dnsLookup host = do
  (code, out, err) <- readProcessWithExitCode "host" [host] ""
  let ips =
        [ last (words l)
        | l <- lines out
        , " has address " `isInfixOf` l
        ]
  case code of
    ExitSuccess | not (null ips) -> pure (Right ips)
    _ -> pure (Left (take 120 (filter (/= '\n') (out ++ err))))

defaultGateway :: IO (Maybe String)
defaultGateway =
  case os of
    "linux" -> readGateway ["-n"]
    _ -> readGateway []
  where
    readGateway extra = do
      (code, out, _) <- readProcessWithExitCode "route" (extra ++ ["get", "default"]) ""
      case code of
        ExitSuccess ->
          pure $
            case [w | l <- lines out, "gateway:" `isInfixOf` l, w <- words l, w /= "gateway:"] of
              (g : _) -> Just g
              _ -> Nothing
        _ -> pure Nothing
