module NetworkMonitor.Router
  ( WanIface (..)
  , discoverWanInterface
  , readWanOctets
  , wanRates
  , renderRouterPanel
  , snmpAvailable
  , testSnmpConnection
  ) where

import Data.Char (isDigit, isSpace, toLower)
import Control.Applicative ((<|>))
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import NetworkMonitor.Animate (blockBar, spinnerAt)
import NetworkMonitor.Format (formatBytes, formatRate, padL, padR)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

data WanIface = WanIface
  { wanIndex :: !Int
  , wanDescr :: !String
  }
  deriving (Eq, Show)

ifDescrOid, ifInOctetsOid, ifOutOctetsOid :: String
ifDescrOid = "1.3.6.1.2.1.2.2.1.2"
ifInOctetsOid = "1.3.6.1.2.1.2.2.1.10"
ifOutOctetsOid = "1.3.6.1.2.1.2.2.1.16"

snmpAvailable :: IO Bool
snmpAvailable = do
  (code, _, _) <- readProcessWithExitCode "snmpget" ["-V"] ""
  pure (code == ExitSuccess)

testSnmpConnection :: String -> String -> Maybe String -> IO (Either String String)
testSnmpConnection host community hint = do
  ok <- snmpAvailable
  if not ok
    then pure (Left "SNMP tools not found (install net-snmp).")
    else do
      result <- discoverWanInterface host community hint
      pure $
        case result of
          Left err -> Left err
          Right wan ->
            Right
              ( "SNMP OK at "
                  ++ host
                  ++ " — WAN interface "
                  ++ wanDescr wan
                  ++ " (#"
                  ++ show (wanIndex wan)
                  ++ ")"
              )

discoverWanInterface :: String -> String -> Maybe String -> IO (Either String WanIface)
discoverWanInterface host community hint = do
  rows <- snmpWalk host community ifDescrOid
  let ifaces = mapMaybe parseIfDescr rows
  pure $
    case ifaces of
      [] -> Left ("No SNMP interfaces found at " ++ host ++ " (is SNMP enabled?)")
      _ ->
        case pickWan ifaces hint of
          Nothing ->
            Left
              ( "Could not identify WAN interface. Set snmp_wan_if in session.conf "
                  ++ "(e.g. wan, ppp, eth0). Found: "
                  ++ unwords (map wanDescr ifaces)
              )
          Just wan -> Right wan

readWanOctets :: String -> String -> WanIface -> IO (Maybe (Integer, Integer))
readWanOctets host community wan = do
  inVal <- snmpGet host community (ifInOctetsOid ++ "." ++ show (wanIndex wan))
  outVal <- snmpGet host community (ifOutOctetsOid ++ "." ++ show (wanIndex wan))
  pure ((,) <$> inVal <*> outVal)

wanRates :: Maybe (Integer, Integer) -> Maybe (Integer, Integer) -> Double -> (Double, Double)
wanRates Nothing _ _ = (0, 0)
wanRates (Just _) Nothing _ = (0, 0)
wanRates (Just (prevIn, prevOut)) (Just (curIn, curOut)) interval =
  ( delta curIn prevIn / interval
  , delta curOut prevOut / interval
  )
  where
    delta cur prev = fromIntegral (max 0 (cur - prev))

renderRouterPanel ::
  Int ->
  Bool ->
  String ->
  WanIface ->
  Double ->
  Double ->
  Integer ->
  Integer ->
  Double ->
  Double ->
  [String]
renderRouterPanel tick colorOn host wan downRate upRate totalIn totalOut peakDown peakUp =
  [ ""
  , styleHeader colorOn $
      padR 18 "ROUTER"
      ++ padL 12 "IN (to router)"
      ++ padL 12 "OUT (from router)"
  , "  "
      ++ padR 18 (host ++ " / " ++ wanDescr wan)
      ++ styleRate colorOn (padL 12 (formatRate downRate))
      ++ styleRate colorOn (padL 12 (formatRate upRate))
      ++ "  "
      ++ blockBar tick downRate peakDown 8
      ++ "  "
      ++ blockBar tick upRate peakUp 8
  , ""
  , "  Lifetime counters on WAN (" ++ wanDescr wan ++ "):"
  , "    Total IN  : " ++ formatBytes (fromIntegral totalIn :: Int64)
  , "    Total OUT : " ++ formatBytes (fromIntegral totalOut :: Int64)
  , ""
  , "  "
      ++ spinnerAt tick
      ++ " IN = internet -> router (download to your network)  |  "
      ++ "OUT = router -> internet (upload from your network)"
  , "  Polls router via SNMP (all devices combined, not just this Mac)."
  , ""
  ]
  where
    styleHeader False s = s
    styleHeader True s = "\ESC[1m\ESC[96m" ++ s ++ "\ESC[0m"

    styleRate False s = s
    styleRate True s = "\ESC[92m" ++ s ++ "\ESC[0m"

snmpWalk :: String -> String -> String -> IO [String]
snmpWalk host community oid = do
  (code, out, err) <-
    readProcessWithExitCode
      "snmpwalk"
      ["-v2c", "-c", community, "-On", "-t", "2", host, oid]
      ""
  pure $
    if code == ExitSuccess
      then lines out
      else lines (out ++ err)

snmpGet :: String -> String -> String -> IO (Maybe Integer)
snmpGet host community oid = do
  (code, out, _) <-
    readProcessWithExitCode
      "snmpget"
      ["-v2c", "-c", community, "-On", "-t", "2", host, oid]
      ""
  pure $
    if code == ExitSuccess
      then parseCounter out
      else Nothing

parseIfDescr :: String -> Maybe WanIface
parseIfDescr line = do
  (idxStr, val) <- parseOidSuffix ifDescrOid line
  idx <- readMaybe idxStr
  pure WanIface {wanIndex = idx, wanDescr = val}

parseOidSuffix :: String -> String -> Maybe (String, String)
parseOidSuffix base line =
  let prefix = "." ++ base ++ "."
   in if prefix `isPrefixOf` line
        then case break (== ' ') line of
          (oidPart, rest) ->
            let idx = drop (length prefix) oidPart
             in if null idx
                  then Nothing
                  else Just (idx, parseSnmpValue rest)
        else Nothing

parseSnmpValue :: String -> String
parseSnmpValue rest =
  case break (== ':') (dropWhile isSpace rest) of
    (_, ':' : val) -> unquote (dropWhile isSpace val)
    _ -> unquote (dropWhile isSpace rest)

unquote :: String -> String
unquote s =
  case s of
    '"' : rest ->
      case break (== '"') rest of
        (inner, _) -> inner
        _ -> s
    _ -> s

parseCounter :: String -> Maybe Integer
parseCounter line =
  case break (== ':') line of
    (_, ':' : rest) -> readInteger (dropWhile isSpace rest)
    _ -> Nothing

readInteger :: String -> Maybe Integer
readInteger s =
  case reads (takeWhile (\c -> isDigit c || c == '-') (dropWhile isSpace s)) of
    [(n, _)] -> Just n
    _ -> Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(n, _)] -> Just n
  _ -> Nothing

pickWan :: [WanIface] -> Maybe String -> Maybe WanIface
pickWan ifaces hint =
  ( case hint of
      Just h | not (null h) ->
        findMatch (contains (map toLower h))
          <|> findMatch (prefixMatch (map toLower h))
      _ -> Nothing
  )
    <|> findMatch preferred
    <|> listToMaybe (reverse ifaces)
  where
    contains needle iface = needle `isInfixOf` map toLower (wanDescr iface)
    prefixMatch needle iface = needle `isPrefixOf` map toLower (wanDescr iface)

    preferred iface =
      let d = map toLower (wanDescr iface)
       in any (`isInfixOf` d) ["wan", "internet", "ppp", "outside", "uplink", "dsl", "cable", "broadband"]
            && not (any (`isInfixOf` d) ["loop", "lan", "wifi", "wlan", "bridge", "internal"])

    findMatch p = listToMaybe [i | i <- ifaces, p i]
