module NetworkMonitor.Intel
  ( IntelSnapshot (..)
  , gatherIntel
  , renderIntelLines
  ) where

import Data.List (isInfixOf, isPrefixOf)
import NetworkMonitor.Probe (defaultGateway)
import System.Info (os)
import System.Process (readProcess)

data IntelSnapshot = IntelSnapshot
  { intelPublicIp :: !String
  , intelGateway :: !String
  , intelWifi :: !String
  , intelVpn :: !String
  , intelPlatform :: !String
  }
  deriving (Eq, Show)

gatherIntel :: IO IntelSnapshot
gatherIntel = do
  gw <- defaultGateway
  pub <- readPublicIp
  wifi <- readWifiInfo
  vpn <- readVpnStatus
  pure
    IntelSnapshot
      { intelPublicIp = pub
      , intelGateway = maybe "unknown" id gw
      , intelWifi = wifi
      , intelVpn = vpn
      , intelPlatform = os
      }

renderIntelLines :: IntelSnapshot -> [String]
renderIntelLines s =
  [ ""
  , "  Public IP   : " ++ intelPublicIp s
  , "  Gateway     : " ++ intelGateway s
  , "  Wi-Fi       : " ++ intelWifi s
  , "  VPN/Tunnel  : " ++ intelVpn s
  , "  Platform    : " ++ intelPlatform s
  , ""
  ]

readPublicIp :: IO String
readPublicIp = do
  out <- readProcess "curl" ["-s", "--max-time", "3", "https://ifconfig.me/ip"] ""
  let ip = takeWhile (`notElem` ['\n', '\r', ' ']) out
   in pure (if null ip then "unavailable" else ip)

readWifiInfo :: IO String
readWifiInfo =
  case os of
    "darwin" -> readDarwinWifi
    _ -> pure "n/a on this platform"

readDarwinWifi :: IO String
readDarwinWifi = do
  out <-
    readProcess
      "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport"
      ["-I"]
      ""
  let ssid = fieldVal "SSID" out
      rssi = fieldVal "agrCtlRSSI" out
   in pure $
        case ssid of
          Just s -> s ++ maybe "" (\r -> " (" ++ r ++ " dBm)") rssi
          Nothing -> "not connected / unavailable"
  where
    fieldVal key blob =
      case [last (words l) | l <- lines blob, key `isPrefixOf` l, length (words l) >= 2] of
        (v : _) -> Just v
        _ -> Nothing

readVpnStatus :: IO String
readVpnStatus = do
  out <- readProcess "netstat" ["-rn"] ""
  let tunnels = length (filter (("utun" `isInfixOf`) . lineIface) (lines out))
   in pure $
        if tunnels > 0
          then "active (" ++ show tunnels ++ " tunnel iface(s))"
          else "not detected"
  where
    lineIface l = case words l of
      (_ : iface : _) -> iface
      _ -> l
