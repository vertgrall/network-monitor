module NetworkMonitor.LanMap
  ( LanDevice (..)
  , readLanDevices
  , renderLanMapLines
  ) where

import Data.Char (isSpace)
import Data.List (isInfixOf, sortBy)
import Data.Ord (comparing)
import NetworkMonitor.Format (padR)
import System.Info (os)
import System.Process (readProcess)

data LanDevice = LanDevice
  { lanIp :: !String
  , lanMac :: !String
  , lanHint :: !String
  }
  deriving (Eq, Show)

readLanDevices :: IO [LanDevice]
readLanDevices =
  case os of
    "darwin" -> readDarwinArp
    "linux" -> readLinuxNeigh
    _ -> pure []

readDarwinArp :: IO [LanDevice]
readDarwinArp = do
  out <- readProcess "arp" ["-a"] ""
  pure $
    sortBy (comparing lanIp) $
      mapMaybe parseArpLine (lines out)

readLinuxNeigh :: IO [LanDevice]
readLinuxNeigh = do
  out <- readProcess "ip" ["neigh"] ""
  pure $
    sortBy (comparing lanIp) $
      mapMaybe parseIpNeigh (lines out)

parseArpLine :: String -> Maybe LanDevice
parseArpLine line
  | " (" `isInfixOf` line =
      case words line of
        name : ip : _ ->
          let cleanIp = takeWhile (/= ')') ip
              mac = takeMac line
           in Just LanDevice {lanIp = cleanIp, lanMac = mac, lanHint = take 20 name}
        _ -> Nothing
  | otherwise = Nothing

parseIpNeigh :: String -> Maybe LanDevice
parseIpNeigh line =
  case words line of
    (ip : _ : _ : mac : _) ->
      Just LanDevice {lanIp = ip, lanMac = mac, lanHint = ""}
    _ -> Nothing

takeMac :: String -> String
takeMac line =
  case words line of
    ws -> case dropWhile (not . looksMac) ws of
      (m : _) -> m
      _ -> "?"
  where
    looksMac w = length w == 17 && ':' `elem` w

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y -> y : mapMaybe f xs

renderLanMapLines :: [LanDevice] -> [String]
renderLanMapLines devs =
  [ ""
  , "  IP                 MAC                NAME/HINT"
  , "  " ++ replicate 52 '-'
  ]
    ++ [ "  " ++ padR 18 (lanIp d) ++ padR 19 (lanMac d) ++ padR 16 (lanHint d) | d <- take 30 devs]
    ++ [""]
