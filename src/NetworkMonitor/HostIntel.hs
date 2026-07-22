module NetworkMonitor.HostIntel
  ( GeoInfo (..)
  , lookupGeo
  , formatGeoShort
  , isPrivateIp
  ) where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

data GeoInfo = GeoInfo
  { geoCountry :: !String
  , geoRegion :: !String
  , geoCity :: !String
  , geoIsp :: !String
  , geoAsn :: !String
  }
  deriving (Eq, Show)

lookupGeo :: String -> IO (Maybe GeoInfo)
lookupGeo ip
  | isPrivateIp ip = pure (Just (GeoInfo "LAN" "" "" "local network" ""))
  | otherwise = do
      (code, out, _) <-
        readProcessWithExitCode
          "curl"
          ["-s", "--max-time", "2", "http://ip-api.com/json/" ++ ip ++ "?fields=status,country,regionName,city,isp,as"]
          ""
      pure $
        if code == ExitSuccess && "\"success\"" `isInfixOf` out
          then
            Just
              GeoInfo
                { geoCountry = jsonField "country" out
                , geoRegion = jsonField "regionName" out
                , geoCity = jsonField "city" out
                , geoIsp = jsonField "isp" out
                , geoAsn = take 24 (jsonField "as" out)
                }
          else Nothing

formatGeoShort :: GeoInfo -> String
formatGeoShort g =
  let loc = filter (not . null) [geoCity g, geoRegion g, geoCountry g]
   in case loc of
        [] -> take 28 (geoIsp g)
        xs -> take 28 (unwords xs ++ " — " ++ geoIsp g)

isPrivateIp :: String -> Bool
isPrivateIp ip
  | ip == "127.0.0.1" = True
  | "10." `isPrefixOf` ip = True
  | "192.168." `isPrefixOf` ip = True
  | "172." `isPrefixOf` ip =
      case reads (takeWhile isDigit (drop 4 ip)) of
        [(v, _)] -> v >= 16 && v <= 31
        _ -> False
  | otherwise = False

jsonField :: String -> String -> String
jsonField key blob =
  let needle = "\"" ++ key ++ "\":\""
   in case dropPrefix needle blob of
        Nothing -> ""
        Just rest -> takeWhile (/= '"') rest

dropPrefix :: String -> String -> Maybe String
dropPrefix prefix s =
  if prefix `isPrefixOf` s
    then Just (drop (length prefix) s)
    else
      if null s
        then Nothing
        else dropPrefix prefix (tail s)
