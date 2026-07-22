module NetworkMonitor.Session
  ( Session (..)
  , defaultSession
  , loadSession
  , saveSession
  , sessionConfigPath
  , showInterface
  , parseInterfaceInput
  , parseCommaList
  , showCommaList
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  )
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile)

data Session = Session
  { sessionInterface :: ![String]
  , sessionInterval :: !Double
  , sessionCount :: !Int
  , sessionState :: !String
  , sessionLimit :: !Int
  , sessionFavorites :: ![String]
  , sessionBlocklist :: ![String]
  , sessionEmitAlert :: !Double
  , sessionLogging :: !Bool
  , sessionRouterHost :: !String
  , sessionSnmpCommunity :: !String
  , sessionSnmpWanIf :: !String
  , sessionFlowResolveDns :: !Bool
  , sessionFlowShowApps :: !Bool
  }
  deriving (Eq, Show)

defaultSession :: Session
defaultSession =
  Session
    { sessionInterface = ["en0"]
    , sessionInterval = 1.0
    , sessionCount = 0
    , sessionState = "ESTABLISHED"
    , sessionLimit = 50
    , sessionFavorites = ["8.8.8.8", "1.1.1.1"]
    , sessionBlocklist = []
    , sessionEmitAlert = 524288
    , sessionLogging = True
    , sessionRouterHost = ""
    , sessionSnmpCommunity = "public"
    , sessionSnmpWanIf = ""
    , sessionFlowResolveDns = True
    , sessionFlowShowApps = True
    }

sessionConfigPath :: IO FilePath
sessionConfigPath = do
  home <- getHomeDirectory
  pure (home ++ "/.config/new-tower/session.conf")

sessionConfigDir :: FilePath
sessionConfigDir = ".config/new-tower"

loadSession :: IO Session
loadSession = do
  path <- sessionConfigPath
  exists <- doesFileExist path
  if exists
    then parseConfig <$> readFile path
    else do
      saveSession defaultSession
      pure defaultSession

saveSession :: Session -> IO ()
saveSession s = do
  home <- getHomeDirectory
  let dir = home ++ "/" ++ sessionConfigDir
  path <- sessionConfigPath
  createDirectoryIfMissing True dir
  h <- openFile path WriteMode
  mapM_
    (hPutStrLn h)
    [ "# NT Sentinel session defaults"
    , "interface=" ++ showInterface (sessionInterface s)
    , "interval=" ++ show (sessionInterval s)
    , "count=" ++ show (sessionCount s)
    , "state=" ++ sessionState s
    , "limit=" ++ show (sessionLimit s)
    , "favorites=" ++ showCommaList (sessionFavorites s)
    , "blocklist=" ++ showCommaList (sessionBlocklist s)
    , "emit_alert=" ++ show (sessionEmitAlert s)
    , "logging=" ++ if sessionLogging s then "1" else "0"
    , "router_host=" ++ sessionRouterHost s
    , "snmp_community=" ++ sessionSnmpCommunity s
    , "snmp_wan_if=" ++ sessionSnmpWanIf s
    , "flow_resolve_dns=" ++ if sessionFlowResolveDns s then "1" else "0"
    , "flow_show_apps=" ++ if sessionFlowShowApps s then "1" else "0"
    ]
  hClose h

showInterface :: [String] -> String
showInterface [] = ""
showInterface xs = intercalate "," xs

showCommaList :: [String] -> String
showCommaList = intercalate ","

parseInterfaceInput :: String -> [String]
parseInterfaceInput = parseCommaList

parseCommaList :: String -> [String]
parseCommaList s =
  let trimmed = dropWhileEnd isSpace (dropWhile isSpace s)
   in if null trimmed
        then []
        else map (dropWhileEnd isSpace . dropWhile isSpace) (splitOnComma trimmed)
  where
    splitOnComma x = case break (== ',') x of
      (a, ',' : rest) -> a : splitOnComma rest
      (a, _) -> [a]

parseConfig :: String -> Session
parseConfig content =
  foldl applyLine defaultSession (lines content)
  where
    applyLine s line
      | null line || head line == '#' = s
      | otherwise =
          case break (== '=') (dropWhile isSpace line) of
            (key, '=' : val) ->
              let k = map toLower (dropWhile isSpace key)
                  v = dropWhile isSpace val
               in case k of
                    "interface" -> s {sessionInterface = parseInterfaceInput v}
                    "interval" -> s {sessionInterval = readField v (sessionInterval s)}
                    "count" -> s {sessionCount = readField v (sessionCount s)}
                    "state" -> s {sessionState = v}
                    "limit" -> s {sessionLimit = readField v (sessionLimit s)}
                    "favorites" -> s {sessionFavorites = if null v then sessionFavorites s else parseCommaList v}
                    "blocklist" -> s {sessionBlocklist = parseCommaList v}
                    "emit_alert" -> s {sessionEmitAlert = readField v (sessionEmitAlert s)}
                    "logging" -> s {sessionLogging = v == "1" || v == "true" || v == "yes"}
                    "router_host" -> s {sessionRouterHost = v}
                    "snmp_community" -> s {sessionSnmpCommunity = if null v then sessionSnmpCommunity s else v}
                    "snmp_wan_if" -> s {sessionSnmpWanIf = v}
                    "flow_resolve_dns" -> s {sessionFlowResolveDns = v == "1" || v == "true" || v == "yes"}
                    "flow_show_apps" -> s {sessionFlowShowApps = v == "1" || v == "true" || v == "yes"}
                    _ -> s
            _ -> s

    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

    readField :: Read a => String -> a -> a
    readField txt fallback =
      case reads (dropWhile isSpace txt) of
        [(n, _)] -> n
        _ -> fallback
