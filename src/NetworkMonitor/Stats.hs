module NetworkMonitor.Stats
  ( InterfaceStats (..)
  , Connection (..)
  , readInterfaceStats
  , readConnections
  , diffStats
  , ratesFromDiff
  ) where

import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Info (os)
import System.Process (readProcess)

data InterfaceStats = InterfaceStats
  { ifaceName :: !String
  , ifaceMtu :: !Int
  , inPackets :: !Int64
  , inErrors :: !Int64
  , inBytes :: !Int64
  , outPackets :: !Int64
  , outErrors :: !Int64
  , outBytes :: !Int64
  }
  deriving (Eq, Show)

data Connection = Connection
  { connProto :: !String
  , connRecvQ :: !Int
  , connSendQ :: !Int
  , connLocal :: !String
  , connForeign :: !String
  , connState :: !String
  }
  deriving (Eq, Show)

readInterfaceStats :: IO [InterfaceStats]
readInterfaceStats =
  case os of
    "linux" -> readLinuxProcNetDev
    _ -> readDarwinNetstat

readConnections :: IO [Connection]
readConnections = do
  out <- readProcess "netstat" ["-an", "-p", "tcp"] ""
  pure $ parseNetstatConnections out

readDarwinNetstat :: IO [InterfaceStats]
readDarwinNetstat = do
  out <- readProcess "netstat" ["-ib"] ""
  pure $
    dedupeByName $
      mapMaybe parseNetstatIBLine (lines out)

readLinuxProcNetDev :: IO [InterfaceStats]
readLinuxProcNetDev = do
  out <- readFile "/proc/net/dev"
  pure $ parseProcNetDev out

dedupeByName :: [InterfaceStats] -> [InterfaceStats]
dedupeByName = Map.elems . Map.fromListWith mergeStats . map (\s -> (ifaceName s, s))
  where
    mergeStats a b =
      a
        { inPackets = inPackets a + inPackets b
        , inErrors = inErrors a + inErrors b
        , inBytes = inBytes a + inBytes b
        , outPackets = outPackets a + outPackets b
        , outErrors = outErrors a + outErrors b
        , outBytes = outBytes a + outBytes b
        }

parseNetstatIBLine :: String -> Maybe InterfaceStats
parseNetstatIBLine line
  | null line = Nothing
  | isPrefixOf "Name" line = Nothing
  | otherwise =
      case words line of
        name : mtu : network : _ : ipkts : ierrs : ibytes : opkts : oerrs : obytes : _ ->
          if isPrefixOf "<Link#" network
            then
              Just
                InterfaceStats
                  { ifaceName = name
                  , ifaceMtu = readField mtu
                  , inPackets = readField ipkts
                  , inErrors = readField ierrs
                  , inBytes = readField ibytes
                  , outPackets = readField opkts
                  , outErrors = readField oerrs
                  , outBytes = readField obytes
                  }
            else Nothing
        _ -> Nothing

readField :: (Read a, Num a) => String -> a
readField s
  | s == "-" = 0
  | otherwise =
      case reads (dropWhile isSpace s) of
        [(n, _)] -> n
        _ -> 0

parseProcNetDev :: String -> [InterfaceStats]
parseProcNetDev out =
  mapMaybe parseProcLine (drop 2 (lines out))
  where
    parseProcLine line =
      case break (== ':') (dropWhile isSpace line) of
        (name, ':' : rest) ->
          let fields = words rest
           in if length fields >= 16
                then
                  Just
                    InterfaceStats
                      { ifaceName = name
                      , ifaceMtu = 0
                      , inBytes = readField (fields !! 0)
                      , inPackets = readField (fields !! 1)
                      , inErrors = readField (fields !! 2)
                      , outBytes = readField (fields !! 8)
                      , outPackets = readField (fields !! 9)
                      , outErrors = readField (fields !! 10)
                      }
                else Nothing
        _ -> Nothing

parseNetstatConnections :: String -> [Connection]
parseNetstatConnections out =
  mapMaybe parseLine (lines out)
  where
    parseLine line
      | "tcp" `isPrefixOf` line || "udp" `isPrefixOf` line =
          case words line of
            proto : recvQ : sendQ : local : remote : state : _ ->
              Just
                Connection
                  { connProto = proto
                  , connRecvQ = readField recvQ
                  , connSendQ = readField sendQ
                  , connLocal = local
                  , connForeign = remote
                  , connState = state
                  }
            _ -> Nothing
      | otherwise = Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y -> y : mapMaybe f xs

diffStats :: InterfaceStats -> InterfaceStats -> InterfaceStats
diffStats prev cur =
  cur
    { inPackets = inPackets cur - inPackets prev
    , inErrors = inErrors cur - inErrors prev
    , inBytes = inBytes cur - inBytes prev
    , outPackets = outPackets cur - outPackets prev
    , outErrors = outErrors cur - outErrors prev
    , outBytes = outBytes cur - outBytes prev
    }

ratesFromDiff :: InterfaceStats -> Double -> (Double, Double)
ratesFromDiff diff seconds =
  ( fromIntegral (inBytes diff) / seconds
  , fromIntegral (outBytes diff) / seconds
  )
