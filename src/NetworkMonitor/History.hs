module NetworkMonitor.History
  ( recordMetric
  , readRecentEmitSamples
  , renderHistorySpark
  ) where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Time.Clock (getCurrentTime)
import NetworkMonitor.Format (sparkBar)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.IO (IOMode (AppendMode, ReadMode), hClose, hGetContents, hPutStrLn, openFile)

recordMetric :: Double -> Int -> Maybe Double -> IO ()
recordMetric emitRate connCount mPingMs = do
  path <- metricsPath
  home <- getHomeDirectory
  createDirectoryIfMissing True (home ++ "/.config/new-tower/history")
  now <- getCurrentTime
  h <- openFile path AppendMode
  hPutStrLn
    h
    ( show now
        ++ ","
        ++ show (round emitRate)
        ++ ","
        ++ show connCount
        ++ ","
        ++ maybe "" (show . round) mPingMs
    )
  hClose h

readRecentEmitSamples :: Int -> IO [Double]
readRecentEmitSamples n = do
  path <- metricsPath
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      h <- openFile path ReadMode
      body <- hGetContents h
      hClose h
      let vals =
            [ v
            | line <- lines body
            , not (null line)
            , line /= "timestamp,emit_bps,connections,gw_ping_ms"
            , Just v <- [parseEmit line]
            ]
      pure (take n (reverse vals))

renderHistorySpark :: [Double] -> String
renderHistorySpark samples =
  let peak = maximum (1 : samples)
      chunk = take 12 (reverse samples)
   in if null chunk
        then "------------"
        else concat [sparkBar v peak 1 | v <- chunk]

metricsPath :: IO FilePath
metricsPath = do
  home <- getHomeDirectory
  pure (home ++ "/.config/new-tower/history/metrics.csv")

parseEmit :: String -> Maybe Double
parseEmit line =
  case splitCommas line of
    (_ : e : _) ->
      case reads e of
        [(n, _)] -> Just n
        _ -> Nothing
    _ -> Nothing

splitCommas :: String -> [String]
splitCommas s = case break (== ',') s of
  (a, ',' : rest) -> a : splitCommas rest
  (a, _) -> [a]
