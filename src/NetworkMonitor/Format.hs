module NetworkMonitor.Format
  ( formatBytes
  , formatRate
  , padR
  , padL
  , sparkBar
  ) where

import Data.Int (Int64)
import Text.Printf (printf)

formatBytes :: Int64 -> String
formatBytes n
  | n < 0 = "-" ++ formatBytes (negate n)
  | n >= 1024 ^ (3 :: Int) = printf "%.2f GB" (fromIntegral n / (1024 ^ (3 :: Int)) :: Double)
  | n >= 1024 ^ (2 :: Int) = printf "%.2f MB" (fromIntegral n / (1024 ^ (2 :: Int)) :: Double)
  | n >= 1024 = printf "%.2f KB" (fromIntegral n / (1024 :: Double))
  | otherwise = show n ++ " B"

formatRate :: Double -> String
formatRate bps = formatBytes (round bps) ++ "/s"

padR :: Int -> String -> String
padR width s =
  let len = length s
   in if len >= width then s else s ++ replicate (width - len) ' '

padL :: Int -> String -> String
padL width s =
  let len = length s
   in if len >= width then s else replicate (width - len) ' ' ++ s

sparkBar :: Double -> Double -> Int -> String
sparkBar rate maxRate width =
  let pct =
        if maxRate <= 0
          then 0
          else min 1 (rate / maxRate)
      filled = max 0 (min width (round (pct * fromIntegral width)))
   in replicate filled '#' ++ replicate (width - filled) '-'

