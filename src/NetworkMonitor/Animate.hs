module NetworkMonitor.Animate
  ( spinnerAt
  , pulseLive
  , blockBar
  , sparkline
  , oscilloscope
  , pushSample
  , historyWidth
  , oscilloscopeWidth
  , packetTicker
  , detectTrafficStorm
  , stormFlashLabel
  ) where

import Data.Int (Int64)
import Data.List (genericTake)

historyWidth :: Int
historyWidth = 16

oscilloscopeWidth :: Int
oscilloscopeWidth = 20

packetTickerWidth :: Int
packetTickerWidth = 54

spinnerFrames :: [String]
spinnerFrames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

spinnerAt :: Int -> String
spinnerAt tick =
  spinnerFrames !! (tick `mod` length spinnerFrames)

pulseFrames :: [String]
pulseFrames = ["●", "◉", "○", "◉"]

pulseLive :: Int -> String
pulseLive tick =
  pulseFrames !! (tick `mod` length pulseFrames)

blockLevels :: [Char]
blockLevels = " ▁▂▃▄▅▆▇█"

blockBar :: Int -> Double -> Double -> Int -> String
blockBar tick rate maxRate width =
  let pct =
        if maxRate <= 0
          then 0
          else min 1 (rate / maxRate)
      total = pct * fromIntegral width
      full = truncate total :: Int
      partial =
        round ((total - fromIntegral full) * fromIntegral (length blockLevels - 1))
      shimmer = 1 + (tick `mod` 2)
   in concat
        [ if i < full
            then "█"
            else
              if i == full && partial > 0
                then
                  [ blockLevels !!
                      min (length blockLevels - 1) (max 0 (partial + shimmer - 1))
                  ]
                else "-"
        | i <- [0 .. width - 1]
        ]

pushSample :: Int -> Double -> [Double] -> [Double]
pushSample maxLen val hist = genericTake maxLen (val : hist)

sparkline :: [Double] -> Int -> String
sparkline hist width =
  let samples = genericTake width hist
      peak = maximum (1.0 : samples)
   in [ sampleChar peak v
      | v <- reverse samples
      ] ++ replicate (width - length samples) '▁'
  where
    sampleChar peak v =
      let pct = min 1 (v / peak)
          idx = min (length blockLevels - 1) (round (pct * fromIntegral (length blockLevels - 1)))
       in blockLevels !! idx

oscilloscope :: [Double] -> Int -> String
oscilloscope = sparkline

packetTicker :: Int -> Int64 -> Int64 -> String
packetTicker tick inPkts outPkts =
  let core =
        " IN "
          ++ show inPkts
          ++ " pkts/s  OUT "
          ++ show outPkts
          ++ " pkts/s  TOTAL "
          ++ show (inPkts + outPkts)
          ++ "  | "
      tape = concat (replicate 4 core)
      offset = tick `mod` max 1 (length core)
   in take packetTickerWidth (drop offset tape)

detectTrafficStorm :: Double -> Double -> Bool
detectTrafficStorm totalRate peakRate =
  totalRate >= 524288
    || (peakRate >= 262144 && totalRate / peakRate >= 0.9)

stormFlashLabel :: Int -> String
stormFlashLabel tick =
  if tick `mod` 2 == 0
    then "⚡ TRAFFIC STORM ⚡"
    else "⚡ HIGH BANDWIDTH ⚡"
