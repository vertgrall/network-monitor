module NetworkMonitor.NetView
  ( NetSnapshot (..)
  , gatherNetSnapshot
  , renderNetViewLines
  , updatePingHistory
  , maxRate
  ) where

import Data.Char (isSpace)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import NetworkMonitor.Animate (blockBar, oscilloscope, spinnerAt)
import NetworkMonitor.Format (formatRate, padL, padR)
import NetworkMonitor.Hosts (topRemoteHosts)
import NetworkMonitor.Probe (defaultGateway)
import NetworkMonitor.Stats
import System.Info (os)

data NetSnapshot = NetSnapshot
  { snapInterfaces :: [(String, Double, Double)]
  , snapGateway :: Maybe String
  , snapRemoteHosts :: [(String, Int)]
  , snapTotalConns :: !Int
  , snapPingMs :: [Double]
  }
  deriving (Eq, Show)

canvasWidth :: Int
canvasWidth = 74

canvasHeight :: Int
canvasHeight = 18

gatherNetSnapshot ::
  [String] ->
  String ->
  Int ->
  Double ->
  Maybe (Map String InterfaceStats) ->
  IO NetSnapshot
gatherNetSnapshot ifaces state limit interval prevMap = do
  stats <- readInterfaceStats
  let current = indexByName stats
  gateway <- defaultGateway
  hosts <- topRemoteHosts limit state
  conns <- readConnections
  let filteredStats =
        if null ifaces
          then sortBy (comparing ifaceName) stats
          else filter ((`elem` ifaces) . ifaceName) stats
      ifaceRates =
        [ ( ifaceName s
          , rateDown prevMap current (ifaceName s) interval
          , rateUp prevMap current (ifaceName s) interval
          )
        | s <- take 3 filteredStats
        ]
      connCount = length (filter ((== state) . connState) conns)
  pure
    NetSnapshot
      { snapInterfaces = ifaceRates
      , snapGateway = gateway
      , snapRemoteHosts = take 8 hosts
      , snapTotalConns = connCount
      , snapPingMs = []
      }

renderNetViewLines :: Int -> Bool -> NetSnapshot -> [String]
renderNetViewLines tick colorOn snap =
  let canvas = renderCanvas tick snap
      stats =
        [ "  "
            ++ padR 12 ("Links " ++ show (snapTotalConns snap))
            ++ "  "
            ++ padR 12 ("Hosts " ++ show (length (snapRemoteHosts snap)))
            ++ "  "
            ++ padR 12 ("Platform " ++ os)
        , ""
        ]
      ifaceRow =
        if null (snapInterfaces snap)
          then ["  (no interfaces matched)"]
          else
            [ "  "
                ++ padR 8 name
                ++ " "
                ++ blockBar tick down (maxRate snap) 14
                ++ " "
                ++ padL 10 (formatRate down)
                ++ " up "
                ++ padL 10 (formatRate up)
            | (name, down, up) <- snapInterfaces snap
            ]
      pingRow =
        case snapPingMs snap of
          [] -> []
          hist ->
            [ ""
            , "  "
                ++ label colorOn "Gateway wave "
                ++ oscilloscope hist 24
                ++ "  "
                ++ label colorOn (show (round (head hist)) ++ " ms")
            ]
   in [""]
        ++ map (label colorOn) (canvasToLines canvas)
        ++ [""]
        ++ stats
        ++ [""]
        ++ [label colorOn "  Interface traffic:"]
        ++ ifaceRow
        ++ pingRow
        ++ [""]

renderCanvas :: Int -> NetSnapshot -> Canvas
renderCanvas tick snap =
  let (cx, cy) = (canvasWidth `div` 2, canvasHeight `div` 2 + 1)
      canvas0 = emptyCanvas
      canvas1 =
        foldl
          ( \cv (idx, (host, count)) ->
              let (x, y) = placeOnCircle (cx, cy) 11 idx (length (snapRemoteHosts snap))
                  pulse = pulseNode tick count
                  lineChar = pulseLink tick count
               in drawNode cv (drawLine cv (cx, cy) (x, y) lineChar) x y (shortHost host) count pulse
          )
          canvas0
          (zip [0 ..] (snapRemoteHosts snap))
      canvas2 =
        case snapGateway snap of
          Nothing -> canvas1
          Just gw ->
            let gy = 1
                gx = cx
                gwLabel = "▲ " ++ shortHost gw
                cv = drawLine canvas1 (cx, cy - 2) (gx, gy + 1) (pulseLink tick 2)
             in putText cv (gx - length gwLabel `div` 2) gy gwLabel
      canvas3 = putText canvas2 (cx - 5) cy "◉ NT CORE"
      canvas4 = putText canvas3 1 0 (spinnerAt tick ++ " mapping live links ")
   in canvas4

type Canvas = [String]

emptyCanvas :: Canvas
emptyCanvas = replicate canvasHeight (replicate canvasWidth ' ')

canvasToLines :: Canvas -> [String]
canvasToLines = map (("  " ++) . dropWhileEnd isSpace . take canvasWidth)

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

setPixel :: Canvas -> Int -> Int -> Char -> Canvas
setPixel cv x y c
  | y < 0 || y >= length cv || x < 0 || x >= canvasWidth = cv
  | otherwise =
      let row = cv !! y
          row' = take x row ++ [c] ++ drop (x + 1) row
       in take y cv ++ [row'] ++ drop (y + 1) cv

putText :: Canvas -> Int -> Int -> String -> Canvas
putText cv x y txt =
  foldl (\c (i, ch) -> setPixel c (x + i) y ch) cv (zip [0 ..] txt)

drawLine :: Canvas -> (Int, Int) -> (Int, Int) -> Char -> Canvas
drawLine cv (x0, y0) (x1, y1) c =
  let steps = max 1 (max (abs (x1 - x0)) (abs (y1 - y0)))
   in foldl
        ( \acc i ->
            let x = x0 + (x1 - x0) * i `div` steps
                y = y0 + (y1 - y0) * i `div` steps
             in if (x, y) == (x0, y0) || (x, y) == (x1, y1)
                  then acc
                  else setPixel acc x y c
        )
        cv
        [0 .. steps]

drawNode :: Canvas -> Canvas -> Int -> Int -> String -> Int -> Char -> Canvas
drawNode _base cv x y host count pulse =
  let label = pulse : take 10 host ++ "(" ++ show count ++ ")"
      lx = max 1 (min (canvasWidth - length label - 1) (x - length label `div` 2))
      ly = max 1 (min (canvasHeight - 2) y)
   in putText cv lx ly label

placeOnCircle :: (Int, Int) -> Int -> Int -> Int -> (Int, Int)
placeOnCircle (cx, cy) radius idx total =
  if total <= 0
    then (cx, cy)
    else
      let angle = (fromIntegral idx / fromIntegral total) * 2 * pi - pi / 2
          x = cx + round (fromIntegral radius * cos angle :: Double)
          y = cy + round (fromIntegral radius * sin angle * 0.55 :: Double)
       in (max 8 (min (canvasWidth - 8) x), max 4 (min (canvasHeight - 3) y))

pulseNode :: Int -> Int -> Char
pulseNode tick count
  | count >= 5 = ['◉', '●', '◎', '●'] !! (tick `mod` 4)
  | count >= 2 = ['●', '○', '●', '◦'] !! (tick `mod` 4)
  | otherwise = '○'

pulseLink :: Int -> Int -> Char
pulseLink tick count
  | count >= 5 = ['═', '─', '─', '─'] !! (tick `mod` 4)
  | count >= 2 = ['─', '·', '─', '·'] !! (tick `mod` 4)
  | otherwise = '·'

shortHost :: String -> String
shortHost s
  | length s <= 11 = s
  | '.' `elem` s = compactIp s
  | otherwise = take 10 s

compactIp :: String -> String
compactIp ip =
  let parts = splitDots ip
   in case reverse parts of
        d : c : _ -> c ++ "." ++ d
        [d] -> d
        _ -> take 11 ip

splitDots :: String -> [String]
splitDots s =
  case break (== '.') s of
    (a, '.' : rest) -> a : splitDots rest
    (a, _) -> [a]

maxRate :: NetSnapshot -> Double
maxRate snap =
  maximum $
    1 : [d | (_, d, _) <- snapInterfaces snap] ++ [u | (_, _, u) <- snapInterfaces snap]

rateDown :: Maybe (Map String InterfaceStats) -> Map String InterfaceStats -> String -> Double -> Double
rateDown prevMap curMap name interval =
  case (prevMap >>= Map.lookup name, Map.lookup name curMap) of
    (Just a, Just b) -> fst (ratesFromDiff (diffStats a b) interval)
    _ -> 0

rateUp :: Maybe (Map String InterfaceStats) -> Map String InterfaceStats -> String -> Double -> Double
rateUp prevMap curMap name interval =
  case (prevMap >>= Map.lookup name, Map.lookup name curMap) of
    (Just a, Just b) -> snd (ratesFromDiff (diffStats a b) interval)
    _ -> 0

indexByName :: [InterfaceStats] -> Map String InterfaceStats
indexByName = Map.fromList . map (\s -> (ifaceName s, s))

label :: Bool -> String -> String
label False s = s
label True s = "\ESC[96m" ++ s ++ "\ESC[0m"

updatePingHistory :: NetSnapshot -> [Double] -> NetSnapshot
updatePingHistory snap hist = snap {snapPingMs = hist}
