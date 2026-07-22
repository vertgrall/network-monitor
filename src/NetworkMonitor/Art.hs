module NetworkMonitor.Art
  ( screenWidth
  , clearScreen
  , hideCursor
  , showCursor
  , enterAltScreen
  , leaveAltScreen
  , bracketTerminal
  , renderHeader
  , renderHeaderAnimated
  , renderLiveBanner
  , renderPanel
  , renderPanelStorm
  , renderError
  , renderPrompt
  , renderMessage
  , styleRate
  , styleBar
  , stylePlatform
  , styleTrend
  , styleTableHeader
  , useColor
  , center
  ) where

import Control.Exception (bracket_)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, intercalate)
import NetworkMonitor.Animate (pulseLive, spinnerAt, stormFlashLabel)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)

screenWidth :: Int
screenWidth = 78

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

enterAltScreen :: IO ()
enterAltScreen = putStr "\ESC[?1049h"

leaveAltScreen :: IO ()
leaveAltScreen = putStr "\ESC[?1049l"

bracketTerminal :: IO a -> IO a
bracketTerminal =
  bracket_
    (enterAltScreen >> hideCursor)
    (showCursor >> leaveAltScreen)

renderHeader :: IO ()
renderHeader = renderHeaderAnimated 0

renderHeaderAnimated :: Int -> IO ()
renderHeaderAnimated tick = do
  colorOn <- useColor
  mapM_ putStrLn (coloredNtsLogoAnimated colorOn tick)
  putStrLn ""
  putStrLn (center screenWidth (styleSubtitle colorOn))
  putStrLn (center screenWidth (styleFooter colorOn))
  putStrLn ""

renderLiveBanner :: Int -> Double -> Int -> Bool -> IO ()
renderLiveBanner tick interval refreshCount storm = do
  colorOn <- useColor
  let secs = round interval :: Int
      pulse = pulseLive tick
      spin = spinnerAt tick
      status =
        intercalate
          "  "
          [ pulse ++ " LIVE"
          , spin ++ " monitoring"
          , "every " ++ show secs ++ "s"
          , "refresh #" ++ show refreshCount
          , "Ctrl-C to quit"
          ]
  putStrLn $
    if storm
      then
        if colorOn
          then
            if tick `mod` 2 == 0
              then bold ++ brightRed ++ stormFlashLabel tick ++ reset ++ dim ++ "  bandwidth spike detected" ++ reset
              else bold ++ brightYellow ++ stormFlashLabel tick ++ reset ++ dim ++ "  bandwidth spike detected" ++ reset
          else stormFlashLabel tick ++ "  bandwidth spike detected"
      else
        if colorOn
          then
            bold
              ++ brightGreen
              ++ pulse
              ++ reset
              ++ brightWhite
              ++ " LIVE"
              ++ reset
              ++ dim
              ++ brightCyan
              ++ "  "
              ++ spin
              ++ " monitoring  every "
              ++ show secs
              ++ "s  refresh #"
              ++ show refreshCount
              ++ "  "
              ++ dim
              ++ "(Ctrl-C to quit)"
              ++ reset
          else status

styleRate :: Bool -> Double -> Double -> String -> String
styleRate False _ _ s = s
styleRate True rate peak s
  | peak <= 0 || rate <= 0 = dim ++ s ++ reset
  | rate / peak >= 0.65 = bold ++ brightRed ++ s ++ reset
  | rate / peak >= 0.3 = brightYellow ++ s ++ reset
  | otherwise = brightGreen ++ s ++ reset

styleBar :: Bool -> Double -> Double -> String -> String
styleBar False _ _ s = s
styleBar True rate peak s
  | peak <= 0 || rate <= 0 = dim ++ s ++ reset
  | rate / peak >= 0.65 = brightRed ++ s ++ reset
  | rate / peak >= 0.3 = brightYellow ++ s ++ reset
  | otherwise = brightCyan ++ s ++ reset

stylePlatform :: Bool -> String -> String
stylePlatform False os = "Platform: " ++ os
stylePlatform True os =
  dim ++ brightCyan ++ "Platform: " ++ brightWhite ++ os ++ reset

styleTrend :: Bool -> String -> String
styleTrend False s = s
styleTrend True s = dim ++ brightMagenta ++ s ++ reset

styleTableHeader :: Bool -> String -> String
styleTableHeader False s = s
styleTableHeader True s = bold ++ brightCyan ++ s ++ reset

useColor :: IO Bool
useColor = do
  noColor <- lookupEnv "NO_COLOR"
  tty <- hIsTerminalDevice stdout
  pure (tty && noColor == Nothing)

renderPanel :: String -> [String] -> IO ()
renderPanel title lines' = renderPanelStorm False 0 title lines'

renderPanelStorm :: Bool -> Int -> String -> [String] -> IO ()
renderPanelStorm storm tick title lines' = do
  colorOn <- useColor
  let inner = maximum (map length lines' ++ [length title, 40])
      width = min screenWidth (inner + 4)
      border = replicate (width - 2) (if storm then '!' else '-')
      plainTitle =
        padR (width - 4) $
          if storm
            then stormFlashLabel tick ++ "  " ++ title
            else title
      plainBody = map (padR (width - 4)) lines'
      top = styleBorderStorm colorOn storm tick ("+" ++ border ++ "+")
      bottom = top
      titleLine = stylePanelRowStorm colorOn storm tick (stylePanelTitleStorm colorOn storm tick plainTitle)
      body = map (stylePanelRowStorm colorOn storm tick . stylePanelBody colorOn) plainBody
  mapM_ putStrLn (top : titleLine : body ++ [bottom])

renderError :: String -> IO ()
renderError msg = do
  colorOn <- useColor
  putStrLn $
    if colorOn
      then bold ++ brightRed ++ "  >> " ++ msg ++ reset
      else "  >> " ++ msg

renderPrompt :: String -> IO ()
renderPrompt msg = do
  colorOn <- useColor
  putStr $
    if colorOn
      then bold ++ brightCyan ++ msg ++ reset
      else msg

renderMessage :: String -> IO ()
renderMessage msg = do
  colorOn <- useColor
  putStrLn $
    if colorOn
      then dim ++ cyan ++ msg ++ reset
      else msg

center :: Int -> String -> String
center w s =
  let len = length s
      pad = max 0 ((w - len) `div` 2)
   in replicate pad ' ' ++ s

padR :: Int -> String -> String
padR width s =
  let len = length s
   in if len >= width then take width s else s ++ replicate (width - len) ' '

styleBorder :: Bool -> String -> String
styleBorder False s = s
styleBorder True s = bold ++ brightMagenta ++ s ++ reset

styleBorderStorm :: Bool -> Bool -> Int -> String -> String
styleBorderStorm False _ _ s = s
styleBorderStorm True storm tick s
  | storm && tick `mod` 2 == 0 = bold ++ brightRed ++ s ++ reset
  | storm = bold ++ brightYellow ++ s ++ reset
  | otherwise = bold ++ brightMagenta ++ s ++ reset

stylePanelRowStorm :: Bool -> Bool -> Int -> String -> String
stylePanelRowStorm colorOn storm tick content =
  let edge = if storm then '!' else '|'
   in if colorOn
        then styleBorderStorm True storm tick [edge] ++ " " ++ content ++ " " ++ styleBorderStorm True storm tick [edge]
        else "| " ++ content ++ " |"

stylePanelTitleStorm :: Bool -> Bool -> Int -> String -> String
stylePanelTitleStorm False _ _ s = s
stylePanelTitleStorm True storm tick s
  | storm && tick `mod` 2 == 0 = bold ++ onRed ++ brightWhite ++ s ++ reset
  | storm = bold ++ onYellow ++ brightRed ++ s ++ reset
  | otherwise = bold ++ onMagenta ++ brightWhite ++ s ++ reset

stylePanelRow :: Bool -> String -> String
stylePanelRow colorOn content = stylePanelRowStorm colorOn False 0 content

stylePanelTitle :: Bool -> String -> String
stylePanelTitle colorOn s = stylePanelTitleStorm colorOn False 0 s

stylePanelBody :: Bool -> String -> String
stylePanelBody False line = line
stylePanelBody True line
  | all isSpace line = line
  | '\ESC' `elem` line = line
  | isMenuOptionLine line = colorMenuOption line
  | ':' `elem` line = colorConfigLine line
  | "network-monitor" `isPrefixOf` dropWhile isSpace line = colorCommandLine line
  | "network-monitor" `elem` words line = colorCommandLine line
  | otherwise = brightWhite ++ line ++ reset

isMenuOptionLine :: String -> Bool
isMenuOptionLine line =
  case dropWhile isSpace line of
    c : _ -> isDigit c
    _ -> False

colorMenuOption :: String -> String
colorMenuOption line =
  case span isSpace line of
    (spaces, rest) ->
      case break (== '.') rest of
        (num, '.' : after) | not (null num) && all isDigit num ->
          spaces
            ++ bold
            ++ brightYellow
            ++ num
            ++ "."
            ++ reset
            ++ brightWhite
            ++ after
            ++ reset
        _ -> brightWhite ++ line ++ reset

colorConfigLine :: String -> String
colorConfigLine line =
  case span isSpace line of
    (spaces, rest) ->
      case break (== ':') rest of
        (label, ':' : val) ->
          spaces
            ++ dim
            ++ brightCyan
            ++ label
            ++ ":"
            ++ reset
            ++ brightWhite
            ++ val
            ++ reset
        _ -> brightWhite ++ line ++ reset

colorCommandLine :: String -> String
colorCommandLine line =
  case breakSubstring "network-monitor" line of
    Nothing -> dim ++ cyan ++ line ++ reset
    Just (before, after) ->
      dim
        ++ cyan
        ++ before
        ++ reset
        ++ bold
        ++ brightGreen
        ++ "network-monitor"
        ++ reset
        ++ brightWhite
        ++ after
        ++ reset

breakSubstring :: String -> String -> Maybe (String, String)
breakSubstring needle haystack = go 0
  where
    nLen = length needle
    go i
      | i + nLen > length haystack = Nothing
      | take nLen (drop i haystack) == needle = Just (take i haystack, drop (i + nLen) haystack)
      | otherwise = go (i + 1)

coloredNtsLogo :: Bool -> [String]
coloredNtsLogo colorOn = coloredNtsLogoAnimated colorOn 0

coloredNtsLogoAnimated :: Bool -> Int -> [String]
coloredNtsLogoAnimated colorOn tick =
  map (center screenWidth . colorNtsLineAnimated colorOn tick) (zip [0 ..] rawNtsLogo)

colorNtsLine :: Bool -> (Int, String) -> String
colorNtsLine colorOn pair = colorNtsLineAnimated colorOn 0 pair

colorNtsLineAnimated :: Bool -> Int -> (Int, String) -> String
colorNtsLineAnimated False _ (_, line) = line
colorNtsLineAnimated True tick (_, line) =
  concat $ zipWith (colorNtsChar tick) [0 ..] line

colorNtsChar :: Int -> Int -> Char -> String
colorNtsChar _ _ ' ' = " "
colorNtsChar tick col c =
  let color = getGradientColor (col + tick)
   in bold ++ color ++ [c] ++ reset

getGradientColor :: Int -> String
getGradientColor col =
  case col `mod` 48 of
    n | n < 8 -> brightRed
    n | n < 16 -> brightYellow
    n | n < 24 -> brightGreen
    n | n < 32 -> brightCyan
    n | n < 40 -> brightBlue
    _ -> brightMagenta

styleSubtitle :: Bool -> String
styleSubtitle False = "NT Sentinel"
styleSubtitle True =
  bold ++ brightGreen ++ "NT" ++ brightWhite ++ " Sentinel" ++ reset

styleFooter :: Bool -> String
styleFooter False = "Jon M - Bellevue/Pasco Wa 2026"
styleFooter True =
  bold
    ++ brightMagenta
    ++ "Jon M"
    ++ reset
    ++ dim
    ++ brightCyan
    ++ " - Bellevue/Pasco Wa "
    ++ reset
    ++ bold
    ++ brightYellow
    ++ "2026"
    ++ reset

rawNtsLogo :: [String]
rawNtsLogo =
  [ "    _   _____________            __  _            __"
  , "   / | / /_  __/ ___/___  ____  / /_(_)___  ___  / /"
  , "  /  |/ / / /  \\__ \\/ _ \\/ __ \\/ __/ / __ \\/ _ \\/ / "
  , " / /|  / / /  ___/ /  __/ / / / /_/ / / / /  __/ /  "
  , "/_/ |_/ /_/  /____/\\___/_/ /_/\\__/_/_/ /_/\\___/_/   "
  ]

-- ANSI styling
reset, bold, dim, cyan, magenta, brightMagenta, brightCyan, brightBlue, brightGreen, brightYellow, brightWhite, brightRed, onMagenta, onRed, onYellow :: String
reset = "\ESC[0m"
bold = "\ESC[1m"
dim = "\ESC[2m"
cyan = "\ESC[36m"
magenta = "\ESC[35m"
brightMagenta = "\ESC[95m"
brightCyan = "\ESC[96m"
brightBlue = "\ESC[94m"
brightGreen = "\ESC[92m"
brightYellow = "\ESC[93m"
brightWhite = "\ESC[97m"
brightRed = "\ESC[91m"
onMagenta = "\ESC[45m"
onRed = "\ESC[41m"
onYellow = "\ESC[43m"
