module NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (..)
  , runMenuPage
  , runMenuPageWithAliases
  , pause
  , promptLine
  , promptDefault
  , configPathHint
  , ifaceLabel
  , onOff
  , persistSession
  , renderBreadcrumb
  , normalizeChoice
  ) where

import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd)
import Data.List (intercalate)
import NetworkMonitor.Art
  ( clearScreen
  , renderError
  , renderHeader
  , renderPanel
  , renderPrompt
  , useColor
  )
import NetworkMonitor.Session (Session (..), saveSession)
import System.IO (hFlush, stdout)

data MenuLine
  = MenuSection !String
  | MenuOpt !String !(Session -> IO Session)
  deriving ()

data MenuFooter
  = FooterBack
  | FooterExit
  deriving (Eq, Show)

runMenuPage ::
  String ->
  [String] ->
  Session ->
  [String] ->
  [MenuLine] ->
  MenuFooter ->
  Maybe String ->
  IO Session
runMenuPage title breadcrumbs session summaryLines menuLines footer mErr =
  runMenuPageWithAliases title breadcrumbs session summaryLines menuLines footer mErr []

runMenuPageWithAliases ::
  String ->
  [String] ->
  Session ->
  [String] ->
  [MenuLine] ->
  MenuFooter ->
  Maybe String ->
  [(String, String)] ->
  IO Session
runMenuPageWithAliases title breadcrumbs session summaryLines menuLines footer mErr aliases =
  go session mErr
  where
    go s mErr' = do
      drawPage title breadcrumbs s summaryLines menuLines footer mErr'
      let (maxChoice, _) = numberedChoices menuLines
      choiceRaw <- promptLine ("Select option [0-" ++ show maxChoice ++ "]: ")
      let choice = normalizeChoice choiceRaw aliases
      case choice of
        "0" -> pure s
        "" -> go s (Just "Please enter a selection.")
        _ ->
          case lookupChoice choice menuLines of
            Nothing -> go s (Just ("Invalid selection. Enter 0-" ++ show maxChoice ++ "."))
            Just act -> act s >>= \s' -> go s' Nothing

drawPage ::
  String ->
  [String] ->
  Session ->
  [String] ->
  [MenuLine] ->
  MenuFooter ->
  Maybe String ->
  IO ()
drawPage title breadcrumbs session summaryLines menuLines footer mErr = do
  clearScreen
  renderHeader
  renderBreadcrumb (sessionTheme session) breadcrumbs
  let footerLabel =
        case footer of
          FooterBack -> "Back"
          FooterExit -> "Exit"
      body =
        [""]
          ++ menuBody menuLines footerLabel
          ++ [""]
          ++ summaryLines
  renderPanel title body
  case mErr of
    Nothing -> pure ()
    Just err -> renderError err
  putStrLn ""

menuBody :: [MenuLine] -> String -> [String]
menuBody lines footerLabel = go 1 lines ++ ["  0.  " ++ footerLabel, ""]
  where
    go _ [] = []
    go n (MenuSection label : rest) =
      ["" , "  -- " ++ label ++ " --"] ++ go n rest
    go n (MenuOpt label _ : rest) =
      ("  " ++ show n ++ ".  " ++ label) : go (n + 1) rest

numberedChoices :: [MenuLine] -> (Int, [(String, Session -> IO Session)])
numberedChoices lines =
  let numbered = go 1 lines
   in (length numbered, numbered)
  where
    go _ [] = []
    go n (MenuOpt _ act : rest) = (show n, act) : go (n + 1) rest
    go n (MenuSection _ : rest) = go n rest

lookupChoice :: String -> [MenuLine] -> Maybe (Session -> IO Session)
lookupChoice key lines =
  lookup key (snd (numberedChoices lines))

normalizeChoice :: String -> [(String, String)] -> String
normalizeChoice raw aliases =
  let trimmed = dropWhile (== ' ') (dropWhileEnd isSpace raw)
      lower = map toLower trimmed
   in case lookup lower aliases of
        Just n -> n
        Nothing -> trimmed
  where
    isSpace c = c == ' ' || c == '\t'

pause :: IO ()
pause = do
  renderPrompt "  Press Enter to continue..."
  hFlush stdout
  _ <- getLine
  pure ()

promptLine :: String -> IO String
promptLine msg = do
  renderPrompt msg
  hFlush stdout
  getLine

promptDefault :: String -> String -> IO String
promptDefault msg defaultVal = do
  val <- promptLine msg
  pure (if null (dropWhile isSpace val) then defaultVal else val)
  where
    isSpace c = c == ' ' || c == '\t'

configPathHint :: String
configPathHint = "~/.config/new-tower/session.conf"

ifaceLabel :: [String] -> String
ifaceLabel [] = "(all interfaces)"
ifaceLabel xs = unwords xs

onOff :: Bool -> String
onOff True = "on"
onOff False = "off"

persistSession :: Session -> IO Session
persistSession s = saveSession s >> pure s

renderBreadcrumb :: String -> [String] -> IO ()
renderBreadcrumb theme parts =
  if null parts
    then pure ()
    else do
      colorOn <- useColor
      let line = "  " ++ intercalate " › " parts
      if colorOn
        then case theme of
          "cyber" -> putStrLn ("\ESC[1m\ESC[95m" ++ line ++ "\ESC[0m")
          "minimal" -> putStrLn line
          _ -> putStrLn ("\ESC[1m\ESC[96m" ++ line ++ "\ESC[0m")
        else putStrLn line
