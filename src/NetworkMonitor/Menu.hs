module NetworkMonitor.Menu (runMenu) where

import Control.Exception
  ( AsyncException (UserInterrupt)
  , SomeException
  , catch
  , fromException
  , throwIO
  )
import Data.Char (isSpace)
import NetworkMonitor.Art
import NetworkMonitor.CLI
  ( Command (..)
  , Options (..)
  , runConnections
  , runDashboard
  , runFlow
  , runTrace
  , runDnsTrace
  , runListen
  , runApps
  , runMission
  , runIntel
  , runReport
  , runMultiPing
  , runInterfaces
  , runNetView
  , runPing
  , runPortCheck
  , runTopHosts
  , runWatch
  )
import NetworkMonitor.Session
  ( Session (..)
  , loadSession
  , parseCommaList
  , parseInterfaceInput
  , saveSession
  , showCommaList
  , showInterface
  )
import System.IO (hFlush, stdout)
import System.Info (os)

runMenu :: IO ()
runMenu = do
  session <- loadSession
  loop session Nothing
  where
    loop session mErr = do
      drawMainMenu session mErr
      choice <- promptLine "Select option [0-19]: "
      case choice of
        "1" -> runAction session Interfaces >> pause >> loop session Nothing
        "2" -> runAction session Watch >> pause >> loop session Nothing
        "3" -> runAction session Connections >> pause >> loop session Nothing
        "4" -> runAction session NetView >> pause >> loop session Nothing
        "5" -> runAction session Flow >> pause >> loop session Nothing
        "6" -> runAction session Dashboard >> pause >> loop session Nothing
        "7" -> runPingPrompt session >> pause >> loop session Nothing
        "8" -> runPortPrompt session >> pause >> loop session Nothing
        "9" -> runAction session TopHosts >> pause >> loop session Nothing
        "10" -> configureSession session >>= \s -> loop s Nothing
        "11" -> showHelp >> pause >> loop session Nothing
        "12" -> runAction session Mission >> pause >> loop session Nothing
        "13" -> runAction session Apps >> pause >> loop session Nothing
        "14" -> runTracePrompt session >> pause >> loop session Nothing
        "15" -> runDnsPrompt session >> pause >> loop session Nothing
        "16" -> runAction session Listen >> pause >> loop session Nothing
        "17" -> runAction session Intel >> pause >> loop session Nothing
        "18" -> runAction session MultiPing >> pause >> loop session Nothing
        "19" -> runAction session Report >> pause >> loop session Nothing
        "0" -> do
          clearScreen
          renderHeader
          renderMessage "  Session saved. Goodbye."
          putStrLn ""
        "" -> loop session (Just "Please enter a selection.")
        _ -> loop session (Just "Invalid selection. Enter 0-19.")

drawMainMenu :: Session -> Maybe String -> IO ()
drawMainMenu session mErr = do
  clearScreen
  renderHeader
  renderPanel
    "MAIN MENU"
    [ ""
    , "  1.  Interface Statistics"
    , "  2.  Live Bandwidth Monitor (Watch)"
    , "  3.  TCP Connections"
    , "  4.  Visual NetView"
    , "  5.  Traffic Flow (Emit Monitor)"
    , "  6.  Network Dashboard"
    , "  7.  Ping Host"
    , "  8.  Port Check"
    , "  9.  Top Remote Hosts"
    , "  10. Configure Session Options"
    , "  11. Help"
    , "  12. Mission Control"
    , "  13. App Traffic Summary"
    , "  14. Traceroute"
    , "  15. DNS Trace"
    , "  16. Listening Ports"
    , "  17. Network Intel"
    , "  18. Multi-Ping Board"
    , "  19. Export Session Report"
    , "  0.  Exit"
    , ""
    , "  Config: " ++ configPathHint
    , "  Saved defaults shown below."
    , ""
    , "  Interface : " ++ ifaceLabel (sessionInterface session)
    , "  Interval  : " ++ show (sessionInterval session) ++ " sec"
    , "  Count     : " ++ show (sessionCount session) ++ " (0 = until Ctrl-C)"
    , "  State     : " ++ sessionState session
    , "  Limit     : " ++ show (sessionLimit session)
    ]
  case mErr of
    Nothing -> pure ()
    Just err -> renderError err
  putStrLn ""

configPathHint :: String
configPathHint = "~/.config/new-tower/session.conf"

ifaceLabel :: [String] -> String
ifaceLabel [] = "(all interfaces)"
ifaceLabel xs = unwords xs

runPingPrompt :: Session -> IO ()
runPingPrompt session = do
  host <- promptDefault "Host to ping [8.8.8.8]: " "8.8.8.8"
  let opts = (optionsFromSession session Ping) {optTarget = host}
  runPing opts

runPortPrompt :: Session -> IO ()
runPortPrompt session = do
  host <- promptDefault "Host [google.com]: " "google.com"
  portStr <- promptDefault "Port [443]: " "443"
  let port = case reads portStr of
        [(n, _)] | n > 0 && n <= 65535 -> n
        _ -> 443
      opts = (optionsFromSession session PortCheck) {optTarget = host, optPort = port}
  runPortCheck opts

runTracePrompt :: Session -> IO ()
runTracePrompt session = do
  host <- promptDefault "Trace host [8.8.8.8]: " "8.8.8.8"
  runTrace (optionsFromSession session Trace) {optTarget = host}

runDnsPrompt :: Session -> IO ()
runDnsPrompt session = do
  host <- promptDefault "DNS lookup [google.com]: " "google.com"
  runDnsTrace (optionsFromSession session DnsTrace) {optTarget = host}

runAction :: Session -> Command -> IO ()
runAction session cmd =
  let opts = optionsFromSession session cmd
   in case cmd of
        Interfaces -> runInterfaces opts
        Watch ->
          runWatch opts `catch` \e ->
            if isUserInterrupt e
              then renderMessage "\n  Watch stopped."
              else throwIO (e :: SomeException)
        Connections -> runConnections opts
        NetView ->
          runNetView opts `catch` \e ->
            if isUserInterrupt e
              then renderMessage "\n  NetView stopped."
              else throwIO (e :: SomeException)
        Flow ->
          runFlow opts `catch` \e ->
            if isUserInterrupt e
              then renderMessage "\n  Traffic flow monitor stopped."
              else throwIO (e :: SomeException)
        Dashboard -> runDashboard opts
        Ping -> runPing opts
        TopHosts -> runTopHosts opts
        PortCheck -> runPortCheck opts
        Trace -> runTrace opts
        DnsTrace -> runDnsTrace opts
        Listen -> runListen opts
        Apps -> runApps opts
        Mission -> runMission opts
        Intel -> runIntel opts
        Report -> runReport opts
        MultiPing -> runMultiPing opts
        Menu -> pure ()

isUserInterrupt :: SomeException -> Bool
isUserInterrupt e =
  case fromException e of
    Just UserInterrupt -> True
    _ -> False

optionsFromSession :: Session -> Command -> Options
optionsFromSession s cmd =
  Options
    { optCommand = cmd
    , optInterval = sessionInterval s
    , optCount = sessionCount s
    , optInterface = sessionInterface s
    , optState = sessionState s
    , optLimit = sessionLimit s
    , optTarget = "8.8.8.8"
    , optPort = 443
    , optPingCount = 4
    , optFavorites = sessionFavorites s
    , optBlocklist = sessionBlocklist s
    , optEmitAlert = sessionEmitAlert s
    , optLogging = sessionLogging s
    }

configureSession :: Session -> IO Session
configureSession session = do
  let go s mErr = do
        drawConfigMenu s mErr
        choice <- promptLine "Select field to edit [0-9]: "
        case choice of
          "1" -> editInterface s >>= \s' -> saveSession s' >> go s' Nothing
          "2" -> editInterval s >>= \s' -> saveSession s' >> go s' Nothing
          "3" -> editCount s >>= \s' -> saveSession s' >> go s' Nothing
          "4" -> editState s >>= \s' -> saveSession s' >> go s' Nothing
          "5" -> editLimit s >>= \s' -> saveSession s' >> go s' Nothing
          "6" -> editFavorites s >>= \s' -> saveSession s' >> go s' Nothing
          "7" -> editBlocklist s >>= \s' -> saveSession s' >> go s' Nothing
          "8" -> editEmitAlert s >>= \s' -> saveSession s' >> go s' Nothing
          "9" -> editLogging s >>= \s' -> saveSession s' >> go s' Nothing
          "0" -> pure s
          "" -> go s (Just "Please enter a selection.")
          _ -> go s (Just "Invalid selection. Enter 0-9.")
  go session Nothing

drawConfigMenu :: Session -> Maybe String -> IO ()
drawConfigMenu session mErr = do
  clearScreen
  renderHeader
  renderPanel
    "CONFIGURE SESSION OPTIONS"
    [ ""
    , "  1.  Interface filter     : " ++ ifaceLabel (sessionInterface session)
    , "  2.  Watch interval (sec) : " ++ show (sessionInterval session)
    , "  3.  Watch count          : " ++ show (sessionCount session)
    , "  4.  Connection state     : " ++ sessionState session
    , "  5.  Connection limit     : " ++ show (sessionLimit session)
    , "  6.  Favorite hosts       : " ++ showCommaList (sessionFavorites session)
    , "  7.  Blocklist hosts      : " ++ showCommaList (sessionBlocklist session)
    , "  8.  Emit alert (B/s)     : " ++ show (sessionEmitAlert session)
    , "  9.  Session logging      : " ++ if sessionLogging session then "on" else "off"
    , "  0.  Save and return to main menu"
    , ""
    , "  Changes are saved immediately to disk."
    ]
  case mErr of
    Nothing -> pure ()
    Just err -> renderError err
  putStrLn ""

editInterface :: Session -> IO Session
editInterface s = do
  val <-
    promptDefault
      ("Interface(s) [" ++ showInterface (sessionInterface s) ++ ", blank=all]: ")
      (showInterface (sessionInterface s))
  pure s {sessionInterface = parseInterfaceInput val}

editInterval :: Session -> IO Session
editInterval s = do
  val <- promptDefault ("Interval seconds [" ++ show (sessionInterval s) ++ "]: ") (show (sessionInterval s))
  case reads val of
    [(n, _)] | n > 0 -> pure s {sessionInterval = n}
    _ -> do
      renderError "Invalid interval. Keeping previous value."
      pause
      pure s

editCount :: Session -> IO Session
editCount s = do
  val <- promptDefault ("Watch count [" ++ show (sessionCount s) ++ "]: ") (show (sessionCount s))
  case reads val of
    [(n, _)] | n >= 0 -> pure s {sessionCount = n}
    _ -> do
      renderError "Invalid count. Keeping previous value."
      pause
      pure s

editState :: Session -> IO Session
editState s = do
  val <- promptDefault ("Connection state [" ++ sessionState s ++ "]: ") (sessionState s)
  pure s {sessionState = if null (dropWhile isSpace val) then sessionState s else val}

editLimit :: Session -> IO Session
editLimit s = do
  val <- promptDefault ("Connection limit [" ++ show (sessionLimit s) ++ "]: ") (show (sessionLimit s))
  case reads val of
    [(n, _)] | n > 0 -> pure s {sessionLimit = n}
    _ -> do
      renderError "Invalid limit. Keeping previous value."
      pause
      pure s

editFavorites :: Session -> IO Session
editFavorites s = do
  val <- promptDefault ("Favorite hosts [" ++ showCommaList (sessionFavorites s) ++ "]: ") (showCommaList (sessionFavorites s))
  pure s {sessionFavorites = parseCommaList val}

editBlocklist :: Session -> IO Session
editBlocklist s = do
  val <- promptDefault ("Blocklist [" ++ showCommaList (sessionBlocklist s) ++ "]: ") (showCommaList (sessionBlocklist s))
  pure s {sessionBlocklist = parseCommaList val}

editEmitAlert :: Session -> IO Session
editEmitAlert s = do
  val <- promptDefault ("Emit alert bytes/sec [" ++ show (sessionEmitAlert s) ++ "]: ") (show (sessionEmitAlert s))
  case reads val of
    [(n, _)] | n > 0 -> pure s {sessionEmitAlert = n}
    _ -> pure s

editLogging :: Session -> IO Session
editLogging s = do
  val <- promptDefault ("Session logging [on/off]: ") (if sessionLogging s then "on" else "off")
  pure s {sessionLogging = val == "on" || val == "1" || val == "yes"}

showHelp :: IO ()
showHelp = do
  clearScreen
  renderHeader
  renderPanel
    "HELP"
    [ ""
    , "  Interactive menu mode (no arguments):"
    , "    network-monitor"
    , ""
    , "  Direct CLI commands:"
    , "    network-monitor interfaces [-I IFACE]"
    , "    network-monitor watch [-I IFACE] [-i SECS] [-c N]"
    , "    network-monitor connections [-s STATE] [-l N]"
    , "    network-monitor netview [-I IFACE] [-i SECS] [-c N]"
    , "    network-monitor flow [-s STATE] [-l N] [-i SECS]"
    , "    network-monitor dashboard [-I IFACE]"
    , "    network-monitor ping [-H HOST] [--pings N]"
    , "    network-monitor port [-H HOST] [-p PORT]"
    , "    network-monitor top [-s STATE] [-l N]"
    , "    network-monitor mission [-i SECS] [--log]"
    , "    network-monitor trace -H HOST"
    , "    network-monitor dns -H DOMAIN"
    , "    network-monitor listen"
    , "    network-monitor intel"
    , "    network-monitor report"
    , "    network-monitor mping [-F HOST ...]"
    , ""
    , "  Aliases: if, w, conn, nv, flow, emit, dash, menu"
    , ""
    , "  Logs: ~/.config/new-tower/sessions.log"
    , "  Reports: ~/.config/new-tower/reports/"
    , ""
    , "  Session defaults persist at:"
    , "    ~/.config/new-tower/session.conf"
    , ""
    , "  Platform: " ++ os
    ]

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
