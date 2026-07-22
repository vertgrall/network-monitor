module NetworkMonitor.Menu.LiveMonitor (liveMonitorPage) where

import NetworkMonitor.CLI (Command (..))
import NetworkMonitor.Menu.Actions as Act (runLiveAction, runOnceCmd)
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , ifaceLabel
  , persistSession
  , promptDefault
  , runMenuPage
  )
import NetworkMonitor.Session
  ( Session (..)
  , parseInterfaceInput
  , showInterface
  )

liveMonitorPage :: Session -> IO Session
liveMonitorPage session =
  runMenuPage
    "LIVE MONITOR"
    session
    pageSummary
    (pageLines session)
    FooterBack
    Nothing

pageLines :: Session -> [MenuLine]
pageLines session =
  [ MenuSection "Run"
  , MenuOpt "Live Bandwidth (Watch)" $ \s -> Act.runLiveAction s Watch "Watch stopped."
  , MenuOpt "Visual NetView" $ \s -> Act.runLiveAction s NetView "NetView stopped."
  , MenuOpt "Network Dashboard" $ \s -> Act.runLiveAction s Dashboard "Dashboard stopped."
  , MenuOpt "Interface snapshot" $ Act.runOnceCmd Interfaces
  , MenuSection "Options"
  , MenuOpt ("Interface filter (" ++ ifaceLabel (sessionInterface session) ++ ")") editInterface
  , MenuOpt ("Refresh interval (" ++ show (sessionInterval session) ++ "s)") editInterval
  , MenuOpt ("Auto-stop count (" ++ show (sessionCount session) ++ ")") editCount
  ]

pageSummary :: [String]
pageSummary =
  [ "  Live views use saved interval and interface filter."
  , "  Count 0 = run until Ctrl-C."
  , ""
  ]

editInterface :: Session -> IO Session
editInterface s = do
  val <-
    promptDefault
      ("Interface(s) [" ++ showInterface (sessionInterface s) ++ ", blank=all]: ")
      (showInterface (sessionInterface s))
  persistSession s {sessionInterface = parseInterfaceInput val}

editInterval :: Session -> IO Session
editInterval s = do
  val <- promptDefault ("Interval seconds [" ++ show (sessionInterval s) ++ "]: ") (show (sessionInterval s))
  case reads val of
    [(n, _)] | n > 0 -> persistSession s {sessionInterval = n}
    _ -> pure s

editCount :: Session -> IO Session
editCount s = do
  val <- promptDefault ("Watch count [" ++ show (sessionCount s) ++ "]: ") (show (sessionCount s))
  case reads val of
    [(n, _)] | n >= 0 -> persistSession s {sessionCount = n}
    _ -> pure s
