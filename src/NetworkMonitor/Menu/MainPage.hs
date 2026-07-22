module NetworkMonitor.Menu.MainPage (runMainMenu) where

import NetworkMonitor.Art
import NetworkMonitor.Menu.Connections (connectionsPage)
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterExit)
  , configPathHint
  , ifaceLabel
  , runMenuPage
  )
import NetworkMonitor.Menu.Diagnostics (diagnosticsPage)
import NetworkMonitor.Menu.Help (helpPage)
import NetworkMonitor.Menu.LiveMonitor (liveMonitorPage)
import NetworkMonitor.Menu.MissionReports (missionReportsPage)
import NetworkMonitor.Menu.RouterPage (routerPage)
import NetworkMonitor.Menu.Settings (settingsPage)
import NetworkMonitor.Session (Session (..), loadSession)
import System.Exit (exitSuccess)

runMainMenu :: IO ()
runMainMenu = do
  session <- loadSession
  loop session
  where
    loop session = do
      session' <-
        runMenuPage
          "NT SENTINEL"
          session
          (mainSummary session)
          mainMenuLines
          FooterExit
          Nothing
      if session' == session
        then goodbye
        else loop session'
    goodbye = do
      clearScreen
      renderHeader
      renderMessage "  Session saved. Goodbye."
      putStrLn ""
      exitSuccess

mainMenuLines :: [MenuLine]
mainMenuLines =
  [ MenuOpt "Live Monitor" liveMonitorPage
  , MenuOpt "Connections & Flow" connectionsPage
  , MenuOpt "Diagnostics" diagnosticsPage
  , MenuOpt "Router & WAN" routerPage
  , MenuOpt "Mission & Reports" missionReportsPage
  , MenuSection "System"
  , MenuOpt "Settings" settingsPage
  , MenuOpt "Help" helpPage
  ]

mainSummary :: Session -> [String]
mainSummary session =
  [ "  Hub menu — pick a category for deeper options."
  , "  Config: " ++ configPathHint
  , "  Interface : " ++ ifaceLabel (sessionInterface session)
  , "  Interval  : " ++ show (sessionInterval session) ++ " sec"
  , "  State     : " ++ sessionState session
  , "  Limit     : " ++ show (sessionLimit session)
  , ""
  ]
