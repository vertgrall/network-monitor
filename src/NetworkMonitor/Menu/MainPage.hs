module NetworkMonitor.Menu.MainPage (runMainMenu) where

import NetworkMonitor.Art
import NetworkMonitor.Menu.Connections (connectionsPage)
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterExit)
  , configPathHint
  , ifaceLabel
  , runMenuPageWithAliases
  )
import NetworkMonitor.Menu.Diagnostics (diagnosticsPage)
import NetworkMonitor.Menu.Help (helpPage)
import NetworkMonitor.Menu.HubStatus (hubStatusLines)
import NetworkMonitor.Menu.LiveMonitor (liveMonitorPage)
import NetworkMonitor.Menu.MissionReports (missionReportsPage)
import NetworkMonitor.Menu.RouterPage (routerPage)
import NetworkMonitor.Menu.Settings (settingsPage)
import NetworkMonitor.Session (Session (..), loadSession)
import System.Exit (exitSuccess)

hubAliases :: [(String, String)]
hubAliases =
  [ ("l", "1")
  , ("c", "2")
  , ("d", "3")
  , ("r", "4")
  , ("m", "5")
  , ("s", "6")
  , ("?", "7")
  , ("h", "7")
  ]

runMainMenu :: IO ()
runMainMenu = do
  session <- loadSession
  loop session
  where
    loop session = do
      status <- hubStatusLines session
      session' <-
        runMenuPageWithAliases
          "NT SENTINEL"
          []
          session
          (mainSummary session ++ status)
          mainMenuLines
          FooterExit
          Nothing
          hubAliases
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
  [ MenuOpt "Live Monitor  (L)" liveMonitorPage
  , MenuOpt "Connections & Flow  (C)" connectionsPage
  , MenuOpt "Diagnostics  (D)" diagnosticsPage
  , MenuOpt "Router & WAN  (R)" routerPage
  , MenuOpt "Mission & Reports  (M)" missionReportsPage
  , MenuSection "System"
  , MenuOpt "Settings  (S)" settingsPage
  , MenuOpt "Help  (?)" helpPage
  ]

mainSummary :: Session -> [String]
mainSummary session =
  [ "  Hub menu — pick a category for deeper options."
  , "  Config: " ++ configPathHint
  , "  Interface : " ++ ifaceLabel (sessionInterface session)
  , "  Interval  : " ++ show (sessionInterval session) ++ " sec"
  , "  Theme     : " ++ sessionTheme session
  , ""
  ]
