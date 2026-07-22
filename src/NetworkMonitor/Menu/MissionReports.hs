module NetworkMonitor.Menu.MissionReports (missionReportsPage) where

import NetworkMonitor.CLI (Command (Apps, Intel, Mission, Report))
import NetworkMonitor.Menu.Actions as Act (runLiveAction, runOnceCmd)
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , runMenuPage
  )
import NetworkMonitor.Session (Session (..))

missionReportsPage :: Session -> IO Session
missionReportsPage session =
  runMenuPage
    "MISSION & REPORTS"
    session
    pageSummary
    pageLines
    FooterBack
    Nothing

pageLines :: [MenuLine]
pageLines =
  [ MenuSection "Run"
  , MenuOpt "Mission Control (live)" $ \s ->
      Act.runLiveAction s Mission "Mission control stopped."
  , MenuOpt "App traffic summary (live)" $ \s ->
      Act.runLiveAction s Apps "App traffic stopped."
  , MenuOpt "Network intel (snapshot)" $ Act.runOnceCmd Intel
  , MenuOpt "Export session report" $ Act.runOnceCmd Report
  ]

pageSummary :: [String]
pageSummary =
  [ "  Combined views and one-shot exports."
  , "  Reports saved under ~/.config/new-tower/reports/"
  , ""
  ]
