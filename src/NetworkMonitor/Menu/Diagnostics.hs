module NetworkMonitor.Menu.Diagnostics (diagnosticsPage) where

import NetworkMonitor.CLI (Command (MultiPing))
import NetworkMonitor.Menu.Actions as Act
  ( runDnsPrompt
  , runHealthOnce
  , runLanMapOnce
  , runLiveAction
  , runPingPrompt
  , runPortPrompt
  , runTracePrompt
  )
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , runMenuPage
  )
import NetworkMonitor.Session (Session (..))

diagnosticsPage :: Session -> IO Session
diagnosticsPage session =
  runMenuPage
    "DIAGNOSTICS"
    ["Main", "Diagnostics"]
    session
    pageSummary
    pageLines
    FooterBack
    Nothing

pageLines :: [MenuLine]
pageLines =
  [ MenuSection "Run"
  , MenuOpt "Ping host" Act.runPingPrompt
  , MenuOpt "Port check" Act.runPortPrompt
  , MenuOpt "Traceroute" Act.runTracePrompt
  , MenuOpt "DNS trace" Act.runDnsPrompt
  , MenuOpt "Multi-ping board (live)" $ \s ->
      Act.runLiveAction s MultiPing "Multi-ping stopped."
  , MenuSection "Network health"
  , MenuOpt "Health score (snapshot)" Act.runHealthOnce
  , MenuOpt "LAN device map" Act.runLanMapOnce
  ]

pageSummary :: [String]
pageSummary =
  [ "  Ping, port, trace, and DNS tools."
  , "  Multi-ping uses favorite hosts from Settings."
  , ""
  ]
