module NetworkMonitor.Menu.Diagnostics (diagnosticsPage) where

import NetworkMonitor.CLI (Command (MultiPing))
import NetworkMonitor.Menu.Actions as Act
  ( runDnsPrompt
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
  ]

pageSummary :: [String]
pageSummary =
  [ "  Ping, port, trace, and DNS tools."
  , "  Multi-ping uses favorite hosts from Settings."
  , ""
  ]
