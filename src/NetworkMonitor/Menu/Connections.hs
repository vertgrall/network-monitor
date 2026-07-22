module NetworkMonitor.Menu.Connections (connectionsPage) where

import NetworkMonitor.CLI (Command (..))
import NetworkMonitor.Menu.Actions as Act
  ( runExportFlows
  , runExportInbound
  , runInboundLive
  , runLiveAction
  , runOnceCmd
  )
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , onOff
  , persistSession
  , promptDefault
  , runMenuPage
  )
import NetworkMonitor.Session (Session (..), parseCommaList, showCommaList)

connectionsPage :: Session -> IO Session
connectionsPage session =
  runMenuPage
    "CONNECTIONS & FLOW"
    ["Main", "Connections & Flow"]
    session
    pageSummary
    (pageLines session)
    FooterBack
    Nothing

pageLines :: Session -> [MenuLine]
pageLines session =
  [ MenuSection "Run"
  , MenuOpt "Traffic Flow (live emit monitor)" $ \s ->
      Act.runLiveAction s Flow "Traffic flow monitor stopped."
  , MenuOpt "Inbound watchers (live)" Act.runInboundLive
  , MenuOpt "Inbound watchers (snapshot)" $ Act.runOnceCmd Inbound
  , MenuOpt "TCP Connections (snapshot)" $ Act.runOnceCmd Connections
  , MenuOpt "Top remote hosts" $ Act.runOnceCmd TopHosts
  , MenuOpt "Listening ports" $ Act.runOnceCmd Listen
  , MenuSection "Flow options"
  , MenuOpt ("Resolve DNS names [" ++ onOff (sessionFlowResolveDns session) ++ "]") toggleFlowDns
  , MenuOpt ("Per-app rollup [" ++ onOff (sessionFlowShowApps session) ++ "]") toggleFlowApps
  , MenuOpt ("Inbound-only flow [" ++ onOff (sessionFlowInboundOnly session) ++ "]") toggleFlowInbound
  , MenuOpt ("Geo lookup [" ++ onOff (sessionGeoLookup session) ++ "]") toggleGeo
  , MenuOpt ("Connection state (" ++ sessionState session ++ ")") editState
  , MenuOpt ("Row limit (" ++ show (sessionLimit session) ++ ")") editLimit
  , MenuOpt ("Blocklist (" ++ blockLabel session ++ ")") editBlocklist
  , MenuOpt ("Emit alert (" ++ show (sessionEmitAlert session) ++ " B/s)") editEmitAlert
  , MenuSection "Export"
  , MenuOpt "Export inbound CSV" Act.runExportInbound
  , MenuOpt "Export all flows CSV" Act.runExportFlows
  ]

pageSummary :: [String]
pageSummary =
  [ "  Flow shows outbound traffic; inbound-only filters to remote hosts on your ports."
  , "  Geo lookup uses ip-api.com for public IPs (LAN shows as local)."
  , ""
  ]

blockLabel :: Session -> String
blockLabel s =
  let xs = sessionBlocklist s
   in if null xs then "none" else showCommaList xs

toggleFlowDns :: Session -> IO Session
toggleFlowDns s =
  persistSession s {sessionFlowResolveDns = not (sessionFlowResolveDns s)}

toggleFlowApps :: Session -> IO Session
toggleFlowApps s =
  persistSession s {sessionFlowShowApps = not (sessionFlowShowApps s)}

toggleFlowInbound :: Session -> IO Session
toggleFlowInbound s =
  persistSession s {sessionFlowInboundOnly = not (sessionFlowInboundOnly s)}

toggleGeo :: Session -> IO Session
toggleGeo s =
  persistSession s {sessionGeoLookup = not (sessionGeoLookup s)}

editState :: Session -> IO Session
editState s = do
  val <- promptDefault ("Connection state [" ++ sessionState s ++ "]: ") (sessionState s)
  persistSession s {sessionState = if null (dropWhile (== ' ') val) then sessionState s else val}

editLimit :: Session -> IO Session
editLimit s = do
  val <- promptDefault ("Connection limit [" ++ show (sessionLimit s) ++ "]: ") (show (sessionLimit s))
  case reads val of
    [(n, _)] | n > 0 -> persistSession s {sessionLimit = n}
    _ -> pure s

editBlocklist :: Session -> IO Session
editBlocklist s = do
  val <- promptDefault ("Blocklist [" ++ showCommaList (sessionBlocklist s) ++ "]: ") (showCommaList (sessionBlocklist s))
  persistSession s {sessionBlocklist = parseCommaList val}

editEmitAlert :: Session -> IO Session
editEmitAlert s = do
  val <- promptDefault ("Emit alert bytes/sec [" ++ show (sessionEmitAlert s) ++ "]: ") (show (sessionEmitAlert s))
  case reads val of
    [(n, _)] | n > 0 -> persistSession s {sessionEmitAlert = n}
    _ -> pure s
