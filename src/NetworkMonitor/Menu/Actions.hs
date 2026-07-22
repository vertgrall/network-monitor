module NetworkMonitor.Menu.Actions
  ( optionsFromSession
  , runCommandAction
  , runLiveAction
  , runOnceAndPause
  , runOnceCmd
  , runPingPrompt
  , runPortPrompt
  , runTracePrompt
  , runDnsPrompt
  , runHealthOnce
  , runLanMapOnce
  , runExportInbound
  , runExportFlows
  , runTestSnmp
  , runInboundLive
  ) where

import Control.Exception
  ( AsyncException (UserInterrupt)
  , SomeException
  , catch
  , fromException
  , throwIO
  )
import Data.Char (isSpace)
import NetworkMonitor.Art (renderMessage)
import NetworkMonitor.CLI
  ( Command (..)
  , Options (..)
  , runApps
  , runConnections
  , runDashboard
  , runDnsTrace
  , runFlow
  , runHealth
  , runInbound
  , runIntel
  , runInterfaces
  , runLanMap
  , runListen
  , runMission
  , runMultiPing
  , runNetView
  , runPing
  , runPortCheck
  , runReport
  , runRouter
  , runTopHosts
  , runTrace
  , runWatch
  )
import qualified NetworkMonitor.CLI as CLI
import NetworkMonitor.Export (exportFlowsCsv, exportInboundCsv)
import NetworkMonitor.Flow (readFlows)
import NetworkMonitor.Inbound (filterInboundFlows)
import NetworkMonitor.Menu.Core (pause, promptDefault)
import NetworkMonitor.Recent (recordRecent)
import NetworkMonitor.Router (testSnmpConnection)
import NetworkMonitor.Session (Session (..))
import NetworkMonitor.Probe (defaultGateway)

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
    , optRouterHost = sessionRouterHost s
    , optSnmpCommunity = sessionSnmpCommunity s
    , optSnmpWanIf = sessionSnmpWanIf s
    , optFlowResolveDns = sessionFlowResolveDns s
    , optFlowShowApps = sessionFlowShowApps s
    , optFlowInboundOnly = sessionFlowInboundOnly s
    , optGeoLookup = sessionGeoLookup s
    , optNotifyAlerts = sessionNotifyAlerts s
    }

runOnceAndPause :: Session -> Command -> IO Session
runOnceAndPause session cmd = do
  recordRecent (commandLabel cmd)
  runCommandAction session cmd
  pause
  pure session

runOnceCmd :: Command -> Session -> IO Session
runOnceCmd cmd session = runOnceAndPause session cmd

runLiveAction :: Session -> Command -> String -> IO Session
runLiveAction session cmd stopMsg = do
  recordRecent (commandLabel cmd ++ " (live)")
  (runCommandAction session cmd >> pure session)
    `catch` \e ->
      if isUserInterrupt e
        then renderMessage ("\n  " ++ stopMsg) >> pure session
        else throwIO (e :: SomeException)

runCommandAction :: Session -> Command -> IO ()
runCommandAction session cmd =
  case cmd of
    Interfaces -> runInterfaces (optionsFromSession session cmd)
    Watch -> runWatch (optionsFromSession session cmd)
    Connections -> runConnections (optionsFromSession session cmd)
    NetView -> runNetView (optionsFromSession session cmd)
    Flow -> runFlow (optionsFromSession session cmd)
    Dashboard -> runDashboard (optionsFromSession session cmd)
    Ping -> runPing (optionsFromSession session cmd)
    TopHosts -> runTopHosts (optionsFromSession session cmd)
    PortCheck -> runPortCheck (optionsFromSession session cmd)
    Trace -> runTrace (optionsFromSession session cmd)
    DnsTrace -> runDnsTrace (optionsFromSession session cmd)
    Listen -> runListen (optionsFromSession session cmd)
    Apps -> runApps (optionsFromSession session cmd)
    Mission -> runMission (optionsFromSession session cmd)
    Intel -> runIntel (optionsFromSession session cmd)
    Report -> runReport (optionsFromSession session cmd)
    MultiPing -> runMultiPing (optionsFromSession session cmd)
    Router -> runRouter (optionsFromSession session cmd)
    Inbound -> runInbound (optionsFromSession session cmd)
    Health -> runHealth (optionsFromSession session cmd)
    LanMap -> runLanMap (optionsFromSession session cmd)
    Menu -> pure ()

runHealthOnce :: Session -> IO Session
runHealthOnce = runOnceCmd Health

runLanMapOnce :: Session -> IO Session
runLanMapOnce = runOnceCmd LanMap

runInboundLive :: Session -> IO Session
runInboundLive session = do
  recordRecent "inbound (live)"
  (CLI.runInboundLive (optionsFromSession session Inbound) >> pure session)
    `catch` \e ->
      if isUserInterrupt e
        then renderMessage "\n  Inbound watcher stopped." >> pure session
        else throwIO (e :: SomeException)

runExportInbound :: Session -> IO Session
runExportInbound session = do
  flows <- readFlows
  let inbound = filterInboundFlows (sessionState session) (sessionLimit session) flows
  path <- exportInboundCsv inbound
  renderMessage ("  Exported inbound CSV: " ++ path)
  recordRecent "export inbound csv"
  pause
  pure session

runExportFlows :: Session -> IO Session
runExportFlows session = do
  flows <- readFlows
  path <- exportFlowsCsv flows
  renderMessage ("  Exported flows CSV: " ++ path)
  recordRecent "export flows csv"
  pause
  pure session

runTestSnmp :: Session -> IO Session
runTestSnmp session = do
  hostInput <-
    promptDefault
      ("Router IP [" ++ routerLabel (sessionRouterHost session) ++ "]: ")
      (sessionRouterHost session)
  gateway <- defaultGateway
  let host =
        if null (dropWhile isSpace hostInput)
          then maybe "" id gateway
          else hostInput
  if null host
    then renderMessage "  No router IP configured or detected." >> pause >> pure session
    else do
      result <-
        testSnmpConnection
          host
          (sessionSnmpCommunity session)
          (if null (sessionSnmpWanIf session) then Nothing else Just (sessionSnmpWanIf session))
      case result of
        Left err -> renderMessage ("  " ++ err)
        Right msg -> renderMessage ("  " ++ msg)
      recordRecent "test snmp"
      pause
      pure session
  where
    routerLabel "" = "auto gateway"
    routerLabel x = x

isUserInterrupt :: SomeException -> Bool
isUserInterrupt e =
  case fromException e of
    Just UserInterrupt -> True
    _ -> False

commandLabel :: Command -> String
commandLabel = show

runPingPrompt :: Session -> IO Session
runPingPrompt session = do
  host <- promptDefault "Host to ping [8.8.8.8]: " "8.8.8.8"
  let opts = (optionsFromSession session Ping) {optTarget = host}
  runPing opts
  pause
  pure session

runPortPrompt :: Session -> IO Session
runPortPrompt session = do
  host <- promptDefault "Host [google.com]: " "google.com"
  portStr <- promptDefault "Port [443]: " "443"
  let port = case reads portStr of
        [(n, _)] | n > 0 && n <= 65535 -> n
        _ -> 443
      opts = (optionsFromSession session PortCheck) {optTarget = host, optPort = port}
  runPortCheck opts
  pause
  pure session

runTracePrompt :: Session -> IO Session
runTracePrompt session = do
  host <- promptDefault "Trace host [8.8.8.8]: " "8.8.8.8"
  runTrace (optionsFromSession session Trace) {optTarget = host}
  pause
  pure session

runDnsPrompt :: Session -> IO Session
runDnsPrompt session = do
  host <- promptDefault "DNS lookup [google.com]: " "google.com"
  runDnsTrace (optionsFromSession session DnsTrace) {optTarget = host}
  pause
  pure session
