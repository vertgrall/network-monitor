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
  , runInbound
  , runIntel
  , runInterfaces
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
import NetworkMonitor.Menu.Core (pause, promptDefault)
import NetworkMonitor.Session (Session (..))

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
    }

runOnceAndPause :: Session -> Command -> IO Session
runOnceAndPause session cmd = do
  runCommandAction session cmd
  pause
  pure session

runOnceCmd :: Command -> Session -> IO Session
runOnceCmd cmd session = runOnceAndPause session cmd

runLiveAction :: Session -> Command -> String -> IO Session
runLiveAction session cmd stopMsg =
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
    Menu -> pure ()

isUserInterrupt :: SomeException -> Bool
isUserInterrupt e =
  case fromException e of
    Just UserInterrupt -> True
    _ -> False

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
