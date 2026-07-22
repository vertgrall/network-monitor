module NetworkMonitor.CLI
  ( Command (..)
  , Options (..)
  , parseOptions
  , runCommand
  , runInterfaces
  , runWatch
  , runConnections
  , runDashboard
  , runPing
  , runTopHosts
  , runPortCheck
  , runNetView
  , runFlow
  , runTrace
  , runDnsTrace
  , runListen
  , runApps
  , runMission
  , runIntel
  , runReport
  , runMultiPing
  , runRouter
  , runInbound
  , runInboundLive
  , runHealth
  , runLanMap
  , mergeSessionOptions
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, when)
import Data.Int (Int64)
import Data.List (intercalate, sort, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import NetworkMonitor.Alert
import NetworkMonitor.Animate
import NetworkMonitor.Apps (computeAppEmits, renderAppLines)
import NetworkMonitor.Art
import NetworkMonitor.DNS (dnsTrace, lookupHostLabel)
import NetworkMonitor.Flow
  ( HostEmit (..)
  , computeHostEmits
  , readFlows
  , renderFlowExtras
  , renderFlowPanel
  )
import qualified NetworkMonitor.Flow as FlowLib
import NetworkMonitor.Health (computeHealth, renderHealthLines)
import NetworkMonitor.History (recordMetric)
import NetworkMonitor.HostIntel (formatGeoShort, lookupGeo)
import NetworkMonitor.Inbound (filterInboundFlows, renderInboundPanel)
import NetworkMonitor.LanMap (readLanDevices, renderLanMapLines)
import NetworkMonitor.Notify (notifyUser)
import NetworkMonitor.Timeline (appendTimeline)
import NetworkMonitor.Format
import NetworkMonitor.Hosts (topRemoteHosts)
import NetworkMonitor.Intel (gatherIntel, renderIntelLines)
import NetworkMonitor.Listen (readListeners, renderListenerLines)
import NetworkMonitor.Log (appendSessionLog)
import NetworkMonitor.Mission (renderMissionPanel)
import NetworkMonitor.NetView (gatherNetSnapshot, maxRate, renderNetViewLines, snapInterfaces, updatePingHistory)
import NetworkMonitor.Probe
import NetworkMonitor.Report (writeSessionReport)
import NetworkMonitor.Router
  ( WanIface (..)
  , discoverWanInterface
  , readWanOctets
  , renderRouterPanel
  , snmpAvailable
  , wanRates
  )
import NetworkMonitor.Session (Session (..))
import NetworkMonitor.Stats
import NetworkMonitor.Trace (traceRoute)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Info (os)

data Command
  = Interfaces
  | Watch
  | Connections
  | Dashboard
  | NetView
  | Flow
  | Trace
  | DnsTrace
  | Listen
  | Apps
  | Mission
  | Intel
  | Report
  | MultiPing
  | Router
  | Inbound
  | Health
  | LanMap
  | Ping
  | TopHosts
  | PortCheck
  | Menu
  deriving (Eq, Show)

data Options = Options
  { optCommand :: !Command
  , optInterval :: !Double
  , optCount :: !Int
  , optInterface :: ![String]
  , optState :: !String
  , optLimit :: !Int
  , optTarget :: !String
  , optPort :: !Int
  , optPingCount :: !Int
  , optFavorites :: ![String]
  , optBlocklist :: ![String]
  , optEmitAlert :: !Double
  , optLogging :: !Bool
  , optRouterHost :: !String
  , optSnmpCommunity :: !String
  , optSnmpWanIf :: !String
  , optFlowResolveDns :: !Bool
  , optFlowShowApps :: !Bool
  , optFlowInboundOnly :: !Bool
  , optGeoLookup :: !Bool
  , optNotifyAlerts :: !Bool
  }
  deriving (Eq, Show)

mergeSessionOptions :: Session -> Options -> Options
mergeSessionOptions s o =
  o { optFlowResolveDns = sessionFlowResolveDns s
    , optFlowShowApps = sessionFlowShowApps s
    , optFlowInboundOnly = sessionFlowInboundOnly s
    , optGeoLookup = sessionGeoLookup s
    , optNotifyAlerts = sessionNotifyAlerts s
    , optRouterHost = pickEmpty (optRouterHost o) (sessionRouterHost s)
    , optSnmpWanIf = pickEmpty (optSnmpWanIf o) (sessionSnmpWanIf s)
    , optSnmpCommunity =
        if null (sessionSnmpCommunity s)
          then optSnmpCommunity o
          else sessionSnmpCommunity s
    , optInterface = if null (optInterface o) then sessionInterface s else optInterface o
    , optFavorites = if null (optFavorites o) then sessionFavorites s else optFavorites o
    , optBlocklist = if null (optBlocklist o) then sessionBlocklist s else optBlocklist o
    }
  where
    pickEmpty cli sess = if null cli then sess else cli

parseOptions :: IO Options
parseOptions =
  execParser
    ( info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Monitor network interfaces, bandwidth, and TCP connections"
            <> header "NTSentinel - network monitoring tool"
        )
    )

options :: Parser Options
options =
  Options
    <$> commandParser
    <*> intervalOption
    <*> countOption
    <*> interfaceOption
    <*> stateOption
    <*> limitOption
    <*> targetOption
    <*> portOption
    <*> pingCountOption
    <*> favoritesOption
    <*> blocklistOption
    <*> emitAlertOption
    <*> loggingOption
    <*> routerHostOption
    <*> snmpCommunityOption
    <*> snmpWanIfOption
    <*> pure True
    <*> pure True
    <*> pure False
    <*> pure True
    <*> pure False

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "interfaces"
        ( info (pure Interfaces) (progDesc "Show interface byte and packet counters"))
        <> command
          "if"
          ( info (pure Interfaces) (progDesc "Alias for interfaces"))
        <> command
          "watch"
          ( info (pure Watch) (progDesc "Live bandwidth monitor with spark bars"))
        <> command
          "w"
          ( info (pure Watch) (progDesc "Alias for watch"))
        <> command
          "connections"
          ( info (pure Connections) (progDesc "Show active TCP connections"))
        <> command
          "conn"
          ( info (pure Connections) (progDesc "Alias for connections"))
        <> command
          "dashboard"
          ( info (pure Dashboard) (progDesc "One-screen network status overview"))
        <> command
          "dash"
          ( info (pure Dashboard) (progDesc "Alias for dashboard"))
        <> command
          "netview"
          ( info (pure NetView) (progDesc "Live visual network topology map"))
        <> command
          "nv"
          ( info (pure NetView) (progDesc "Alias for netview"))
        <> command
          "flow"
          ( info (pure Flow) (progDesc "Live traffic flow: what you emit and to whom"))
        <> command
          "emit"
          ( info (pure Flow) (progDesc "Alias for flow"))
        <> command
          "trace"
          ( info (pure Trace) (progDesc "Traceroute to a host"))
        <> command
          "dns"
          ( info (pure DnsTrace) (progDesc "DNS record trace for a domain"))
        <> command
          "listen"
          ( info (pure Listen) (progDesc "Show local listening ports and processes"))
        <> command
          "apps"
          ( info (pure Apps) (progDesc "Live per-app traffic summary"))
        <> command
          "mission"
          ( info (pure Mission) (progDesc "All-in-one mission control view"))
        <> command
          "intel"
          ( info (pure Intel) (progDesc "Public IP, Wi-Fi, VPN snapshot"))
        <> command
          "report"
          ( info (pure Report) (progDesc "Export a session report file"))
        <> command
          "mping"
          ( info (pure MultiPing) (progDesc "Multi-target ping board"))
        <> command
          "router"
          ( info (pure Router) (progDesc "Live WAN traffic through your router (SNMP)"))
        <> command
          "wan"
          ( info (pure Router) (progDesc "Alias for router"))
        <> command
          "inbound"
          ( info (pure Inbound) (progDesc "Remote hosts connected to your listening ports"))
        <> command
          "health"
          ( info (pure Health) (progDesc "Network health score snapshot"))
        <> command
          "lanmap"
          ( info (pure LanMap) (progDesc "LAN device map from ARP/neighbor table"))
        <> command
          "ping"
          ( info (pure Ping) (progDesc "Ping a host and show latency stats"))
        <> command
          "top"
          ( info (pure TopHosts) (progDesc "Top remote hosts by connection count"))
        <> command
          "port"
          ( info (pure PortCheck) (progDesc "Check if a TCP port is open"))
        <> command
          "menu"
          ( info (pure Menu) (progDesc "Interactive menu with ASCII art"))
    )

intervalOption :: Parser Double
intervalOption =
  option
    auto
    ( long "interval"
        <> short 'i'
        <> metavar "SECS"
        <> value 1.0
        <> showDefault
        <> help "Refresh interval in seconds (watch mode)"
    )

countOption :: Parser Int
countOption =
  option
    auto
    ( long "count"
        <> short 'c'
        <> metavar "N"
        <> value 0
        <> showDefault
        <> help "Number of watch refreshes (0 = run until Ctrl-C)"
    )

interfaceOption :: Parser [String]
interfaceOption =
  many
    ( strOption
        ( long "interface"
            <> short 'I'
            <> metavar "IFACE"
            <> help "Limit to specific interface(s), e.g. en0"
        )
    )

stateOption :: Parser String
stateOption =
  strOption
    ( long "state"
        <> short 's'
        <> metavar "STATE"
        <> value "ESTABLISHED"
        <> showDefault
        <> help "Filter connections by state"
    )

limitOption :: Parser Int
limitOption =
  option
    auto
    ( long "limit"
        <> short 'l'
        <> metavar "N"
        <> value 50
        <> showDefault
        <> help "Max rows to display"
    )

targetOption :: Parser String
targetOption =
  strOption
    ( long "host"
        <> short 'H'
        <> metavar "HOST"
        <> value "8.8.8.8"
        <> showDefault
        <> help "Target host for ping/port commands"
    )

portOption :: Parser Int
portOption =
  option
    auto
    ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 443
        <> showDefault
        <> help "Target port for port check"
    )

pingCountOption :: Parser Int
pingCountOption =
  option
    auto
    ( long "pings"
        <> metavar "N"
        <> value 4
        <> showDefault
        <> help "Number of ping packets to send"
    )

favoritesOption :: Parser [String]
favoritesOption =
  many
    ( strOption
        ( long "favorite"
            <> short 'F'
            <> metavar "HOST"
            <> help "Favorite host for multi-ping board"
        )
    )

blocklistOption :: Parser [String]
blocklistOption =
  many
    ( strOption
        ( long "block"
            <> metavar "HOST"
            <> help "Blocklisted remote host prefix"
        )
    )

emitAlertOption :: Parser Double
emitAlertOption =
  option
    auto
    ( long "emit-alert"
        <> metavar "BPS"
        <> value 524288
        <> showDefault
        <> help "Emit rate alert threshold in bytes/sec"
    )

loggingOption :: Parser Bool
loggingOption =
  switch
    ( long "log"
        <> help "Append session events to ~/.config/new-tower/sessions.log"
    )

routerHostOption :: Parser String
routerHostOption =
  strOption
    ( long "router"
        <> metavar "HOST"
        <> value ""
        <> showDefault
        <> help "Router IP for SNMP WAN monitor (default: auto-detect gateway)"
    )

snmpCommunityOption :: Parser String
snmpCommunityOption =
  strOption
    ( long "snmp-community"
        <> metavar "COMMUNITY"
        <> value "public"
        <> showDefault
        <> help "SNMP v2c community string for router polling"
    )

snmpWanIfOption :: Parser String
snmpWanIfOption =
  strOption
    ( long "snmp-wan-if"
        <> metavar "NAME"
        <> value ""
        <> showDefault
        <> help "WAN interface name hint (e.g. wan, ppp0, eth0)"
    )

runCommand :: Options -> IO ()
runCommand opts =
  case optCommand opts of
    Menu -> pure ()
    Interfaces -> runInterfaces opts
    Watch -> runWatch opts
    Connections -> runConnections opts
    Dashboard -> runDashboard opts
    NetView -> runNetView opts
    Flow -> runFlow opts
    Trace -> runTrace opts
    DnsTrace -> runDnsTrace opts
    Listen -> runListen opts
    Apps -> runApps opts
    Mission -> runMission opts
    Intel -> runIntel opts
    Report -> runReport opts
    MultiPing -> runMultiPing opts
    Router -> runRouter opts
    Inbound -> runInbound opts
    Health -> runHealth opts
    LanMap -> runLanMap opts
    Ping -> runPing opts
    TopHosts -> runTopHosts opts
    PortCheck -> runPortCheck opts

runInterfaces :: Options -> IO ()
runInterfaces opts = do
  stats <- filterByInterface opts <$> readInterfaceStats
  putStrLn headerRow
  mapM_ (putStrLn . renderInterfaceRow) stats
  putStrLn ""
  putStrLn $ "Platform: " ++ os ++ "  |  Interfaces: " ++ show (length stats)

runWatch :: Options -> IO ()
runWatch opts =
  bracketTerminal $ do
    gateway <- defaultGateway
    baseline <- indexByName <$> readInterfaceStats
    loop baseline 0 0 0 Map.empty [] gateway 0
  where
    loop prevMap n maxDown maxUp histories pingHist gateway stormTicks = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      pingHist' <- sampleGatewayPing n pingHist gateway
      current <- indexByName <$> readInterfaceStats
      let names = sort (Map.keys (Map.union prevMap current))
          filteredNames =
            case optInterface opts of
              [] -> names
              wanted -> filter (`elem` wanted) names
          samples =
            [ (name, samplePair prevMap current name)
            | name <- filteredNames
            ]
          rates = [(name, (down, up)) | (name, (down, up, _, _)) <- samples]
          peakDown = maximum (maxDown : map (fst . snd) rates ++ [0])
          peakUp = maximum (maxUp : map (snd . snd) rates ++ [0])
          totalInPkts = sum [inPkts | (_, (_, _, inPkts, _)) <- samples]
          totalOutPkts = sum [outPkts | (_, (_, _, _, outPkts)) <- samples]
          totalRate = sum [down + up | (_, (down, up)) <- rates]
          peakRate = max peakDown peakUp
          stormNow = detectTrafficStorm totalRate peakRate
          stormLeft = if stormNow then 10 else max 0 (stormTicks - 1)
          inStorm = stormLeft > 0
          newHistories =
            Map.fromList
              [ ( name
                , pushSample historyWidth (down + up) (Map.findWithDefault [] name histories)
                )
              | (name, (down, up, _, _)) <- samples
              ]
          latestPing =
            case pingHist' of
              (ms : _) -> show (round ms) ++ " ms"
              _ -> "n/a"
          panelBody =
            [ ""
            , headerWatchRow colorOn
            ]
              ++ map (renderWatchRow colorOn n peakDown peakUp newHistories) rates
              ++ [ ""
                 , styleTableHeader colorOn ("  ◀ " ++ packetTicker n totalInPkts totalOutPkts ++ " ▶")
                 , ""
                 , "  "
                    ++ styleTableHeader colorOn "Gateway latency "
                    ++ styleTrend colorOn (oscilloscope pingHist' oscilloscopeWidth)
                    ++ "  "
                    ++ latestPing
                 , stylePlatform colorOn os
                 , ""
                 ]
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) inStorm
      renderPanelStorm inStorm n "LIVE BANDWIDTH MONITOR" panelBody
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop current (n + 1) peakDown peakUp newHistories pingHist' gateway stormLeft

    sampleGatewayPing tick hist target =
      if tick `mod` 3 /= 0
        then pure hist
        else
          case target of
            Nothing -> pure hist
            Just host -> do
              result <- pingHost host 1
              pure $
                case result of
                  Right r -> pushSample oscilloscopeWidth (pingAvgMs r) hist
                  _ -> hist

    samplePair prevMap curMap name =
      case (Map.lookup name prevMap, Map.lookup name curMap) of
        (Just prev, Just cur) ->
          let delta = diffStats prev cur
              (down, up) = ratesFromDiff delta (optInterval opts)
           in
            ( down
            , up
            , packetRate (inPackets delta)
            , packetRate (outPackets delta)
            )
        _ -> (0, 0, 0, 0)

    packetRate pkts =
      round (fromIntegral pkts / optInterval opts :: Double) :: Int64

    renderWatchRow colorOn tick peakDown peakUp histories (name, (down, up)) =
      let hist = Map.findWithDefault [] name histories
          downBar = blockBar tick down peakDown 12
          upBar = blockBar (tick + 2) up peakUp 12
          trend = sparkline hist historyWidth
       in intercalate
            "  "
            [ padR 8 name
            , styleRate colorOn down peakDown (padL 10 (formatRate down))
            , styleRate colorOn up peakUp (padL 10 (formatRate up))
            , styleBar colorOn down peakDown (padR 12 downBar)
            , styleBar colorOn up peakUp (padR 12 upBar)
            , styleTrend colorOn trend
            ]

runNetView :: Options -> IO ()
runNetView opts =
  bracketTerminal $ do
    gateway <- defaultGateway
    baseline <- indexByName <$> readInterfaceStats
    loop (Just baseline) [] 0 gateway
  where
    loop prevMap pingHist n gateway = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      pingHist' <- sampleGatewayPing n pingHist gateway
      current <- indexByName <$> readInterfaceStats
      snap <-
        gatherNetSnapshot
          (optInterface opts)
          (optState opts)
          (min 8 (optLimit opts))
          (optInterval opts)
          prevMap
      let snap' = updatePingHistory snap pingHist'
          totalRate = sum [d + u | (_, d, u) <- snapInterfaces snap']
          peakRate = maxRate snap'
          inStorm = detectTrafficStorm totalRate peakRate
          body = renderNetViewLines n colorOn snap'
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) inStorm
      renderPanelStorm inStorm n "VISUAL NETVIEW" body
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop (Just current) pingHist' (n + 1) gateway

    sampleGatewayPing tick hist target =
      if tick `mod` 3 /= 0
        then pure hist
        else
          case target of
            Nothing -> pure hist
            Just host -> do
              result <- pingHost host 1
              pure $
                case result of
                  Right r -> pushSample 24 (pingAvgMs r) hist
                  _ -> hist

runFlow :: Options -> IO ()
runFlow opts =
  bracketTerminal $ do
    gateway <- defaultGateway
    loop Nothing Nothing 0 gateway False
  where
    loop prevTotals prevFlows n gateway prevStorm = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      flowsRaw <- readFlows
      let flows =
            if optFlowInboundOnly opts
              then filterInboundFlows (optState opts) (optLimit opts) flowsRaw
              else flowsRaw
          (emits, totals) =
            computeHostEmits
              (optInterval opts)
              (optState opts)
              (optLimit opts)
              flows
              prevTotals
          newFlows = detectNewFlows prevFlows flows
          blocked = blockedHosts (optBlocklist opts) flows
          alerts =
            renderAlertLines colorOn newFlows blocked (suspiciousFlows flows)
          apps =
            if optFlowShowApps opts
              then renderAppLines n colorOn (computeAppEmits emits)
              else []
          totalEmit = sum (map emitTxRate emits)
          inStorm =
            thresholdExceeded (optEmitAlert opts) flows totalEmit
              || detectTrafficStorm totalEmit (maximum (1 : map emitTxRate emits))
      dnsLines <-
        if optFlowResolveDns opts
          then mapM enrichDns (take 5 emits)
          else
            if optGeoLookup opts
              then mapM enrichGeo (take 5 emits)
              else pure []
      let body =
            renderFlowPanel n colorOn (optInterval opts) emits flows
              ++ renderFlowExtras alerts apps dnsLines
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) inStorm
      renderPanelStorm inStorm n "TRAFFIC FLOW MONITOR" body
      mPing <- sampleGatewayMs gateway
      conns <- length <$> readConnections
      recordMetric totalEmit conns mPing
      when (optLogging opts) $
        appendSessionLog
          ( "flow refresh "
              ++ show (n + 1)
              ++ " emit="
              ++ show (round totalEmit)
              ++ "B/s hosts="
              ++ show (length emits)
          )
      when (optNotifyAlerts opts && inStorm && not prevStorm) $
        notifyUser "NT Sentinel" ("High emit rate: " ++ show (round totalEmit) ++ " B/s")
      when (inStorm && not prevStorm) $ appendTimeline "emit storm detected"
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop (Just totals) (Just flows) (n + 1) gateway inStorm

    enrichDns e = do
      label <- lookupHostLabel (emitHost e)
      pure
        ( "  "
            ++ padR 18 (emitHost e)
            ++ " -> "
            ++ take 40 label
        )

    enrichGeo e = do
      geo <- lookupGeo (emitHost e)
      pure
        ( "  "
            ++ padR 18 (emitHost e)
            ++ " -> "
            ++ take 40 (maybe "?" formatGeoShort geo)
        )

    sampleGatewayMs :: Maybe String -> IO (Maybe Double)
    sampleGatewayMs Nothing = pure Nothing
    sampleGatewayMs (Just host) = do
      result <- pingHost host 1
      pure (either (const Nothing) (Just . pingAvgMs) result)

runConnections :: Options -> IO ()
runConnections opts = do
  conns <- readConnections
  let filtered =
        take (optLimit opts) $
          filter ((== optState opts) . connState) conns
  putStrLn connHeaderRow
  forM_ filtered $ \c ->
    putStrLn $
      intercalate
        "  "
        [ padR 6 (connProto c)
        , padL 6 (show (connRecvQ c))
        , padL 6 (show (connSendQ c))
        , padR 24 (connLocal c)
        , padR 24 (connForeign c)
        , connState c
        ]
  putStrLn ""
  putStrLn $
    "Showing "
      ++ show (length filtered)
      ++ " "
      ++ optState opts
      ++ " connection(s)"

runDashboard :: Options -> IO ()
runDashboard opts =
  bracketTerminal $ do
    gateway <- defaultGateway
    stats <- filterByInterface opts <$> readInterfaceStats
    baseline <- indexByName <$> readInterfaceStats
    pingHist <-
      foldM
        ( \hist tick -> do
            hist' <-
              if tick `mod` 3 == 0
                then sampleDashboardPing gateway hist
                else pure hist
            clearScreen
            renderHeaderAnimated tick
            renderPanel
              "NETWORK DASHBOARD"
              [ ""
              , if tick `mod` 2 == 0
                  then "  " ++ spinnerAt tick ++ "  Sampling interface traffic..."
                  else "  " ++ spinnerAt tick ++ "  Scanning active connections..."
              , "  " ++ pulseLive tick ++ "  Building live snapshot"
              , if null hist'
                  then ""
                  else
                    "  "
                      ++ "Gateway latency "
                      ++ oscilloscope hist' oscilloscopeWidth
                      ++ "  sampling..."
              , ""
              ]
            threadDelay 100000
            pure hist'
        )
        []
        [0 .. 9]
    current <- indexByName <$> readInterfaceStats
    conns <- readConnections
    hosts <- topRemoteHosts 5 (optState opts)
    pingResult <- maybe (pure Nothing) (\g -> Just <$> pingHost g 3) gateway
    let connCount = length (filter ((== optState opts) . connState) conns)
        peakDown = maxDownRate stats baseline current
        peakUp = maxUpRate stats baseline current
        totalRate = sum [downRate s + upRate s | s <- take 3 stats]
        inStorm = detectTrafficStorm totalRate (max peakDown peakUp)
        ifaceLines =
          [ "  "
              ++ padR 8 (ifaceName s)
              ++ "  down "
              ++ padL 10 (formatRate (downRate s))
              ++ "  "
              ++ blockBar 0 (downRate s) peakDown 10
              ++ "  up "
              ++ padL 10 (formatRate (upRate s))
              ++ "  "
              ++ blockBar 0 (upRate s) peakUp 10
          | s <- take 3 stats
          ]
        downRate s =
          case (Map.lookup (ifaceName s) baseline, Map.lookup (ifaceName s) current) of
            (Just a, Just b) -> fst (ratesFromDiff (diffStats a b) 1)
            _ -> 0
        upRate s =
          case (Map.lookup (ifaceName s) baseline, Map.lookup (ifaceName s) current) of
            (Just a, Just b) -> snd (ratesFromDiff (diffStats a b) 1)
            _ -> 0
        hostLines =
          [ "  " ++ padR 22 addr ++ "  " ++ show n ++ " conn(s)"
          | (addr, n) <- hosts
          ]
        pingScopeLine =
          case pingHist of
            [] -> ""
            _ ->
              "  Gateway latency "
                ++ oscilloscope pingHist oscilloscopeWidth
                ++ "  (live samples)"
        pingLine =
          case pingResult of
            Nothing -> "  Gateway ping: unavailable"
            Just (Left err) -> "  Gateway ping: " ++ take 50 err
            Just (Right r) ->
              "  Gateway "
                ++ pingTarget r
                ++ ": avg "
                ++ show (round (pingAvgMs r))
                ++ " ms, loss "
                ++ show (round (pingLossPct r))
                ++ "%"
    intel <- gatherIntel
    clearScreen
    renderHeaderAnimated 10
    renderPanelStorm inStorm 10
      "NETWORK DASHBOARD"
      ( [ ""
        , "  Active " ++ optState opts ++ " connections : " ++ show connCount
        , "  Platform                              : " ++ os
        , ""
        , "  Interface rates (last 1 sec):"
        ]
          ++ if null ifaceLines then ["  (no interfaces matched)"] else ifaceLines
          ++ [ ""
             , "  Top remote hosts:"
             ]
          ++ if null hostLines then ["  (none)"] else hostLines
          ++ [ "" ]
          ++ (if null pingScopeLine then [] else [pingScopeLine])
          ++ [ pingLine ]
          ++ renderIntelLines intel
          ++ [ "" ]
      )
  where
    sampleDashboardPing target hist =
      case target of
        Nothing -> pure hist
        Just host -> do
          result <- pingHost host 1
          pure $
            case result of
              Right r -> pushSample oscilloscopeWidth (pingAvgMs r) hist
              _ -> hist

    maxDownRate stats baseline current =
      maximum $
        1 : [ sampleDown s | s <- take 3 stats ]
      where
        sampleDown s =
          case (Map.lookup (ifaceName s) baseline, Map.lookup (ifaceName s) current) of
            (Just a, Just b) -> fst (ratesFromDiff (diffStats a b) 1)
            _ -> 0

    maxUpRate stats baseline current =
      maximum $
        1 : [ sampleUp s | s <- take 3 stats ]
      where
        sampleUp s =
          case (Map.lookup (ifaceName s) baseline, Map.lookup (ifaceName s) current) of
            (Just a, Just b) -> snd (ratesFromDiff (diffStats a b) 1)
            _ -> 0

runPing :: Options -> IO ()
runPing opts = do
  let host = optTarget opts
  putStrLn ("Pinging " ++ host ++ " (" ++ show (optPingCount opts) ++ " packets)...")
  putStrLn ""
  result <- pingHost host (optPingCount opts)
  case result of
    Left err -> putStrLn err
    Right r -> do
      putStrLn $ intercalate "  " ["TARGET", "TX", "RX", "LOSS", "MIN", "AVG", "MAX"]
      putStrLn $
        intercalate
          "  "
          [ padR 16 (pingTarget r)
          , padL 4 (show (pingTransmitted r))
          , padL 4 (show (pingReceived r))
          , padL 6 (show (round (pingLossPct r)) ++ "%")
          , padL 8 (show (pingMinMs r) ++ "ms")
          , padL 8 (show (pingAvgMs r) ++ "ms")
          , padL 8 (show (pingMaxMs r) ++ "ms")
          ]

runTopHosts :: Options -> IO ()
runTopHosts opts = do
  hosts <- topRemoteHosts (optLimit opts) (optState opts)
  putStrLn $ intercalate "  " [padR 24 "REMOTE HOST", "CONNECTIONS"]
  mapM_
    ( \(addr, n) ->
        putStrLn $ intercalate "  " [padR 24 addr, padL 12 (show n)]
    )
    hosts
  putStrLn ""
  putStrLn $
    "Top "
      ++ show (length hosts)
      ++ " remote hosts ("
      ++ optState opts
      ++ ")"

runPortCheck :: Options -> IO ()
runPortCheck opts = do
  let host = optTarget opts
      port = optPort opts
  putStrLn ("Checking " ++ host ++ ":" ++ show port ++ " ...")
  result <- portCheck host port
  dns <- dnsLookup host
  case result of
    Right True -> putStrLn ("  OPEN   " ++ host ++ ":" ++ show port)
    Right False -> putStrLn ("  CLOSED " ++ host ++ ":" ++ show port)
    Left err -> putStrLn ("  FAIL   " ++ err)
  case dns of
    Right ips -> putStrLn ("  DNS    " ++ unwords ips)
    Left _ -> pure ()

runTrace :: Options -> IO ()
runTrace opts = do
  lines' <- traceRoute (optTarget opts)
  clearScreen
  renderHeader
  renderPanel ("TRACEROUTE -> " ++ optTarget opts) lines'

runDnsTrace :: Options -> IO ()
runDnsTrace opts = do
  lines' <- dnsTrace (optTarget opts)
  clearScreen
  renderHeader
  renderPanel ("DNS TRACE -> " ++ optTarget opts) lines'

runListen :: Options -> IO ()
runListen opts = do
  ls <- readListeners
  clearScreen
  renderHeader
  renderPanel "LISTENING PORTS" (renderListenerLines ls)

runApps :: Options -> IO ()
runApps opts =
  bracketTerminal $ do
    loop Nothing 0
  where
    loop prevTotals n = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      flows <- readFlows
      let (emits, totals) =
            computeHostEmits (optInterval opts) (optState opts) (optLimit opts) flows prevTotals
          body =
            [ "" ]
              ++ renderAppLines n colorOn (computeAppEmits emits)
              ++ [ "" ]
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) False
      renderPanel "APP TRAFFIC SUMMARY" body
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop (Just totals) (n + 1)

runMission :: Options -> IO ()
runMission opts =
  bracketTerminal $ do
    intel <- gatherIntel
    loop Nothing Nothing 0 intel
  where
    loop prevTotals prevFlows n intel = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      flows <- readFlows
      let (emits, totals) =
            computeHostEmits (optInterval opts) (optState opts) 8 flows prevTotals
          alerts =
            renderAlertLines colorOn (detectNewFlows prevFlows flows) (blockedHosts (optBlocklist opts) flows) (suspiciousFlows flows)
          body = renderMissionPanel n colorOn (optInterval opts) intel emits flows alerts
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) False
      renderPanel "MISSION CONTROL" body
      when (optLogging opts) $ appendSessionLog ("mission refresh " ++ show (n + 1))
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop (Just totals) (Just flows) (n + 1) intel

runIntel :: Options -> IO ()
runIntel _opts = do
  intel <- gatherIntel
  clearScreen
  renderHeader
  renderPanel "NETWORK INTEL" (renderIntelLines intel)

runReport :: Options -> IO ()
runReport opts = do
  intel <- gatherIntel
  flows <- readFlows
  let (emits, _) = computeHostEmits 1 (optState opts) (optLimit opts) flows Nothing
      alerts = renderAlertLines False [] [] []
  path <- writeSessionReport intel emits alerts
  putStrLn ("Report written to: " ++ path)

runMultiPing :: Options -> IO ()
runMultiPing opts =
  bracketTerminal $
    let targets =
          if null (optFavorites opts)
            then ["8.8.8.8", "1.1.1.1"]
            else optFavorites opts
     in loop targets 0
  where
    loop targets n = do
      threadDelay (round (optInterval opts * 1000000))
      colorOn <- useColor
      rows <- mapM pingRow targets
      let body = [""] ++ rows ++ [""]
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) False
      renderPanel "MULTI-PING BOARD" body
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop targets (n + 1)

    pingRow host = do
      result <- pingHost host 1
      pure $
        case result of
          Right r ->
            "  "
              ++ padR 20 host
              ++ padL 8 (show (round (pingAvgMs r)) ++ " ms")
              ++ padL 8 (show (round (pingLossPct r)) ++ "% loss")
          Left err -> "  " ++ padR 20 host ++ "  " ++ take 40 err

runRouter :: Options -> IO ()
runRouter opts = do
  ok <- snmpAvailable
  if not ok
    then do
      renderMessage "  SNMP tools not found. Install net-snmp (snmpget/snmpwalk)."
      renderMessage "  On macOS: brew install net-snmp"
    else do
      host <- resolveRouterHost opts
      case host of
        Nothing -> renderMessage "  Could not determine router IP. Set router_host in session.conf or use --router."
        Just routerIp -> do
          let community = optSnmpCommunity opts
              hint =
                if null (optSnmpWanIf opts)
                  then Nothing
                  else Just (optSnmpWanIf opts)
          wanResult <- discoverWanInterface routerIp community hint
          case wanResult of
            Left err -> renderMessage ("  " ++ err)
            Right wan ->
              bracketTerminal $ do
                baseline <- readWanOctets routerIp community wan
                loop baseline 0 0 0
              where
                loop prev n peakDown peakUp = do
                  threadDelay (round (optInterval opts * 1000000))
                  colorOn <- useColor
                  current <- readWanOctets routerIp community wan
                  let (down, up) = wanRates prev current (optInterval opts)
                      peakDown' = maximum (peakDown : [down, 0])
                      peakUp' = maximum (peakUp : [up, 0])
                      (totalIn, totalOut) =
                        case current of
                          Just (i, o) -> (i, o)
                          Nothing -> (0, 0)
                      body =
                        renderRouterPanel
                          n
                          colorOn
                          routerIp
                          wan
                          down
                          up
                          totalIn
                          totalOut
                          peakDown'
                          peakUp'
                  clearScreen
                  renderHeaderAnimated n
                  renderLiveBanner n (optInterval opts) (n + 1) False
                  renderPanel "ROUTER WAN TRAFFIC (SNMP)" body
                  when (optLogging opts) $
                    appendSessionLog
                      ( "router wan in="
                          ++ show (round down)
                          ++ "B/s out="
                          ++ show (round up)
                          ++ "B/s"
                      )
                  when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
                  loop current (n + 1) peakDown' peakUp'

resolveRouterHost :: Options -> IO (Maybe String)
resolveRouterHost opts =
  if not (null (optRouterHost opts))
    then pure (Just (optRouterHost opts))
    else defaultGateway

runInbound :: Options -> IO ()
runInbound opts = do
  flows <- FlowLib.readFlows
  let inbound = filterInboundFlows (optState opts) (optLimit opts) flows
  clearScreen
  renderHeader
  rows <- renderInboundPanel (optFlowResolveDns opts) (optGeoLookup opts) inbound
  let body =
        [ "  Remote hosts with connections TO your machine (inbound):" ]
          ++ rows
  renderPanel "INBOUND WATCHERS" body
  putStrLn $
    "Showing "
      ++ show (length inbound)
      ++ " inbound "
      ++ optState opts
      ++ " connection(s)."

runInboundLive :: Options -> IO ()
runInboundLive opts =
  bracketTerminal $ loop 0
  where
    loop n = do
      threadDelay (round (optInterval opts * 1000000))
      flows <- FlowLib.readFlows
      let inbound = filterInboundFlows (optState opts) (optLimit opts) flows
      rows <- renderInboundPanel (optFlowResolveDns opts) (optGeoLookup opts) inbound
      let body =
            [ "  Live inbound watchers — remote hosts on your listening ports" ]
              ++ rows
      clearScreen
      renderHeaderAnimated n
      renderLiveBanner n (optInterval opts) (n + 1) False
      renderPanel "INBOUND WATCHERS (LIVE)" body
      when (optLogging opts) $
        appendSessionLog ("inbound live refresh " ++ show (n + 1) ++ " rows=" ++ show (length inbound))
      when (optCount opts > 0 && n + 1 >= optCount opts) exitSuccess
      loop (n + 1)

runHealth :: Options -> IO ()
runHealth _opts = do
  gateway <- defaultGateway
  pingInfo <-
    case gateway of
      Nothing -> pure (Nothing, Nothing)
      Just host -> do
        result <- pingHost host 4
        pure $
          case result of
            Right r -> (Just (pingAvgMs r), Just (pingLossPct r))
            Left _ -> (Nothing, Nothing)
  conns <- readConnections
  stats <- readInterfaceStats
  let ifaceErrors = sum [fromIntegral (inErrors s + outErrors s) | s <- stats]
      report = computeHealth (fst pingInfo) (snd pingInfo) (length conns) ifaceErrors
  clearScreen
  renderHeader
  renderPanel "NETWORK HEALTH" (renderHealthLines report)

runLanMap :: Options -> IO ()
runLanMap _opts = do
  devs <- readLanDevices
  clearScreen
  renderHeader
  renderPanel "LAN MAP" (renderLanMapLines devs)
  putStrLn ("Devices found: " ++ show (length devs))

filterByInterface :: Options -> [InterfaceStats] -> [InterfaceStats]
filterByInterface opts stats =
  let names = optInterface opts
   in if null names
        then sortBy (comparing ifaceName) stats
        else filter ((`elem` names) . ifaceName) stats

indexByName :: [InterfaceStats] -> Map String InterfaceStats
indexByName = Map.fromList . map (\s -> (ifaceName s, s))

headerRow :: String
headerRow =
  intercalate
    "  "
    [ padR 8 "IFACE"
    , padL 6 "MTU"
    , padL 12 "IN-BYTES"
    , padL 12 "OUT-BYTES"
    , padL 10 "IN-PKTS"
    , padL 10 "OUT-PKTS"
    , padL 8 "IN-ERR"
    , padL 8 "OUT-ERR"
    ]

headerWatchRow :: Bool -> String
headerWatchRow colorOn =
  let cols =
        intercalate
          "  "
          [ padR 8 "IFACE"
          , padL 10 "DOWN"
          , padL 10 "UP"
          , padR 12 "DOWN BAR"
          , padR 12 "UP BAR"
          , padR historyWidth "TREND"
          ]
   in styleTableHeader colorOn cols

renderInterfaceRow :: InterfaceStats -> String
renderInterfaceRow s =
  intercalate
    "  "
    [ padR 8 (ifaceName s)
    , padL 6 (show (ifaceMtu s))
    , padL 12 (formatBytes (inBytes s))
    , padL 12 (formatBytes (outBytes s))
    , padL 10 (show (inPackets s))
    , padL 10 (show (outPackets s))
    , padL 8 (show (inErrors s))
    , padL 8 (show (outErrors s))
    ]

connHeaderRow :: String
connHeaderRow =
  intercalate
    "  "
    [ padR 6 "PROTO"
    , padL 6 "RECV-Q"
    , padL 6 "SEND-Q"
    , padR 24 "LOCAL"
    , padR 24 "FOREIGN"
    , "STATE"
    ]
