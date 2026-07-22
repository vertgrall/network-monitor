module NetworkMonitor.Menu.Help (helpPage) where

import NetworkMonitor.Art
import NetworkMonitor.Menu.Core (pause)
import NetworkMonitor.Session (Session (..))
import System.Info (os)

helpPage :: Session -> IO Session
helpPage session = do
  showHelp
  pause
  pure session

showHelp :: IO ()
showHelp = do
  clearScreen
  renderHeader
  renderPanel
    "HELP"
    [ ""
    , "  Hub menu categories:"
    , "    Live Monitor       — watch, netview, dashboard, interfaces"
    , "    Connections & Flow — emit monitor, inbound, TCP, listen, flow options"
    , "    Diagnostics        — ping, port, trace, dns, multi-ping"
    , "    Router & WAN       — SNMP total WAN traffic + router settings"
    , "    Mission & Reports  — mission control, apps, intel, export"
    , "    Settings           — global session defaults"
    , ""
    , "  Launch with no args for menu, or use CLI directly:"
    , "    network-monitor flow"
    , "    network-monitor router"
    , "    network-monitor inbound"
    , "    network-monitor --help"
    , ""
    , "  Config: ~/.config/new-tower/session.conf"
    , "  Logs:   ~/.config/new-tower/sessions.log"
    , "  Platform: " ++ os
    , ""
    ]
