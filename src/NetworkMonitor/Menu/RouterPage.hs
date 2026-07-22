module NetworkMonitor.Menu.RouterPage (routerPage) where

import NetworkMonitor.CLI (Command (Router))
import NetworkMonitor.Menu.Actions as Act (runLiveAction)
import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , persistSession
  , promptDefault
  , runMenuPage
  )
import NetworkMonitor.Session (Session (..))

routerPage :: Session -> IO Session
routerPage session =
  runMenuPage
    "ROUTER & WAN"
    session
    pageSummary
    (pageLines session)
    FooterBack
    Nothing

pageLines :: Session -> [MenuLine]
pageLines session =
  [ MenuSection "Run"
  , MenuOpt "Live WAN traffic (SNMP)" $ \s ->
      Act.runLiveAction s Router "Router monitor stopped."
  , MenuSection "SNMP settings"
  , MenuOpt ("Router IP (" ++ routerHostLabel (sessionRouterHost session) ++ ")") editRouterHost
  , MenuOpt ("Community (" ++ sessionSnmpCommunity session ++ ")") editSnmpCommunity
  , MenuOpt ("WAN interface hint (" ++ snmpWanLabel (sessionSnmpWanIf session) ++ ")") editSnmpWanIf
  ]

pageSummary :: [String]
pageSummary =
  [ "  Shows total internet traffic through your router (all devices)."
  , "  Requires SNMP enabled on the router."
  , ""
  ]

routerHostLabel :: String -> String
routerHostLabel "" = "auto gateway"
routerHostLabel x = x

snmpWanLabel :: String -> String
snmpWanLabel "" = "auto detect"
snmpWanLabel x = x

editRouterHost :: Session -> IO Session
editRouterHost s = do
  val <- promptDefault "Router IP [blank=auto gateway]: " (sessionRouterHost s)
  persistSession s {sessionRouterHost = dropWhile (== ' ') val}

editSnmpCommunity :: Session -> IO Session
editSnmpCommunity s = do
  val <- promptDefault ("SNMP community [" ++ sessionSnmpCommunity s ++ "]: ") (sessionSnmpCommunity s)
  persistSession s {sessionSnmpCommunity = if null (dropWhile (== ' ') val) then sessionSnmpCommunity s else val}

editSnmpWanIf :: Session -> IO Session
editSnmpWanIf s = do
  val <- promptDefault "WAN interface hint [blank=auto]: " (sessionSnmpWanIf s)
  persistSession s {sessionSnmpWanIf = dropWhile (== ' ') val}
