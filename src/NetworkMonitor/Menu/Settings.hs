module NetworkMonitor.Menu.Settings (settingsPage) where

import NetworkMonitor.Menu.Core
  ( MenuLine (..)
  , MenuFooter (FooterBack)
  , ifaceLabel
  , onOff
  , persistSession
  , promptDefault
  , runMenuPage
  )
import NetworkMonitor.Session
  ( Session (..)
  , defaultSession
  , parseCommaList
  , parseInterfaceInput
  , saveSession
  , showCommaList
  , showInterface
  )

settingsPage :: Session -> IO Session
settingsPage session =
  runMenuPage
    "SETTINGS"
    ["Main", "Settings"]
    session
    pageSummary
    (pageLines session)
    FooterBack
    Nothing

pageLines :: Session -> [MenuLine]
pageLines session =
  [ MenuSection "Defaults"
  , MenuOpt ("Interface (" ++ ifaceLabel (sessionInterface session) ++ ")") editInterface
  , MenuOpt ("Interval (" ++ show (sessionInterval session) ++ "s)") editInterval
  , MenuOpt ("Count (" ++ show (sessionCount session) ++ ")") editCount
  , MenuOpt ("Connection state (" ++ sessionState session ++ ")") editState
  , MenuOpt ("Limit (" ++ show (sessionLimit session) ++ ")") editLimit
  , MenuSection "Display"
  , MenuOpt ("Theme (" ++ sessionTheme session ++ ")") editTheme
  , MenuSection "Alerts & logging"
  , MenuOpt ("Favorites (" ++ showCommaList (sessionFavorites session) ++ ")") editFavorites
  , MenuOpt ("Blocklist (" ++ showCommaList (sessionBlocklist session) ++ ")") editBlocklist
  , MenuOpt ("Emit alert (" ++ show (sessionEmitAlert session) ++ " B/s)") editEmitAlert
  , MenuOpt ("Logging [" ++ onOff (sessionLogging session) ++ "]") toggleLogging
  , MenuOpt ("Notify on alerts [" ++ onOff (sessionNotifyAlerts session) ++ "]") toggleNotify
  , MenuSection "Maintenance"
  , MenuOpt "Reset to defaults" resetDefaults
  , MenuOpt "Save session (writes config now)" saveNow
  ]

pageSummary :: [String]
pageSummary =
  [ "  Global defaults for live tools and CLI."
  , "  Themes: default, cyber, minimal (breadcrumb colors)."
  , ""
  ]

editInterface :: Session -> IO Session
editInterface s = do
  val <-
    promptDefault
      ("Interface(s) [" ++ showInterface (sessionInterface s) ++ ", blank=all]: ")
      (showInterface (sessionInterface s))
  persistSession s {sessionInterface = parseInterfaceInput val}

editInterval :: Session -> IO Session
editInterval s = do
  val <- promptDefault ("Interval seconds [" ++ show (sessionInterval s) ++ "]: ") (show (sessionInterval s))
  case reads val of
    [(n, _)] | n > 0 -> persistSession s {sessionInterval = n}
    _ -> pure s

editCount :: Session -> IO Session
editCount s = do
  val <- promptDefault ("Watch count [" ++ show (sessionCount s) ++ "]: ") (show (sessionCount s))
  case reads val of
    [(n, _)] | n >= 0 -> persistSession s {sessionCount = n}
    _ -> pure s

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

editTheme :: Session -> IO Session
editTheme s = do
  val <- promptDefault "Theme [default/cyber/minimal]: " (sessionTheme s)
  let theme =
        case dropWhile (== ' ') val of
          "cyber" -> "cyber"
          "minimal" -> "minimal"
          _ -> "default"
  persistSession s {sessionTheme = theme}

editFavorites :: Session -> IO Session
editFavorites s = do
  val <- promptDefault ("Favorite hosts [" ++ showCommaList (sessionFavorites s) ++ "]: ") (showCommaList (sessionFavorites s))
  persistSession s {sessionFavorites = parseCommaList val}

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

toggleLogging :: Session -> IO Session
toggleLogging s = persistSession s {sessionLogging = not (sessionLogging s)}

toggleNotify :: Session -> IO Session
toggleNotify s = persistSession s {sessionNotifyAlerts = not (sessionNotifyAlerts s)}

resetDefaults :: Session -> IO Session
resetDefaults _ = persistSession defaultSession

saveNow :: Session -> IO Session
saveNow s = saveSession s >> pure s
