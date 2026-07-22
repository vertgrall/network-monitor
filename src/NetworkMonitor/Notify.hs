module NetworkMonitor.Notify
  ( notifyUser
  ) where

import System.Info (os)
import System.Process (readProcess)

notifyUser :: String -> String -> IO ()
notifyUser title message =
  case os of
    "darwin" ->
      readProcess
        "osascript"
        [ "-e"
        , "display notification "
            ++ quote (take 120 message)
            ++ " with title "
            ++ quote (take 40 title)
        ]
        ""
        >> pure ()
    _ -> pure ()
  where
    quote s = "\"" ++ concatMap escape s ++ "\""
    escape '"' = "\\\""
    escape c = [c]
