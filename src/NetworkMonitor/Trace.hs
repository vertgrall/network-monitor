module NetworkMonitor.Trace (traceRoute) where

import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf, isPrefixOf)
import System.Info (os)
import System.Process (readProcessWithExitCode)

traceRoute :: String -> IO [String]
traceRoute host = do
  let (cmd, args) =
        case os of
          "linux" -> ("traceroute", ["-n", "-w", "2", "-q", "1", host])
          _ -> ("traceroute", ["-n", "-w", "2", "-q", "1", host])
  (_, out, err) <- readProcessWithExitCode cmd args ""
  let ls = lines (out ++ err)
  pure $
    if null ls
      then ["  (traceroute produced no output)"]
      else map ("  " ++) (take 20 ls)

isDigitChar :: Char -> Bool
isDigitChar = isDigit
