module NetworkMonitor.DNS
  ( reverseLookup
  , dnsTrace
  , lookupHostLabel
  ) where

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

reverseLookup :: String -> IO (Maybe String)
reverseLookup ip = do
  (code, out, _) <- readProcessWithExitCode "host" [ip] ""
  pure $
    case code of
      ExitSuccess ->
        case [unwords (drop 4 (words l)) | l <- lines out, "domain name pointer" `isInfixOf` l] of
          (name : _) -> Just (trimDot name)
          _ ->
            case [last (words l) | l <- lines out, " pointer " `isInfixOf` l] of
              (name : _) -> Just (trimDot name)
              _ -> Nothing
      _ -> Nothing
  where
    trimDot s = case reverse s of
      '.' : rest -> reverse rest
      _ -> s

lookupHostLabel :: String -> IO String
lookupHostLabel ip = do
  m <- reverseLookup ip
  pure (maybe ip (take 28) m)

dnsTrace :: String -> IO [String]
dnsTrace host = do
  (_, out, err) <- readProcessWithExitCode "host" ["-a", host] ""
  let body = lines (out ++ err)
  pure $
    if null body
      then ["  (no DNS records found)"]
      else
        [ "  " ++ l
        | l <- body
        , not (null l)
        , not ("Host" `isPrefixOf` l && "not found" `isInfixOf` l)
        ]
