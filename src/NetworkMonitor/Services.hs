module NetworkMonitor.Services (serviceName) where

import Data.List (lookup)

serviceName :: Int -> String
serviceName port =
  case lookup port knownServices of
    Just name -> name
    Nothing -> show port

knownServices :: [(Int, String)]
knownServices =
  [ (20, "FTP")
  , (22, "SSH")
  , (53, "DNS")
  , (80, "HTTP")
  , (443, "HTTPS")
  , (445, "SMB")
  , (465, "SMTPS")
  , (587, "SMTP")
  , (993, "IMAPS")
  , (995, "POP3S")
  , (3306, "MySQL")
  , (3389, "RDP")
  , (5223, "iMessage")
  , (5432, "PostgreSQL")
  , (5900, "VNC")
  , (6379, "Redis")
  , (8080, "HTTP-ALT")
  , (8443, "HTTPS-ALT")
  ]
