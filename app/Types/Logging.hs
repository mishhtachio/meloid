{- | Logging helpers used by the UI event stream. The formatting
stays here so all log output is consistent.
-}
module Types.Logging (
  formatLog,
  logEv,
  logInfo,
  logWarn,
  logError,
  logDebug,
) where

import Brick.BChan (BChan, writeBChan)
import Data.Time
import Text.Printf (printf)
import Types.Core

-- | Format a log line with a timestamp and source label.
formatLog :: String -> String -> IO String
formatLog from msg = do
  timestamp <- getZonedTime
  let timeFormat = "%H:%M:%S"
      timeStr = formatTime defaultTimeLocale timeFormat timestamp
  pure $ printf "[%s] [%s]: %s" timeStr from msg

-- | Emit a log event into the Brick channel.
logEv :: BChan (Event' a) -> LogLevel -> String -> String -> IO ()
logEv chan level from msg = do
  formatted <- formatLog from msg
  writeBChan chan (Log (level, formatted))

-- | Emit an informational log line.
logInfo :: BChan (Event' a) -> String -> String -> IO ()
logInfo chan from msg = logEv chan Info from msg

-- | Emit an error log line.
logError :: BChan (Event' a) -> String -> String -> IO ()
logError chan from msg = logEv chan Error from msg

-- | Emit a warning log line.
logWarn :: BChan (Event' a) -> String -> String -> IO ()
logWarn chan from msg = logEv chan Warn from msg

-- | Emit a debug log line.
logDebug :: BChan (Event' a) -> String -> String -> IO ()
logDebug chan from msg = logEv chan Debug from msg
