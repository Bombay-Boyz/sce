{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Core.Logger
Description : Structured logging for SCE
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Provides structured logging with severity levels for better observability.

IMPORTANT: This module now uses explicit LogLevel passing instead of global state.
Functions that need logging should accept a LogLevel parameter or use the default INFO level.
-}
module SCE.Core.Logger
  ( -- * Log Levels
    LogLevel(..)
  , LogContext(..)
    -- * Logging Functions
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logDebugWith
  , logInfoWith
  , logWarnWith
  , logErrorWith
  , withContext
  , addData
    -- * Default Log Level
  , defaultLogLevel
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (stderr, stdout, hFlush)

-- | Log severity levels
data LogLevel
  = DEBUG   -- ^ Detailed diagnostic information
  | INFO    -- ^ General informational messages
  | WARN    -- ^ Warning messages for potentially harmful situations
  | ERROR   -- ^ Error messages for serious problems
  deriving (Eq, Ord, Show)

-- | Context information for structured logging
data LogContext = LogContext
  { contextModule :: Text
  , contextData   :: [(Text, Text)]  -- Key-value pairs
  }

-- | Default log level for the application
-- This is a pure value that can be overridden by configuration
defaultLogLevel :: LogLevel
defaultLogLevel = INFO

-- | Format a log message with timestamp and context
formatLogMessage :: LogLevel -> Text -> LogContext -> Text
formatLogMessage level msg context =
  let levelStr = T.pack $ show level
      moduleStr = contextModule context
      contextStr = if null (contextData context)
                   then ""
                   else " | " <> formatContext (contextData context)
  in "[" <> levelStr <> "] " <> moduleStr <> " - " <> msg <> contextStr
  where
    formatContext :: [(Text, Text)] -> Text
    formatContext pairs = T.intercalate ", " 
      [ k <> "=" <> v | (k, v) <- pairs ]

-- | Log a debug message (uses default log level)
logDebug :: Text -> LogContext -> IO ()
logDebug = logDebugWith defaultLogLevel

-- | Log an info message (uses default log level)
logInfo :: Text -> LogContext -> IO ()
logInfo = logInfoWith defaultLogLevel

-- | Log a warning message (uses default log level)
logWarn :: Text -> LogContext -> IO ()
logWarn = logWarnWith defaultLogLevel

-- | Log an error message (always logged regardless of level)
logError :: Text -> LogContext -> IO ()
logError = logErrorWith defaultLogLevel

-- | Log a debug message with explicit minimum log level
logDebugWith :: LogLevel -> Text -> LogContext -> IO ()
logDebugWith minLevel msg context = logMessageWith minLevel DEBUG msg context

-- | Log an info message with explicit minimum log level
logInfoWith :: LogLevel -> Text -> LogContext -> IO ()
logInfoWith minLevel msg context = logMessageWith minLevel INFO msg context

-- | Log a warning message with explicit minimum log level
logWarnWith :: LogLevel -> Text -> LogContext -> IO ()
logWarnWith minLevel msg context = logMessageWith minLevel WARN msg context

-- | Log an error message with explicit minimum log level
logErrorWith :: LogLevel -> Text -> LogContext -> IO ()
logErrorWith minLevel msg context = logMessageWith minLevel ERROR msg context

-- | Internal: Log a message with the given level
logMessageWith :: LogLevel -> LogLevel -> Text -> LogContext -> IO ()
logMessageWith minLevel level msg context = do
  let shouldWrite = level >= minLevel
  when shouldWrite $ do
    timestamp <- getCurrentTime
    let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
    let formatted = "[" <> timeStr <> "] " <> formatLogMessage level msg context
    
    -- Write to appropriate stream based on level
    case level of
      ERROR -> TIO.hPutStrLn stderr formatted >> hFlush stderr
      WARN  -> TIO.hPutStrLn stderr formatted >> hFlush stderr
      _     -> TIO.putStrLn formatted >> hFlush stdout

-- | Helper to run an action with when condition
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

-- | Create a logging context with module name
withContext :: Text -> LogContext
withContext moduleName = LogContext
  { contextModule = moduleName
  , contextData = []
  }

-- | Add key-value data to context
addData :: Text -> Text -> LogContext -> LogContext
addData key value context = 
  context { contextData = contextData context ++ [(key, value)] }
