{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Logging.Logger
    ( LogDestination (..)
    , LogLevel (..)
    , setLoggingDestination
    , setMinLogLevel
    , logDebug
    , logPrintDebug
    , logInfo
    , logPrintInfo
    , logWarning
    , logPrintWarning
    , logError
    , logPrintError
    , finalizeLogger
    ) where

import           Control.Monad              (when)
import qualified Data.ByteString.Char8      as S8
import           Data.IORef
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax as TH
import           System.IO

import           Logging.Date
import           Logging.LoggerSet
import           Logging.LogStr
import           Logging.Push

import           System.IO.Unsafe           (unsafePerformIO)

finalizeLogger :: IO ()
finalizeLogger = readIORef loggerSet >>= rmLoggerSet

-- | Logging destination. See also `setLoggingDestination`.
data LogDestination
  = LogStdErr
  | LogStdOut
  | LogFile FilePath

-- | Set the destination for all consequitive for logging. You should only set this once, at the beginning of the program! The default is `LogStdOut`.
setLoggingDestination :: LogDestination -> IO ()
setLoggingDestination LogStdErr    = newStderrLoggerSet defaultBufSize >>= setLoggerSet
setLoggingDestination LogStdOut    = newStdoutLoggerSet defaultBufSize >>= setLoggerSet
setLoggingDestination (LogFile fp) = newFileLoggerSet defaultBufSize fp >>= setLoggerSet

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

setLoggerSet :: LoggerSet -> IO ()
setLoggerSet set = writeIORef loggerSet set

loggerSet :: IORef LoggerSet
loggerSet = unsafePerformIO $ newStdoutLoggerSet defaultBufSize >>= newIORef
{-# NOINLINE loggerSet  #-}


-- | Log Level. Levels are sorted.
data LogLevel
  = None
  | Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

-- | Log level text.
logLevelText :: LogLevel -> T.Text
logLevelText None    = mempty
logLevelText Debug   = "DEBUG"
logLevelText Info    = "INFO "
logLevelText Warning = "WARN "
logLevelText Error   = "ERROR"

-- | Generic log function. Use TH version, e.g. `logDebug`.
logFun :: (ToLogStr msg) => Loc -> LogLevel -> msg -> IO ()
logFun _ None _ = return ()
logFun loc level msg = do
  minLevel <- readIORef minLogLevel
  when (level >= minLevel) $ do
    now <- readIORef cachedTime >>= \ch -> ch
    readIORef loggerSet >>= \set -> pushLogStr set (defaultLogStr loc now level (toLogStr msg))


cachedTime :: IORef (IO FormattedTime)
cachedTime = unsafePerformIO $ do
  cache <- newTimeCache simpleTimeFormat'
  newIORef cache


minLogLevel :: IORef LogLevel
minLogLevel = unsafePerformIO $ newIORef None
{-# NOINLINE minLogLevel  #-}

-- | Set the least logging level. Levels lower will not be logged. Log Level Order: `None` < `Debug` < `Info` < `Warning` < `Error`. Note that the output to stderr using e.g. `logPrintError` will not
-- be affected!
setMinLogLevel :: LogLevel -> IO ()
setMinLogLevel = writeIORef minLogLevel


-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = [| logFun $(qLocation >>= liftLoc) Debug |]

-- | Same as `logDebug`, but also prints the message on `stderr`.
logPrintDebug :: Q Exp
logPrintDebug = [| \txt -> hPutStrLn stderr ("DEBUG: " ++ T.unpack txt) >> hFlush stderr >> logFun $(qLocation >>= liftLoc) Debug txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelInfo' message. Usage:
--
-- > $(logInfo) "This is a info log message"
logInfo :: Q Exp
logInfo = [| logFun $(qLocation >>= liftLoc) Info |]

-- | Same as `logInfo`, but also prints the message on `stderr`.
logPrintInfo :: Q Exp
logPrintInfo = [| \txt -> hPutStrLn stderr ("INFO: " ++ T.unpack txt) >> hFlush stderr >> logFun $(qLocation >>= liftLoc) Info txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelWarning' message. Usage:
--
-- > $(logWarning) "This is a warning log message"
logWarning :: Q Exp
logWarning = [| logFun $(qLocation >>= liftLoc) Warning |]


-- | Same as `logWarning`, but also prints the message on `stderr`.
logPrintWarning :: Q Exp
logPrintWarning = [| \txt -> hPutStrLn stderr ("WARNING: " ++ T.unpack txt) >> hFlush stderr >> logFun $(qLocation >>= liftLoc) Warning txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelError' message. Usage:
--
-- > $(logError) "This is a error log message"
logError :: Q Exp
logError = [| logFun $(qLocation >>= liftLoc) Error |]

-- | Same as `logError`, but also prints the message on stderr.
logPrintError :: Q Exp
logPrintError = [| \txt -> hPutStrLn stderr ("ERROR: " ++ T.unpack txt) >> hFlush stderr >> logFun $(qLocation >>= liftLoc) Error txt |]


---- Helpers:

defaultLogStr :: Loc
              -> FormattedTime
              -> LogLevel
              -> LogStr
              -> LogStr
defaultLogStr loc time level msg =
  "[" <> (toLogStr $ logLevelText level) <> ("#" <> toLogStr time) <> "] " <> (mkTrailWs msg) <> " @(" <> toLogStr (S8.pack fileLocStr) <> ")\n"
  where
    mkTrailWs = mkMinLogStrLen defaultMinLogMsgLen
    fileLocStr = (loc_package loc) ++ ':' : (loc_module loc) ++ ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc) ++ '-' : (lineEnd loc) ++ ':' : (charEnd loc)
    line = show . fst . loc_start
    char = show . snd . loc_start
    lineEnd = show . fst . loc_end
    charEnd = show . snd . loc_end


defaultMinLogMsgLen :: Int
defaultMinLogMsgLen = 60


liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(TH.lift a)
    $(TH.lift b)
    $(TH.lift c)
    ($(TH.lift d1), $(TH.lift d2))
    ($(TH.lift e1), $(TH.lift e2))
    |]


