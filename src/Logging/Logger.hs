{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Logging.Logger
    ( LogDestination (..)
    , LogLevel (..)
    , initLogger
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

import           Control.Applicative        ((<|>))
import           Control.Monad              (void, when)
import qualified Data.ByteString.Char8      as S8
import           Data.IORef
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax as TH
import           System.IO

import           Logging.Date
import           Logging.LoggerSet
import           Logging.LogStr
import           Logging.Push

import           System.IO.Unsafe           (unsafePerformIO)

finalizeLogger :: IO ()
finalizeLogger = readIORef loggerSets >>= mapM_ rmLoggerSet

-- | Logging destination. See also `setLoggingDestination`.
data LogDestination
  = LogStdErr
  | LogStdOut
  | LogFile FilePath

-- | Log messages from other packages that use this library too, even if they did not call @initLogger@?
type LogFromAllPackages = Bool

-- | Initialise the logger. MUST only be called in the executable code (not the exposed library code)! Takes a `Bool` that decides wether to log messages from other packages that use the same library
-- and did not initalize the Logger (which should be the case for all of them!).
initLogger :: Q Exp
initLogger = [|\logAll -> setLoggingDestination $(qLocation >>= liftLoc) logAll |]


-- | Set the destination for all consequitive for logging. You should only set this once, at the beginning of the program! The default is `LogStdOut`.
setLoggingDestination :: Loc -> LogFromAllPackages -> LogDestination -> IO ()
setLoggingDestination loc logAll LogStdErr = newStderrLoggerSet defaultBufSize >>= setLoggerSet (loc_package loc) >> setAllPackagesLogger logAll LogStdErr
setLoggingDestination loc logAll LogStdOut = newStdoutLoggerSet defaultBufSize >>= setLoggerSet (loc_package loc) >> setAllPackagesLogger logAll LogStdOut
setLoggingDestination loc logAll (LogFile fp) = newFileLoggerSet defaultBufSize fp >>= setLoggerSet (loc_package loc) >> setAllPackagesLogger logAll (LogFile fp)

defaultLogPkgName :: String
defaultLogPkgName = "__default__"

setAllPackagesLogger :: Bool -> LogDestination -> IO ()
setAllPackagesLogger False _ = return ()
setAllPackagesLogger True LogStdErr = newStderrLoggerSet defaultBufSize >>= setLoggerSet defaultLogPkgName
setAllPackagesLogger True LogStdOut = newStdoutLoggerSet defaultBufSize >>= setLoggerSet defaultLogPkgName
setAllPackagesLogger True (LogFile fp) = newFileLoggerSet defaultBufSize fp >>= setLoggerSet defaultLogPkgName

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

setLoggerSet :: String -> LoggerSet -> IO ()
setLoggerSet pkgName set = modifyIORef' loggerSets (M.insert pkgName set)

-- | Set of loggers for each package
loggerSets :: IORef (M.Map String LoggerSet)
loggerSets = unsafePerformIO $ newIORef mempty
{-# NOINLINE loggerSets  #-}


-- | Log Level. Levels are sorted. Debug < Info < Warning < Error. None disables all logging. Default: Debug
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
logFun loc@(Loc _ pkg _ _ _) level msg = do
  minLevel <- readIORef minLogLevel
  when (level >= minLevel) $ do
    now <- readIORef cachedTime >>= \ch -> ch
    readIORef loggerSets >>= \sets ->
      case getLogger sets of
        Nothing | M.null sets -> error "You must call `initLogger` at the start of your application! See `Logging.Logger`."
        Nothing  -> return ()
        Just set -> pushLogStr set (defaultLogStr loc now level (toLogStr msg))
  where
    getLogger sets = M.lookup pkg sets <|> M.lookup defaultLogPkgName sets


cachedTime :: IORef (IO FormattedTime)
cachedTime = unsafePerformIO $ do
  cache <- newTimeCache simpleTimeFormat'
  newIORef cache


minLogLevel :: IORef LogLevel
minLogLevel = unsafePerformIO $ newIORef Debug
{-# NOINLINE minLogLevel  #-}

-- | Set the least logging level. Levels lower will not be logged. Log Level Order: `Debug` < `Info` < `Warning` < `Error`. `None` disables all logging. Note that the output to stderr using e.g. `logPrintError` will not
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


