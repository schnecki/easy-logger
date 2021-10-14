{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module EasyLogger.Logger
    ( LogDestination (..)
    , LogLevel (..)
    , initLogger
    , initLoggerAllPackages
    , setLoggingDestination
    , setMinLogLevel
    , logAll
    , logPrintAll
    , logDebug
    , logPrintDebug
    , logInfo
    , logPrintInfo
    , logWarning
    , logPrintWarning
    , logError
    , logPrintError
    , finalizeAllLoggers
    , finalizeLogger
    , flushLoggers
    ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (when)
import qualified Data.ByteString.Char8      as S8
import           Data.IORef
import           Data.List                  (find)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax as TH
import           System.IO

import           EasyLogger.Date
import           EasyLogger.LoggerSet
import           EasyLogger.LogStr
import           EasyLogger.Push

import           System.IO.Unsafe           (unsafePerformIO)

-- | Add a @LoggerSet@ to the known loggers.
setLoggerSet :: String -> LoggerSet -> IO ()
setLoggerSet pkgName set = modifyIORef' loggerSets (M.insert pkgName set)


-- | Set of loggers. We have one @LoggerSet@ for each package.
loggerSets :: IORef (M.Map String LoggerSet)
loggerSets = unsafePerformIO $ newIORef mempty
{-# NOINLINE loggerSets  #-}


-- | Should be used to ensure all logs are completely written before the program exists. Cleans all the file descriptors. You (and also no other library) MUST NOT log after this command as all loggers
-- are deinitalized. However, you might initialize the loggers again and before restarting to log.
finalizeAllLoggers :: IO ()
finalizeAllLoggers = do
  pkgs <- map fst . M.toList <$> readIORef loggerSets
  mapM_ closeLoggerPkg pkgs


-- | Can be used to destroy your own logger (from your package) only. You MUST NOT log after this command.
finalizeLogger :: Q Exp
finalizeLogger = [| closeLogger $(qLocation >>= liftLoc)|]


-- | Flush all loggers of all packages.
flushLoggers :: IO ()
flushLoggers = readIORef loggerSets >>= mapM_ flushLoggerSet


-- | Close logger of calling package.
closeLogger :: Loc -> IO ()
closeLogger (Loc _ pkgName _ _ _) = closeLoggerPkg pkgName

-- | Close logger of package with provided package name.
closeLoggerPkg :: String -> IO ()
closeLoggerPkg pkgName = do
  refs <- readIORef loggerSets
  case M.lookup pkgName refs of
    Nothing -> return ()
    Just set@(LoggerSet Nothing _ _ _) -> deletePackage pkgName >> rmLoggerSet set
    Just set@(LoggerSet justFp _ _ _) -> do
      deletePackage pkgName
      let nrFD = length $ filter (\(LoggerSet mFp _ _ _) -> mFp == justFp) (M.elems refs)
      if nrFD <= 1
        then rmLoggerSet set
        else flushLoggerSet set

-- | Delete a package from the logger sets and with this disable all logging. Ensure the LoggerSet is deleted in case this is the last FD before calling this function!
deletePackage :: String -> IO ()
deletePackage pkg = modifyIORef' loggerSets (M.delete pkg)


-- | Logging destination. See also `setLoggingDestination`.
data LogDestination
  = LogStdErr
  | LogStdOut
  | LogFile FilePath

-- | Log messages from other packages that use this library too, even if they did not call @initLogger@?
type LogFromAllPackages = Bool

-- | Initialise the logger. MUST only be called in the executable code (not the exposed library code)! Takes a `Bool` that decides wether to log messages from other packages that use the same library
-- and did not initalize the Logger (which should be the case for all of them!).
initLoggerAllPackages :: Q Exp
initLoggerAllPackages = [| \dest logLevel logAllPkgs -> setMinLogLevel logLevel >> setLoggingDestination (loc_package $(qLocation >>= liftLoc)) dest logAllPkgs |]

-- | Initialise the logger. MUST only be called in the executable code (not the exposed library code)! Ignores the other packages logs, if the same packages is used for logging.
initLogger :: Q Exp
initLogger = [| \dest logLevel -> setMinLogLevel logLevel >> setLoggingDestination (loc_package $(qLocation >>= liftLoc)) dest False |]

-- | Set the destination for all consequitive for logging. You should only set this once, at the beginning of the program! The default is `LogStdOut`.
setLoggingDestination :: String -> LogDestination -> LogFromAllPackages -> IO ()
setLoggingDestination pkgName LogStdErr logAllPkgs    = newStderrLoggerSet defaultBufSize  >>= \ls -> setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName LogStdErr)
setLoggingDestination pkgName LogStdOut logAllPkgs    = newStdoutLoggerSet defaultBufSize  >>= \ls -> setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName LogStdOut)
setLoggingDestination pkgName (LogFile fp) logAllPkgs = do
  allLs <- M.elems <$> readIORef loggerSets
  ls <-
    case find (\(LoggerSet mFp _ _ _) -> mFp == Just fp) allLs of
      Nothing     -> newFileLoggerSet defaultBufSize fp
      Just lsFile -> newFileLoggerSetSameFile defaultBufSize lsFile
  setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName (LogFile fp))


setLoggingDestinationAllPkgs :: LoggerSet -> String -> LogDestination -> IO ()
setLoggingDestinationAllPkgs _ pkgName LogStdErr  = newStderrLoggerSet defaultBufSize          >>= setLoggerSet pkgName
setLoggingDestinationAllPkgs _ pkgName LogStdOut  = newStdoutLoggerSet defaultBufSize          >>= setLoggerSet pkgName
setLoggingDestinationAllPkgs ls pkgName LogFile{} = newFileLoggerSetSameFile defaultBufSize ls >>= setLoggerSet pkgName


defaultLogPkgName :: String
defaultLogPkgName = "__default__"

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096


-- | Log Level. Levels are sorted. `All` < `Debug` < `Info` < `Warning` < `Error`. None disables all logging. Default: All
data LogLevel
  = LogNone
  | LogAll
  | LogDebug
  | LogInfo
  | LogWarning
  | LogError
  deriving (Show, Eq, Ord)

-- | Log level text. Make sure you call @initLogger@, or logging will be disabled.
logLevelText :: LogLevel -> T.Text
logLevelText LogNone    = mempty
logLevelText LogAll     = "ALL"
logLevelText LogDebug   = "DEBUG"
logLevelText LogInfo    = "INFO "
logLevelText LogWarning = "WARN "
logLevelText LogError   = "ERROR"

-- | Generic log function. Use TH version, e.g. `logDebug`.
logFun :: (ToLogStr msg) => Loc -> LogLevel -> msg -> IO ()
logFun _ LogNone _ = return ()
logFun loc@(Loc _ pkg _ _ _) level msg = do
  minLevel <- readIORef minLogLevel
  when (level >= minLevel) $ do
    now <- readIORef cachedTime >>= \ch -> ch
    readIORef loggerSets >>= \sets ->
      case getLogger sets of
        -- Nothing | M.null sets -> error "You must call `initLogger` at the start of your application! See the documentation of `EasyLogger.Logger`."
        Nothing  -> return ()
        Just set -> pushLogStr set (defaultLogStr loc now level (toLogStr msg))
  where
    getLogger sets = M.lookup pkg sets <|> M.lookup defaultLogPkgName sets


cachedTime :: IORef (IO FormattedTime)
cachedTime = unsafePerformIO $ do
  cache <- newTimeCache simpleTimeFormat'
  newIORef cache


minLogLevel :: IORef LogLevel
minLogLevel = unsafePerformIO $ newIORef LogAll
{-# NOINLINE minLogLevel  #-}

-- | Set the least logging level. Levels lower will not be logged. Log Level Order: `Debug` < `Info` < `Warning` < `Error`. `None` disables all logging. Note that the output to stderr using e.g. `logPrintError` will not
-- be affected!
setMinLogLevel :: LogLevel -> IO ()
setMinLogLevel = writeIORef minLogLevel


-- | Generates a function that takes a 'Text' and logs a 'LevelAll' message. Usage:
--
-- > $(logAll) "This is a debug log message"
logAll :: Q Exp
logAll = [| logFun $(qLocation >>= liftLoc) LogAll |]

-- | Same as `logAll`, but also prints the message on `stdout`.
logPrintAll :: Q Exp
logPrintAll = [| \txt -> hPutStrLn stdout ("DEBUG: " ++ T.unpack txt) >> hFlush stdout >> logFun $(qLocation >>= liftLoc) LogAll txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = [| logFun $(qLocation >>= liftLoc) LogDebug |]

-- | Same as `logDebug`, but also prints the message on `stdout`.
logPrintDebug :: Q Exp
logPrintDebug = [| \txt -> hPutStrLn stdout ("DEBUG: " ++ T.unpack txt) >> hFlush stdout >> logFun $(qLocation >>= liftLoc) LogDebug txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelInfo' message. Usage:
--
-- > $(logInfo) "This is a info log message"
logInfo :: Q Exp
logInfo = [| logFun $(qLocation >>= liftLoc) LogInfo |]

-- | Same as `logInfo`, but also prints the message on `stdout`.
logPrintInfo :: Q Exp
logPrintInfo = [| \txt -> hPutStrLn stdout ("INFO: " ++ T.unpack txt) >> hFlush stdout >> logFun $(qLocation >>= liftLoc) LogInfo txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelWarning' message. Usage:
--
-- > $(logWarning) "This is a warning log message"
logWarning :: Q Exp
logWarning = [| logFun $(qLocation >>= liftLoc) LogWarning |]


-- | Same as `logWarning`, but also prints the message on `stdout`.
logPrintWarning :: Q Exp
logPrintWarning = [| \txt -> hPutStrLn stdout ("WARNING: " ++ T.unpack txt) >> hFlush stdout >> logFun $(qLocation >>= liftLoc) LogWarning txt |]


-- | Generates a function that takes a 'Text' and logs a 'LevelError' message. Usage:
--
-- > $(logError) "This is a error log message"
logError :: Q Exp
logError = [| logFun $(qLocation >>= liftLoc) LogError |]

-- | Same as `logError`, but also prints the message on `stderr`.
logPrintError :: Q Exp
logPrintError = [| \txt -> hPutStrLn stderr ("ERROR: " ++ T.unpack txt) >> hFlush stderr >> logFun $(qLocation >>= liftLoc) LogError txt |]


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


