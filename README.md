# Easy Logging for Haskell

Easy-logger can be used to easily create logs, without handling any Monad. Just log as you go. The
package provides logging for code that lives in the `IO` Monad, as well as for pure code.

## Usage

Initialise the logger for your package and start logging:

    import qualified Data.Text                          as T

    main :: IO ()
    main = do
      $(initLogger) (LogFile "package.log") LogDebug
      $(logInfo) ("Starting App" :: T.Text)
      ...
      # At the end of your program, flush the buffers:
      finalizeAllLoggers

You can also include the logs of the libraries that you use and which use the @easy-logger@ package
for logging. If the library maintainer is nice, (s)he allows you to turn on/off logging for that
library only. See the Library section below how to do it. To turn logging for all packages
(that use easy-logger) on, do this:

    import qualified Data.Text                          as T

    main :: IO ()
    main = do
      $(initLoggerAllPackages) (LogFile "package.log") LogAll True
      $(logInfo) ("Starting App" :: T.Text)
      ...
      # At the end of your program, flush the buffers:
      finalizeAllLoggers

You want to log in pure code without wrapping anything in a Monad? Here you go:

    fromEither :: Either String Int -> Int
    fromEither (Right v)  = v
    fromEither (Left str) = $(pureLogPrintError) ("Parse error: " <> T.pack str) defaultValue
    defaultValue = 0

Under the hood `pureLogPrintError` uses `unsafePerformIO` to log. The return value, i.e.
`defaultValue` in the example, ensures that the log is actually executed when the error occurs.

## Log Levels and Destinations

The available log levels are:

    -- | Log Level. Levels are sorted. `All` < `Debug` < `Info` < `Warning` < `Error`. None disables all logging. Default: All
    data LogLevel
      = LogNone
      | LogAll
      | LogDebug
      | LogInfo
      | LogWarning
      | LogError
      deriving (Show, Read, Bounded, Enum, Eq, Ord)

The logger can be used to log to @stderr@, @stdout@ or a file:

    -- | Logging destination. See also `setLoggingDestination`.
    data LogDestination
      = LogStdErr
      | LogStdOut
      | LogFile FilePath


## Library

If you are exposing a library, let your user turn on/off the logging for your library. The
Template-Haskell code ensures that your package name is provided to the logger, and thus logging for this module only is turned on/off.


    {-# LANGUAGE TemplateHaskell #-}
    module My.Great.Package.Logging
        ( enableRequestLogging
        , disableRequestLogging
        ) where

    import           EasyLogger

    enableRequestLogging :: LogDestination -> LogLevel -> IO ()
    enableRequestLogging = $(initLogger)

    disableRequestLogging :: IO ()
    disableRequestLogging = $(finalizeLogger)


