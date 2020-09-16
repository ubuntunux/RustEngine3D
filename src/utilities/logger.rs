{-# LANGUAGE RecordWildCards #-}

module HulkanEngine3D.Utilities.Logger
    ( logTrivialInfo
    , logInfo
    , logDebug
    , logWarn
    , logError
    ) where

import GHC.Stack hiding (prettyCallStack, prettySrcLoc)
import Data.List
import Data.Time
import Control.Monad

prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]

prettySrcLoc' :: SrcLoc -> String
prettySrcLoc' SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol
      ]


prettyCallStack :: CallStack -> String -> String -> String -> String
prettyCallStack cs time loggerLevel msg = intercalate "\n" $ prettyCallStackLines cs time loggerLevel msg


prettyCallStackLines :: CallStack -> String -> String -> String -> [String]
prettyCallStackLines cs time loggerLevel msg = case getCallStack cs of
  []  -> []
  stk -> map ((++"    ") . prettyCallSite) stk
  where
    --prettyCallSite (f, loc) = "[" ++ time ++ "]" ++ "[" ++ loggerLevel ++ "] " ++ msg ++ " (" ++ prettySrcLoc loc ++ ")"
    prettyCallSite (f, loc) = "[" ++ time ++ "]" ++ "[" ++ loggerLevel ++ "] " ++ msg ++ " (" ++ prettySrcLoc' loc ++ ")"

getLoggerTime :: IO String
getLoggerTime = formatTime defaultTimeLocale "%F %T.%3q" <$> getZonedTime

enableLogTrivialInfo :: Bool
enableLogTrivialInfo = False

logTrivialInfo :: HasCallStack => String -> IO ()
logTrivialInfo msg =
    when enableLogTrivialInfo $ do
        time <- getLoggerTime
        putStrLn $ prettyCallStack callStack time "INFO" msg

logInfo :: HasCallStack => String -> IO ()
logInfo msg = do
    time <- getLoggerTime
    putStrLn $ prettyCallStack callStack time "INFO" msg

logDebug :: HasCallStack => String -> IO ()
logDebug msg = do
    time <- getLoggerTime
    putStrLn $ prettyCallStack callStack time "DEBUG" msg

logWarn :: HasCallStack => String -> IO ()
logWarn msg = do
    time <- getLoggerTime
    putStrLn $ prettyCallStack callStack time "WARNING" msg

logError :: HasCallStack => String -> IO ()
logError msg = do
    time <- getLoggerTime
    putStrLn $ prettyCallStack callStack time "ERROR" msg