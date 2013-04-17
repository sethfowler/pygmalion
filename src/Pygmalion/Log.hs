module Pygmalion.Log
( initLogger
, Priority (..)
, logDebug
, logInfo
, logWarn
, logError
, logException
) where

import System.Log.Logger

logger :: String
logger = "Pygmalion"

initLogger :: Priority -> IO ()
initLogger p = updateGlobalLogger logger (setLevel p)

logDebug :: String -> IO ()
logDebug = debugM logger

logInfo :: String -> IO ()
logInfo = infoM logger

logWarn :: String -> IO ()
logWarn = warningM logger

logError :: String -> IO ()
logError = errorM logger

logException :: Priority -> String -> IO a -> IO a
logException p prefix action = traplogging logger p prefix action
