module Pygmalion.Log
( initLogger
, Priority (..)
, logDebug
, logInfo
, logWarn
, logError
, logException
) where

import Control.Monad.IO.Class
import System.Log.Logger

logger :: String
logger = "Pygmalion"

initLogger :: MonadIO m => Priority -> m ()
initLogger p = liftIO $ updateGlobalLogger logger (setLevel p)

logDebug :: MonadIO m => String -> m ()
logDebug str = liftIO $ debugM logger str

logInfo :: MonadIO m => String -> m ()
logInfo str = liftIO $ infoM logger str

logWarn :: MonadIO m => String -> m ()
logWarn str = liftIO $ warningM logger str

logError :: MonadIO m => String -> m ()
logError str = liftIO $ errorM logger str

logException :: MonadIO m => Priority -> String -> IO a -> m a
logException p prefix action = liftIO $ traplogging logger p prefix action
