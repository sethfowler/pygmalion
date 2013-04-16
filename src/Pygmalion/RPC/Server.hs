{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String
import Network.Socket

import Pygmalion.Analyze
import Pygmalion.Config
import Pygmalion.Core

runRPCServer :: Config -> MVar Int -> AnalysisChan -> IO ()
runRPCServer cf port chan = runTCPServer settings (serverApp chan)
  where settings = baseSettings { serverAfterBind = notifyPort }
        baseSettings = (serverSettings confPort confAddr) :: ServerSettings IO
        confPort = ifPort cf
        confAddr = fromString (ifAddr cf)
        notifyPort s = socketPort s >>= (putMVar port) . fromIntegral

serverApp :: AnalysisChan -> Application IO
serverApp chan ad = (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where getCI = {-# SCC "serverget" #-} get :: Get CommandInfo
        process = do
          result <- await
          -- liftIO $ putStrLn $ "Server got: " ++ (show result)
          case result of
            Just ci -> liftIO (writeChan chan $ Analyze ci) >> yield "OK"
            _       -> yield "ERROR"
