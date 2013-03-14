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
import Network.Socket

import Pygmalion.Core

runRPCServer :: MVar Int -> Chan (Maybe CommandInfo) -> IO ()
runRPCServer port chan = runTCPServer settings (serverApp chan)
  where settings = baseSettings { serverAfterBind = notifyPort }
        baseSettings = (serverSettings 0 "127.0.0.1") :: ServerSettings IO
        notifyPort s = socketPort s >>= (putMVar port) . fromIntegral

serverApp :: Chan (Maybe CommandInfo) -> Application IO
serverApp chan ad = (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where getCI = get :: Get CommandInfo
        process = do
          result <- await
          -- liftIO $ putStrLn $ "Server got: " ++ (show result)
          case result of
            ci@(Just _) -> liftIO (writeChan chan ci) >> yield "OK"
            _           -> yield "ERROR"
