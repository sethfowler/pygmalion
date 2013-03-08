{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString.Char8 ()
import Data.Conduit
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
serverApp chan ad = (appSource ad) $$ conduit =$ (appSink ad)
  where conduit = do
          result <- await
          case result of
            Just serialized -> processCmd (decode serialized)
            _               -> error "Client never sent anything"

        processCmd (Right tup) = do
          liftIO $ putStrLn $ "Server got: " ++ (show tup)
          case tupleToCommandInfo tup of
            Just cmdInfo -> do
                    liftIO $ writeChan chan (Just cmdInfo)
                    yield "OK"
            _            -> yield "ERROR"
        processCmd _ = yield "ERROR"
