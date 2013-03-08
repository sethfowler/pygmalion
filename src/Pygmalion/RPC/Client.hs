{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Client
( sendScanMessage
) where

import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Network
import Data.Serialize

import Pygmalion.Core

sendScanMessage :: Port -> CommandInfo -> IO ()
sendScanMessage port cmd = runTCPClient settings (scanApp cmd)
  where settings = clientSettings port "127.0.0.1"

scanApp :: CommandInfo -> Application IO
scanApp cmdInfo ad = (appSource ad) $$ conduit =$ (appSink ad)
  where conduit = do
          yield (encode . commandInfoToTuple $ cmdInfo)
          result <- await
          case result of
            Just "OK"    -> return ()
            Just "ERROR" -> error "Server reported an error"
            _            -> error "Unexpected result from server"
