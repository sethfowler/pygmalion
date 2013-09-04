{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Index.Manager
import Pygmalion.Log
import Pygmalion.RPC.Request

runRPCServer :: Config -> IndexChan -> DBChan -> DBChan -> IO ()
runRPCServer cf iChan dbChan dbQueryChan =
    runTCPServer settings (serverApp $ RPCServerContext iChan dbChan dbQueryChan)
  where
    settings = (serverSettings port addr) :: ServerSettings IO
    port = ifPort cf
    addr = fromString $ ifAddr cf

serverApp :: RPCServerContext -> Application IO
serverApp ctx ad =
    (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest
    process = do
      result <- await
      case result of
        Just RPCDone -> return ()  -- Close connection.
        Just req     -> do res <- liftIO $ runReaderT (route req) ctx
                           case res of
                              Just res' -> yield res' >> process
                              Nothing   -> process
        _            -> do logError "RPC server got bad request"
                           yield . encode $ (RPCError :: RPCResponse ())

data RPCServerContext = RPCServerContext
  { rsIndexChan   :: !IndexChan
  , rsDBChan      :: !DBChan
  , rsDBQueryChan :: !DBChan
  }
type RPCServer a = ReaderT RPCServerContext IO a

route :: RPCRequest -> RPCServer (Maybe ByteString)
route (RPCSendCommandInfo ci)       = sendIndex_ $ Index (FromBuild ci)
route (RPCGetCommandInfo sf)        = sendQuery $ DBGetCommandInfo sf
route (RPCGetSimilarCommandInfo sf) = sendQuery $ DBGetSimilarCommandInfo sf
route (RPCGetDefinition usr)        = sendQuery $ DBGetDefinition usr
route (RPCGetCallers usr)           = sendQuery $ DBGetCallers usr
route (RPCGetCallees usr)           = sendQuery $ DBGetCallees usr
route (RPCGetBases usr)             = sendQuery $ DBGetBases usr
route (RPCGetOverrides usr)         = sendQuery $ DBGetOverrides usr
route (RPCGetRefs usr)              = sendQuery $ DBGetRefs usr
route (RPCGetReferenced sl)         = sendQuery $ DBGetReferenced sl
route (RPCFoundDef df)              = sendUpdate_ $ DBUpdateDef df
route (RPCFoundOverride ov)         = sendUpdate_ $ DBUpdateOverride ov
route (RPCFoundRef ru)              = sendUpdate_ $ DBUpdateRef ru
route (RPCFoundInclusion ic)        = sendInclusionUpdate_ ic
route (RPCLog s)                    = logInfo s >> return Nothing
route (RPCPing)                     = return . Just $! encode (RPCOK ())
route (RPCDone)                     = error "Should not route RPCDone"

sendIndex_ :: IndexRequest -> RPCServer (Maybe ByteString)
sendIndex_ !req = do
  ctx <- ask
  writeLenChan (rsIndexChan ctx) req
  return Nothing

sendQuery :: Serialize a => (Response a -> DBRequest) -> RPCServer (Maybe ByteString)
sendQuery !req = do
  ctx <- ask
  result <- callLenChan (rsDBQueryChan ctx) req
  return . Just $! encode (RPCOK result)

sendUpdate_ :: DBRequest -> RPCServer (Maybe ByteString)
sendUpdate_ !req = do
  ctx <- ask
  writeLenChan (rsDBChan ctx) req
  return Nothing

sendInclusionUpdate_ :: Inclusion -> RPCServer (Maybe ByteString)
sendInclusionUpdate_ ic = do
  ctx <- ask
  writeLenChan (rsDBChan ctx) (DBUpdateInclusion ic)
  existing <- callLenChan (rsDBQueryChan ctx)
                          (DBInsertFileAndCheck . icHeaderFile $ ic)
  when (not existing) $
    writeLenChan (rsIndexChan ctx) (Index . FromBuild . icCommandInfo $ ic)
  return Nothing
