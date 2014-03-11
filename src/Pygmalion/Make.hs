{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Make
( observeMake
, indexUserCommand
) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception (catch, SomeException)
import Control.Monad hiding (join)
import Data.IORef
import qualified Data.IntSet as Set
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.String.Utils (join, replace)
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.Process (createProcess, CreateProcess(..), shell)
import System.Process.Voyeur

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.File
import Pygmalion.Index.Command
import Pygmalion.Log
import Pygmalion.RPC.Client

data ObservedEvent = ExecEvent String [String] String
                   | FinishedEvent

observeMake :: Config -> String -> [String] -> IO ExitCode
observeMake cf cmd args = do
  evtChan <- newChan
  eventAsync <- async (sendEvents cf evtChan)
  ret <- withVoyeur (doWatch cf evtChan cmd args)
  writeChan evtChan FinishedEvent
  wait eventAsync
  return ret

indexUserCommand :: Config -> String -> [String] -> IO ()
indexUserCommand cf cmd args = do
  wd <- getCurrentDirectory
  indexCommand cf cmd args wd

sendEvents :: Config -> Chan ObservedEvent -> IO ()
sendEvents !cf !evtChan = do
  evt <- readChan evtChan
  case evt of
    ExecEvent !cmd !args !wd -> do indexCommand cf cmd args wd `catch` ignoreRPCFailure
                                   sendEvents cf evtChan
    FinishedEvent            -> return ()

doWatch :: Config -> Chan ObservedEvent -> String -> [String] -> VoyeurContext -> IO ExitCode
doWatch cf evtChan cmd args ctx = do
  -- Set up our handlers.
  let execFlags = defaultObserveExecFlags { observeExecCWD = True }
      exitFlags = defaultObserveExitFlags

  badPids <- newIORef Set.empty
  observeExec ctx execFlags (execHandler evtChan badPids)
  observeExit ctx exitFlags (exitHandler badPids)

  -- Set up the environment.
  envp <- prepareEnvironment ctx =<< getEnvironment

  -- Start the child process.
  let shellCmd = expandedCommand cf cmd args
  (_, _, _, handle) <- createProcess $ (shell shellCmd)
                                       { env = Just envp
                                       , close_fds = True
                                       }

  -- Observe it!
  ret <- startObserving ctx handle

  logInfo "startObserving returned"
  return ret

expandedCommand :: Config -> String -> [String] -> String
expandedCommand cf cmd args = replace "$(projectroot)" (projectDir cf)
                            . replace "$(args)" (join " " (cmd : args))
                            . makeCmd $ cf

compilers :: [B.ByteString]
compilers = ["clang", "clang++", "gcc", "g++"]

execHandler :: Chan ObservedEvent -> IORef Set.IntSet -> ObserveExecHandler
execHandler evtChan badPids path argv _ wd pid ppid =
  -- TODO: Should extract just filename instead of using isSuffixOf.
  when (any (`B.isSuffixOf` path) compilers) $ do
    badPidSet <- readIORef badPids
    unless ((fromIntegral pid `Set.member` badPidSet) ||
            (fromIntegral ppid `Set.member` badPidSet)) $ do
               modifyIORef' badPids (Set.insert $ fromIntegral pid)
               let cmd  = BU.toString path
                   args = map BU.toString (safeTail argv)
                   wd'  = BU.toString wd
               writeChan evtChan $ ExecEvent cmd args wd'

exitHandler :: IORef Set.IntSet -> ObserveExitHandler
exitHandler badPids _ pid _ = do
  badPidSet <- readIORef badPids
  when (fromIntegral pid `Set.member` badPidSet) $
    modifyIORef' badPids (Set.delete $ fromIntegral pid)

indexCommand :: Config -> String -> [String] -> String -> IO ()
indexCommand cf cmd args wd = do
  --logWarn $ "indexCommand: [" ++ show cmd ++ "] [" ++ show args ++ "] [" ++ show wd ++ "]"
  mayCI <- getCommandInfo cmd args wd
  case mayCI of
    Just ci -> do let sf = ciSourceFile ci
                  mayMTime <- getMTime sf
                  case mayMTime of
                    Just mtime -> withRPC' cf $ runRPC (rpcIndexCommand ci mtime)
                    Nothing    -> putStrLn $ "Couldn't read file " ++ show sf
    Nothing -> return ()  -- Couldn't parse command. Very likely a linker command.

ignoreRPCFailure :: SomeException -> IO ()
ignoreRPCFailure _ = return ()  -- Silently ignore failure.

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs
