{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Make
( observeMake
, indexUserCommand
) where

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
import Pygmalion.RPC.Client

observeMake :: Config -> String -> [String] -> IO ExitCode
observeMake cf cmd args = withVoyeur (doWatch cf cmd args)

indexUserCommand :: Config -> String -> [String] -> IO ()
indexUserCommand cf cmd args = do
  wd <- getCurrentDirectory
  indexCommand cf cmd args wd

doWatch :: Config -> String -> [String] -> VoyeurContext -> IO ExitCode
doWatch cf cmd args ctx = do
  -- Set up our handlers.
  let execFlags = defaultObserveExecFlags { observeExecCWD = True }
      exitFlags = defaultObserveExitFlags

  badPids <- newIORef Set.empty
  observeExec ctx execFlags (execHandler cf badPids)
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
  startObserving ctx handle

expandedCommand :: Config -> String -> [String] -> String
expandedCommand cf cmd args = replace "$(projectroot)" (projectDir cf)
                            . replace "$(args)" (join " " (cmd : args))
                            . makeCmd $ cf

compilers :: [B.ByteString]
compilers = ["clang", "clang++", "gcc", "g++", "ccache"]

execHandler :: Config -> IORef Set.IntSet -> ObserveExecHandler
execHandler cf badPids path argv _ wd pid ppid =
  -- TODO: Should extract just filename instead of using isSuffixOf.
  -- TODO: Not sure ccache handling here is sufficient.
  when (any (`B.isSuffixOf` path) compilers) $ do
    badPidSet <- readIORef badPids
    unless ((fromIntegral pid `Set.member` badPidSet) ||
            (fromIntegral ppid `Set.member` badPidSet)) $ do
               modifyIORef' badPids (Set.insert $ fromIntegral pid)
               let cmd  = BU.toString path
                   args = map BU.toString (safeTail argv)
                   wd'  = BU.toString wd
               indexCommand cf cmd args wd' `catch` ignoreRPCFailure

exitHandler :: IORef Set.IntSet -> ObserveExitHandler
exitHandler badPids _ pid _ = do
  badPidSet <- readIORef badPids
  when (fromIntegral pid `Set.member` badPidSet) $
    modifyIORef' badPids (Set.delete $ fromIntegral pid)

indexCommand :: Config -> String -> [String] -> String -> IO ()
indexCommand cf cmd args wd = do
  mayCI <- getCommandInfo cmd args wd
  case mayCI of
    Just ci -> do let sf = ciSourceFile ci
                  mayMTime <- getMTime sf
                  case mayMTime of
                    Just mtime -> withRPC cf $ runRPC (rpcIndexCommand ci mtime)
                    Nothing    -> putStrLn $ "Couldn't read file " ++ show sf
    Nothing -> return ()  -- Couldn't parse command. Very likely a linker command.

ignoreRPCFailure :: SomeException -> IO ()
ignoreRPCFailure _ = return ()  -- Silently ignore failure.

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs
