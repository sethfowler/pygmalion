module Pygmalion.Analyze
( scanCommandAndUpdateDB
) where

import Control.Monad
import Control.Monad.Reader
import System.Exit
import System.FilePath.Posix

import Pygmalion.Analyze.Command
import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

-- TODO: Get rid of "exitSuccess" and such in here and run this whole thing
-- in the maybe monad. Generally could use more refactoring.
scanCommandAndUpdateDB :: (FilePath, Command) -> IO ()
scanCommandAndUpdateDB = runScanner (\c -> analyzeCmd c
                                       >>= analyzeCode
                                       >>= updateDB)

type DBPath  = FilePath
type Scanner a = ReaderT DBPath IO a

runScanner :: (a -> Scanner b) -> (DBPath, a) -> IO b
runScanner f (db, v) = runReaderT (f v) db

analyzeCmd :: Command -> Scanner CommandInfo
analyzeCmd cmd = do
  mci <- liftIO $ getCommandInfo cmd
  case mci of
    Just ci -> return ci
    _       -> liftIO $ exitSuccess

analyzeCode :: CommandInfo -> Scanner (CommandInfo, [FilePath])
analyzeCode ci = do
  includedFiles <- liftIO $ clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (ci, map normalise localHeaders)

updateDB :: (CommandInfo, [FilePath]) -> Scanner ()
updateDB (ci, headers) = do
  db <- ask
  liftIO $ withDB db $ \handle -> do
    updateRecord handle ci
    -- Add entries for all included non-system headers, using the same metadata.
    forM_ headers $ \header -> do
      updateRecord handle $ updateSourceFile ci header
