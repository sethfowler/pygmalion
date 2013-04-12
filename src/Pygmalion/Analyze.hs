module Pygmalion.Analyze
( scanCommandAndUpdateDB
, runAnalysisThread
) where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import System.Directory

import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

runAnalysisThread :: Chan (Maybe CommandInfo) -> IO ()
runAnalysisThread chan = do
    wd <- T.pack <$> getCurrentDirectory
    withDB (go wd)
  where go :: T.Text -> DBHandle -> IO ()
        go wd h = do  mayCmd <- readChan chan
                      case mayCmd of
                        Just cmd -> scanCommandAndUpdateDB h wd cmd >> go wd h
                        Nothing  -> return ()

scanCommandAndUpdateDB :: DBHandle -> T.Text -> CommandInfo -> IO ()
scanCommandAndUpdateDB h wd cmdInfo = do
  mayResult <- analyzeCode wd cmdInfo
  case mayResult of
    Just result -> updateDB h result
    Nothing     -> return ()

analyzeCode :: T.Text -> CommandInfo -> IO (Maybe (CommandInfo, [SourceFile], [DefInfo]))
analyzeCode wd ci = do
  --liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  result <- liftIO $ runSourceAnalyses wd ci
  case result of
    Just (is, ds) -> return . Just $ (ci, is, ds)
    Nothing       -> return Nothing

updateDB :: DBHandle -> (CommandInfo, [SourceFile], [DefInfo]) -> IO ()
updateDB h (ci, includes, defs) = liftIO $ do
    updateSourceFile h ci
    -- Update entries for all non-system includes, using the same metadata.
    -- forM_ includes $ \i -> do
      -- updateSourceFile h $ withSourceFile ci i
    -- Update entries for all definitions.
    forM_ defs $ \d -> do
      updateDef h d
    --liftIO $ putStrLn $ "Updated DB entries related to " ++ (show ci)
