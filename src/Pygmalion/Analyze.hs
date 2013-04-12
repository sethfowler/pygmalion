module Pygmalion.Analyze
( scanCommandAndUpdateDB
, runAnalysisThread
) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans

import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

runAnalysisThread :: Chan (Maybe CommandInfo) -> IO ()
runAnalysisThread chan = withDB go
  where go :: DBHandle -> IO ()
        go h = do mayCmd <- readChan chan
                  case mayCmd of
                    Just cmd -> scanCommandAndUpdateDB h cmd >> go h
                    Nothing  -> return ()

scanCommandAndUpdateDB :: DBHandle -> CommandInfo -> IO ()
scanCommandAndUpdateDB h cmdInfo = do
  mayResult <- analyzeCode cmdInfo
  case mayResult of
    Just result -> updateDB h result
    Nothing     -> return ()

analyzeCode :: CommandInfo -> IO (Maybe (CommandInfo, [SourceFile], [DefInfo]))
analyzeCode ci = do
  --liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  result <- liftIO $ runSourceAnalyses ci
  case result of
    Just (is, ds) -> return . Just $ (ci, is, ds)
    Nothing       -> return Nothing

updateDB :: DBHandle -> (CommandInfo, [SourceFile], [DefInfo]) -> IO ()
updateDB h (ci, includes, defs) = liftIO $ do
    updateSourceFile h ci
    -- Update entries for all non-system includes, using the same metadata.
    forM_ includes $ \i -> do
      updateSourceFile h $ withSourceFile ci i
    -- Update entries for all definitions.
    forM_ defs $ \d -> do
      updateDef h d
    --liftIO $ putStrLn $ "Updated DB entries related to " ++ (show ci)
