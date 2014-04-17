module Pygmalion.File
( getMTime
, asSourceFile
, canonicalPath
) where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString.UTF8 as B
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Directory
import System.Path
import System.Posix.Directory.Traversals

import Pygmalion.Core
import Pygmalion.Log

getMTime :: SourceFile -> IO (Maybe Time)
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.

asSourceFile :: FilePath -> FilePath -> IO SourceFile
asSourceFile wd p = canonicalPath
                  . B.fromString
                  $ fromMaybe p (absNormPath wd p)

canonicalPath :: B.ByteString -> IO B.ByteString
canonicalPath p = either handleError id <$> try (realpath p)
  where handleError :: IOError -> B.ByteString
        handleError = const p
