{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile (..)
, WorkingDirectory
, Command (..)
, DefInfo (..)
, Identifier (..)
, SourceLocation (..)
, IdName
, IdUSR
, SourceLine
, SourceColumn
, DefKind
, Port
, mkSourceFile
, unSourceFile
, unSourceFileText
, withSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, dbFile
, configFile
, compileCommandsFile
) where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), field)
import GHC.Generics
import System.FilePath.Posix

type Port = Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo !SourceFile !WorkingDirectory !Command !Time
  deriving (Eq, Show, Generic)

instance Serialize T.Text where
  put = put . TE.encodeUtf16BE
  get = liftM (TE.decodeUtf16BEWith onError) get
      where onError _ _ = Nothing

instance Serialize CommandInfo

instance FromRow CommandInfo where
  fromRow = do
    sf    <- SourceFile <$> field <*> field
    wd    <- field
    cmd   <- Command <$> field <*> (T.words <$> field)
    time  <- field
    return $ CommandInfo sf wd cmd time

data Command = Command !T.Text ![T.Text]
  deriving (Eq, Show, Generic)

instance Serialize Command

withSourceFile :: CommandInfo -> SourceFile -> CommandInfo
withSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

-- A source file has a name and a path.
data SourceFile = SourceFile !T.Text !T.Text
  deriving (Eq, Show, Generic)

instance Serialize SourceFile

mkSourceFile :: FilePath -> SourceFile
mkSourceFile f = SourceFile (T.pack . normalise . takeFileName $ f)
                            (T.pack . normalise . takeDirectory $ f)

unSourceFileText :: SourceFile -> T.Text
unSourceFileText (SourceFile sn sp) = T.concat [sp, "/", sn]

unSourceFile :: SourceFile -> FilePath
unSourceFile = T.unpack . unSourceFileText

type WorkingDirectory = T.Text
type Time = Int64

-- The information we collect about definitions in source code.
data DefInfo = DefInfo !Identifier !SourceLocation !DefKind
  deriving (Eq, Show, Generic)

instance FromRow DefInfo where
  fromRow = do
    ident <- Identifier <$> field <*> field
    sf    <- SourceFile <$> field <*> field
    sl    <- SourceLocation sf <$> field <*> field
    kind  <- field
    return $ DefInfo ident sl kind

data Identifier = Identifier !IdName !IdUSR
  deriving (Eq, Show, Generic)

data SourceLocation = SourceLocation !SourceFile !SourceLine !SourceColumn
  deriving (Eq, Show, Generic)

type IdName = T.Text
type IdUSR = T.Text
type SourceLine = Int
type SourceColumn = Int
type DefKind = T.Text

-- Tool names.
queryExecutable, scanExecutable, makeExecutable :: String
queryExecutable = "pygmalion"
scanExecutable  = "pygscan"
makeExecutable  = "pygmake"

-- Data files.
dbFile, configFile, compileCommandsFile :: String
dbFile              = ".pygmalion.sqlite"
configFile          = ".pygmalion.conf"
compileCommandsFile = "compile_commands.json"
