{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingDirectory
, Time
, Command (..)
, Inclusion (..)
, DefInfo (..)
, SourceLocation (..)
, Identifier
, USR
, SourceLine
, SourceColumn
, DefKind
, Port
, mkSourceFile
, unSourceFile
, unSourceFileText
, queryExecutable
, scanExecutable
, makeExecutable
, daemonExecutable
, clangExecutable
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
data CommandInfo = CommandInfo
  { ciSourceFile  :: !SourceFile
  , ciWorkingPath :: !WorkingDirectory
  , ciCommand     :: !Command
  , ciLastIndexed :: !Time
  } deriving (Eq, Show, Generic)

instance Serialize T.Text where
  put = put . TE.encodeUtf16BE
  get = liftM (TE.decodeUtf16BEWith onError) get
      where onError _ _ = Nothing

instance Serialize CommandInfo

instance FromRow CommandInfo where
  fromRow = do
    sf    <- field
    wd    <- field
    cmd   <- Command <$> field <*> (T.words <$> field)
    time  <- field
    return $ CommandInfo sf wd cmd time

data Command = Command !T.Text ![T.Text]
  deriving (Eq, Show, Generic)

instance Serialize Command

type SourceFile = T.Text

mkSourceFile :: FilePath -> SourceFile
mkSourceFile = T.pack

unSourceFileText :: SourceFile -> T.Text
unSourceFileText = id

unSourceFile :: SourceFile -> FilePath
unSourceFile = T.unpack

type WorkingDirectory = T.Text
type Time = Int64

-- Inclusion metadata.
data Inclusion = Inclusion
    { inSourceFile :: !SourceFile
    , inHeaderFile :: !SourceFile
    , inDirect     :: !Bool
    } deriving (Eq, Show, Generic)

instance Serialize Inclusion

instance FromRow Inclusion where
  fromRow = Inclusion <$> field <*> field <*> field

-- The information we collect about definitions in source code.
data DefInfo = DefInfo
    { diIdentifier     :: !Identifier
    , diUSR            :: !USR
    , diSourceLocation :: !SourceLocation
    , diDefKind        :: !DefKind
    } deriving (Eq, Show, Generic)

instance Serialize DefInfo

instance FromRow DefInfo where
  fromRow = do
    ident <- field
    usr   <- field
    sl    <- SourceLocation <$> field <*> field <*> field
    kind  <- field
    return $ DefInfo ident usr sl kind

data SourceLocation = SourceLocation !SourceFile !SourceLine !SourceColumn
  deriving (Eq, Show, Generic)

instance Serialize SourceLocation

type Identifier   = T.Text
type USR          = T.Text
type SourceLine   = Int
type SourceColumn = Int
type DefKind      = T.Text

-- Tool names.
queryExecutable, scanExecutable, makeExecutable, daemonExecutable, clangExecutable :: String
queryExecutable  = "pygmalion"
scanExecutable   = "pygscan"
makeExecutable   = "pygmake"
daemonExecutable = "pygd"
clangExecutable  = "pygclangindex"

-- Data files.
dbFile, configFile, compileCommandsFile :: String
dbFile              = ".pygmalion.sqlite"
configFile          = ".pygmalion.conf"
compileCommandsFile = "compile_commands.json"
