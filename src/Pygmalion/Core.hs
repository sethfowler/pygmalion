{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile
, WorkingPath
, Time
, Language (..)
, Inclusion (..)
, DefInfo (..)
, SourceLocation (..)
, SourceRange (..)
, Identifier
, USR
, SourceLine
, SourceCol
, SourceKind (..)
, Override (..)
, Invocation (..)
, Reference (..)
, SourceReferenced (..)
, SourceReference (..)
, SourceContext
, Port
, mkSourceFile
, unSourceFile
--, unSourceFileText
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
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), field)
import GHC.Generics

import Pygmalion.SourceKind

type Port = Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo
  { ciSourceFile  :: !SourceFile
  , ciWorkingPath :: !WorkingPath
  , ciCommand     :: !B.ByteString
  , ciArgs        :: ![B.ByteString]
  , ciLanguage    :: !Language
  , ciLastIndexed :: !Time
  } deriving (Eq, Read, Show, Generic)

{-
instance Serialize T.Text where
  put = put . TE.encodeUtf16BE
  get = liftM (TE.decodeUtf16BEWith onError) get
      where onError _ _ = Nothing
-}

instance Serialize CommandInfo

instance FromRow CommandInfo where
  fromRow = CommandInfo <$> field               -- ciSourceFile
                        <*> field               -- ciWorkingPath
                        <*> field               -- ciCommand
                        <*> (B.lines <$> field) -- ciArgs
                        <*> fromRow             -- ciLanguage
                        <*> field               -- ciLastIndexed

type SourceFile = B.ByteString

mkSourceFile :: FilePath -> SourceFile
mkSourceFile = B.fromString

{-
unSourceFileText :: SourceFile -> B.ByteString
unSourceFileText = id
-}

unSourceFile :: SourceFile -> FilePath
unSourceFile = B.toString

type WorkingPath = B.ByteString
type Time = Int64

data Language = CLanguage
              | CPPLanguage
              | UnknownLanguage
              deriving (Eq, Enum, Generic, Ord, Read, Show)

instance Serialize Language

instance FromRow Language where
  fromRow = toEnum <$> field

-- Inclusion metadata.
data Inclusion = Inclusion
    { icCommandInfo :: !CommandInfo
    , icHeaderFile  :: !SourceFile
    , icDirect      :: !Bool
    } deriving (Eq, Show, Generic)

instance Serialize Inclusion

-- The information we collect about definitions in source code.
data DefInfo = DefInfo
    { diIdentifier     :: !Identifier
    , diUSR            :: !USR
    , diSourceLocation :: !SourceLocation
    , diDefKind        :: !SourceKind
    } deriving (Eq, Show, Generic)

instance Serialize DefInfo

instance FromRow DefInfo where
  fromRow = DefInfo <$> field <*> field <*> fromRow <*> fromRow

data SourceLocation = SourceLocation
    { slFile :: !SourceFile
    , slLine :: !SourceLine
    , slCol  :: !SourceCol
    } deriving (Eq, Show, Generic)

instance Serialize SourceLocation

instance FromRow SourceLocation where
  fromRow = SourceLocation <$> field <*> field <*> field

data SourceRange = SourceRange
    { srFile    :: !SourceFile
    , srLine    :: !SourceLine
    , srCol     :: !SourceCol
    , srEndLine :: !SourceLine
    , srEndCol  :: !SourceCol
    } deriving (Eq, Show, Generic)

instance Serialize SourceRange

instance FromRow SourceRange where
  fromRow = SourceRange <$> field <*> field <*> field <*> field <*> field

type Identifier = B.ByteString
type USR        = B.ByteString
type SourceLine = Int
type SourceCol  = Int

data Override = Override
    { orDef       :: !USR
    , orOverrided :: !USR
    } deriving (Eq, Show, Generic)

instance Serialize Override

data Invocation = Invocation
    { ivDefInfo        :: !DefInfo
    , ivSourceLocation :: !SourceLocation
    } deriving (Eq, Show, Generic)

instance Serialize Invocation

instance FromRow Invocation where
  fromRow = Invocation <$> fromRow <*> fromRow

data Reference = Reference
    { rfRange   :: !SourceRange
    , rfKind    :: !SourceKind
    , rfContext :: !USR
    , rfUSR     :: !USR
    } deriving (Eq, Show, Generic)

instance Serialize Reference

data SourceReferenced = SourceReferenced
    { sdDef   :: !DefInfo
    , sdRange :: !SourceRange
    , sdKind  :: !SourceKind
    } deriving (Eq, Show, Generic)

instance Serialize SourceReferenced

instance FromRow SourceReferenced where
  fromRow = SourceReferenced <$> fromRow <*> fromRow <*> fromRow

data SourceReference = SourceReference
    { srLocation :: !SourceLocation
    , srKind     :: !SourceKind
    , srContext  :: !SourceContext
    } deriving (Eq, Show, Generic)

instance Serialize SourceReference

instance FromRow SourceReference where
  fromRow = SourceReference <$> fromRow <*> fromRow <*> field

type SourceContext = B.ByteString

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
