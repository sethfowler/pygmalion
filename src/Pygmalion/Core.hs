{-# LANGUAGE DeriveGeneric #-}

module Pygmalion.Core
( CommandInfo (..)
, SourceFile
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
, withSourceFile
, queryExecutable
, scanExecutable
, makeExecutable
, dbFile
, configFile
, compileCommandsFile
) where

import Control.Applicative
import Data.Int
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), field)
import GHC.Generics
import System.FilePath.Posix

type Port = Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
  deriving (Eq, Show, Generic)

instance Serialize CommandInfo

instance FromRow CommandInfo where
  fromRow = do
    sf   <- (flip combine) <$> field <*> field
    wd   <- field
    cmd  <- Command <$> field <*> (words <$> field)
    time <- field
    return $ CommandInfo (normalise sf) wd cmd time

data Command = Command String [String]
  deriving (Eq, Show, Generic)

instance Serialize Command

withSourceFile :: CommandInfo -> SourceFile -> CommandInfo
withSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

type SourceFile = String
type WorkingDirectory = String
type Time = Int64

-- The information we collect about definitions in source code.
data DefInfo = DefInfo Identifier SourceLocation DefKind
  deriving (Eq, Show, Generic)

instance FromRow DefInfo where
  fromRow = do
    ident <- Identifier <$> field <*> field
    sf    <- (flip combine) <$> field <*> field
    sl    <- SourceLocation sf <$> field <*> field
    kind  <- field
    return $ DefInfo ident sl kind

data Identifier = Identifier IdName IdUSR
  deriving (Eq, Show, Generic)

data SourceLocation = SourceLocation SourceFile SourceLine SourceColumn
  deriving (Eq, Show, Generic)

type IdName = String
type IdUSR = String
type SourceLine = Int
type SourceColumn = Int
type DefKind = String

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
