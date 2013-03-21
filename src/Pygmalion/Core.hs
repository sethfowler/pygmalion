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
import Control.DeepSeq
import Data.Int
import Data.List
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), ToRow(..), field)
import Database.SQLite.Simple.ToField (ToField(..))
import GHC.Generics
import System.FilePath.Posix

type Port = Int

-- The information we collect about a compilation command.
data CommandInfo = CommandInfo SourceFile WorkingDirectory Command Time
  deriving (Eq, Show, Generic)

instance Serialize CommandInfo

instance NFData CommandInfo where
  rnf (CommandInfo sf wd cmd t) =
    sf `deepseq` wd `deepseq` cmd `deepseq` t `deepseq` ()

instance ToRow CommandInfo where
  toRow (CommandInfo sf wd (Command cmd args) t) =
    [toField . takeFileName $ sf,
     toField . takeDirectory $ sf,
     toField wd,
     toField (intercalate " " (cmd : args)),
     toField t]

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

instance NFData Command where
  rnf (Command cmd args) = cmd `deepseq` args `deepseq` ()

withSourceFile :: CommandInfo -> SourceFile -> CommandInfo
withSourceFile (CommandInfo _ wd cmd t) sf' = CommandInfo sf' wd cmd t

type SourceFile = String
type WorkingDirectory = String
type Time = Int64

-- The information we collect about definitions in source code.
data DefInfo = DefInfo Identifier SourceLocation DefKind
  deriving (Eq, Show, Generic)

instance NFData DefInfo where
  rnf (DefInfo i sl k) = i `deepseq` sl `deepseq` k `deepseq` ()

data Identifier = Identifier IdName IdUSR
  deriving (Eq, Show, Generic)

instance NFData Identifier where
  rnf (Identifier n u) = n `deepseq` u `deepseq` ()

data SourceLocation = SourceLocation SourceFile SourceLine SourceColumn
  deriving (Eq, Show, Generic)

instance NFData SourceLocation where
  rnf (SourceLocation sf l c) = sf `deepseq` l `deepseq` c `deepseq` ()

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
