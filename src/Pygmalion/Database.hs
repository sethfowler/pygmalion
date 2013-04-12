{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database
( ensureDB
, withDB
, updateSourceFile
, getAllSourceFiles
, getCommandInfo
, getSimilarCommandInfo
, updateDef
, getDef
, enableTracing
, DBHandle
) where

import Control.Applicative
import Control.Exception(bracket)
import Control.Monad
import Data.Int
import Data.String
import qualified Data.Text as T
import Database.SQLite.Simple

import Control.Exception.Labeled
import Pygmalion.Core

-- General database manipulation functions. These are thin wrappers around the
-- underlying database implementation that also verify that the database is
-- configured according to the correct schema and enable foreign keys.
data DBHandle = DBHandle {
                  conn :: Connection,
                  updateSourceFileStmt :: Statement,
                  updateDefStmt :: Statement,
                  getFileStmt :: Statement,
                  insertFileStmt :: Statement,
                  getPathStmt :: Statement,
                  insertPathStmt :: Statement,
                  getCommandStmt :: Statement,
                  insertCommandStmt :: Statement,
                  getArgsStmt :: Statement,
                  insertArgsStmt :: Statement,
                  getKindStmt :: Statement,
                  insertKindStmt :: Statement
                }

ensureDB :: IO ()
ensureDB = withDB (const . return $ ())

withDB :: (DBHandle -> IO a) -> IO a
withDB f = bracket (openDB dbFile) closeDB f

openDB :: FilePath -> IO DBHandle
openDB db = labeledCatch "openDB" $ do
  c <- open db
  h <- DBHandle c <$> openStatement c (mkQueryT updateSourceFileSQL)
                  <*> openStatement c (mkQueryT updateDefSQL)
                  <*> openStatement c (mkQueryT getFileSQL)
                  <*> openStatement c (mkQueryT insertFileSQL)
                  <*> openStatement c (mkQueryT getPathSQL)
                  <*> openStatement c (mkQueryT insertPathSQL)
                  <*> openStatement c (mkQueryT getCommandSQL)
                  <*> openStatement c (mkQueryT insertCommandSQL)
                  <*> openStatement c (mkQueryT getArgsSQL)
                  <*> openStatement c (mkQueryT insertArgsSQL)
                  <*> openStatement c (mkQueryT getKindSQL)
                  <*> openStatement c (mkQueryT insertKindSQL)
  enableForeignKeyConstraints h
  ensureSchema h
  return h

closeDB :: DBHandle -> IO ()
closeDB h = do
  closeStatement (updateSourceFileStmt h)
  closeStatement (updateDefStmt h)
  closeStatement (getFileStmt h)
  closeStatement (insertFileStmt h)
  closeStatement (getPathStmt h)
  closeStatement (insertPathStmt h)
  closeStatement (getCommandStmt h)
  closeStatement (insertCommandStmt h)
  closeStatement (getArgsStmt h)
  closeStatement (insertArgsStmt h)
  closeStatement (getKindStmt h)
  closeStatement (insertKindStmt h)
  close (conn h)

enableForeignKeyConstraints :: DBHandle -> IO ()
enableForeignKeyConstraints h = execute_ (conn h) "pragma foreign_keys = on"

enableTracing :: DBHandle -> IO ()
enableTracing h = setTrace (conn h) (Just $ putStrLn . T.unpack)

voidNextRow :: Statement -> IO (Maybe (Only Int64))
voidNextRow = nextRow

voidWithBind :: ToRow params => Statement -> params -> IO () -> IO ()
voidWithBind s p a = void $ withBind s p a

-- Schema and operations for the Metadata table.
dbToolName :: String
dbToolName = "pygmalion"

dbMajorVersion, dbMinorVersion :: Int64
dbMajorVersion = 0
dbMinorVersion = 4

defineMetadataTable :: DBHandle -> IO ()
defineMetadataTable h = execute_ (conn h) 
  "create table if not exists Metadata(               \
  \ Tool varchar(2048) primary key not null,          \
  \ MajorVersion integer zerofill unsigned not null,  \
  \ MinorVersion integer zerofill unsigned not null)"

getDBVersion :: DBHandle -> IO (Maybe (Int64, Int64))
getDBVersion h = do
    row <- query (conn h) sql params
    return $ case row of
              [version] -> Just version
              _         -> Nothing
  where sql = "select MajorVersion, MinorVersion from Metadata \
              \where Tool = ?"
        params = Only dbToolName

setDBVersion :: DBHandle -> IO ()
setDBVersion h = execute (conn h) sql params
  where sql = "insert into Metadata (Tool, MajorVersion, MinorVersion) \
              \values (?, ?, ?)"
        params = (dbToolName, dbMajorVersion, dbMinorVersion)

-- Schema and operations for the Files table.
defineFilesTable :: DBHandle -> IO ()
defineFilesTable h = execute_ (conn h) sql
  where sql =  "create table if not exists Files(        \
               \ Id integer primary key unique not null, \
               \ Name varchar(2048) unique not null)"

-- Schema and operations for the Paths table.
definePathsTable :: DBHandle -> IO ()
definePathsTable h = execute_ (conn h) sql
  where sql =  "create table if not exists Paths(        \
               \ Id integer primary key unique not null, \
               \ Path varchar(2048) unique not null)"

-- Schema and operations for the BuildCommands table.
defineBuildCommandsTable :: DBHandle -> IO ()
defineBuildCommandsTable h = execute_ (conn h) sql
  where sql =  "create table if not exists BuildCommands(\
               \ Id integer primary key unique not null, \
               \ Command varchar(2048) unique not null)"

-- Schema and operations for the BuildArgs table.
defineBuildArgsTable :: DBHandle -> IO ()
defineBuildArgsTable h = execute_ (conn h) sql
  where sql =  "create table if not exists BuildArgs(    \
               \ Id integer primary key unique not null, \
               \ Args varchar(2048) unique not null)"

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: DBHandle -> IO ()
defineSourceFilesTable h = execute_ (conn h) sql
  where sql =  "create table if not exists SourceFiles(                  \
               \ File integer not null,                                  \
               \ Path integer not null,                                  \
               \ WorkingDirectory integer not null,                      \
               \ BuildCommand integer not null,                          \
               \ BuildArgs integer not null,                             \
               \ LastBuilt integer zerofill unsigned not null,           \
               \ foreign key(File) references Files(Id),                 \
               \ foreign key(Path) references Paths(Id),                 \
               \ foreign key(WorkingDirectory) references Paths(Id),     \
               \ foreign key(BuildCommand) references BuildCommands(Id), \
               \ foreign key(BuildArgs) references BuildArgs(Id),        \
               \ primary key(File, Path))"

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h (CommandInfo (SourceFile sn sp) wd (Command cmd args) t) = do
    --execute_ (conn h) "begin transaction"
    fileId <- getFileId h sn
    pathId <- getPathId h sp
    wdId <- getPathId h wd
    cmdId <- getCommandId h cmd
    argsId <- getArgsId h (T.intercalate " " args)
    void $ withBind (updateSourceFileStmt h) (fileId, pathId, wdId, cmdId, argsId, t) $ voidNextRow (updateSourceFileStmt h)
    --execute_ (conn h) "commit"

updateSourceFileSQL :: T.Text
updateSourceFileSQL = "replace into SourceFiles                                           \
                      \(File, Path, WorkingDirectory, BuildCommand, BuildArgs, LastBuilt) \
                      \values (?, ?, ?, ?, ?, ?)"

getFileId :: DBHandle -> T.Text -> IO Int64
getFileId h v = do
  existingId <- withBind (getFileStmt h) (Only v) $ nextRow (getFileStmt h)
  case existingId of
    Just (Only i) -> return i
    _             -> withBind (insertFileStmt h) (Only v) (voidNextRow (insertFileStmt h))
                  >> lastInsertRowId (conn h)

getPathId :: DBHandle -> T.Text -> IO Int64
getPathId h v = do
  existingId <- withBind (getPathStmt h) (Only v) $ nextRow (getPathStmt h)
  case existingId of
    Just (Only i) -> return i
    _             -> withBind (insertPathStmt h) (Only v) (voidNextRow (insertPathStmt h))
                  >> lastInsertRowId (conn h)

getCommandId :: DBHandle -> T.Text -> IO Int64
getCommandId h v = do
  existingId <- withBind (getCommandStmt h) (Only v) $ nextRow (getCommandStmt h)
  case existingId of
    Just (Only i) -> return i
    _             -> withBind (insertCommandStmt h) (Only v) (voidNextRow (insertCommandStmt h))
                  >> lastInsertRowId (conn h)

getArgsId :: DBHandle -> T.Text -> IO Int64
getArgsId h v = do
  existingId <- withBind (getArgsStmt h) (Only v) $ nextRow (getArgsStmt h)
  case existingId of
    Just (Only i) -> return i
    _             -> withBind (insertArgsStmt h) (Only v) (voidNextRow (insertArgsStmt h))
                  >> lastInsertRowId (conn h)

getKindId :: DBHandle -> T.Text -> IO Int64
getKindId h v = do
  existingId <- withBind (getKindStmt h) (Only v) $ nextRow (getKindStmt h)
  case existingId of
    Just (Only i) -> return i
    _             -> withBind (insertKindStmt h) (Only v) (voidNextRow (insertKindStmt h))
                  >> lastInsertRowId (conn h)

getFileSQL, insertFileSQL :: T.Text
getFileSQL = "select Id from Files where Name = ?"
insertFileSQL = "insert into Files (Name) values (?)"

getPathSQL, insertPathSQL :: T.Text
getPathSQL = "select Id from Paths where Path = ?"
insertPathSQL = "insert into Paths (Path) values (?)"

getCommandSQL, insertCommandSQL :: T.Text
getCommandSQL = "select Id from BuildCommands where Command = ?"
insertCommandSQL = "insert into BuildCommands (Command) values (?)"

getArgsSQL, insertArgsSQL :: T.Text
getArgsSQL = "select Id from BuildArgs where Args = ?"
insertArgsSQL = "insert into BuildArgs (Args) values (?)"

getKindSQL, insertKindSQL :: T.Text
getKindSQL = "select Id from Kinds where Kind = ?"
insertKindSQL = "insert into Kinds (Kind) values (?)"

mkQuery :: String -> Query
mkQuery = fromString

mkQueryT :: T.Text -> Query
mkQueryT = mkQuery . T.unpack

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ (conn h) sql
  where sql = "select F.Name, P.Path, W.Path, C.Command, A.Args, LastBuilt \
              \ from SourceFiles                                           \
              \ join Files as F on SourceFiles.File = F.Id                 \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id"

getCommandInfo :: DBHandle -> SourceFile -> IO (Maybe CommandInfo)
getCommandInfo h (SourceFile sn sp) = do
    row <- query (conn h) sql (sn, sp)
    return $ case row of
              (ci : _) -> Just ci
              _        -> Nothing
  where sql = "select F.Name, P.Path, W.Path, C.Command, A.Args, LastBuilt \
              \ from SourceFiles                                           \
              \ join Files as F on SourceFiles.File = F.Id                 \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id        \
              \ where F.Name = ? and P.Path = ? limit 1"

-- Eventually this should be more statistical, but right now it will just
-- return an arbitrary file from the same directory.
getSimilarCommandInfo :: DBHandle -> SourceFile -> IO (Maybe CommandInfo)
getSimilarCommandInfo h sf@(SourceFile _ sp) = do
    row <- query (conn h) sql (Only sp)
    return $ case row of
              (ci : _) -> Just $ withSourceFile ci sf
              _        -> Nothing
  where sql = "select F.Name, P.Path, W.Path, C.Command, A.Args, LastBuilt \
              \ from SourceFiles                                           \
              \ join Files as F on SourceFiles.File = F.Id                 \
              \ join Paths as P on SourceFiles.Path = P.Id                 \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Id     \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Id \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Id        \
              \ where P.Path = ? limit 1"

-- Schema and operations for the Kinds table.
defineKindsTable :: DBHandle -> IO ()
defineKindsTable h = execute_ (conn h) sql
  where sql =  "create table if not exists Kinds(        \
               \ Id integer primary key unique not null, \
               \ Kind varchar(2048) unique not null)"

-- Schema and operations for the Definitions table.
defineDefinitionsTable :: DBHandle -> IO ()
defineDefinitionsTable h = execute_ (conn h) sql
  where sql =  "create table if not exists Definitions(         \
               \ Name varchar(2048) not null,                   \
               \ USR varchar(2048) unique not null primary key, \
               \ File integer not null,                         \
               \ Path integer not null,                         \
               \ Line integer not null,                         \
               \ Column integer not null,                       \
               \ Kind integer not null,                         \
               \ foreign key(File) references Files(Id),        \
               \ foreign key(Path) references Paths(Id),        \
               \ foreign key(Kind) references Kinds(Id))"

updateDef :: DBHandle -> DefInfo -> IO ()
updateDef h (DefInfo (Identifier n u) (SourceLocation (SourceFile sn sp) l c) k) = do
    -- execute_ (conn h) "begin transaction"
    fileId <- getFileId h sn
    pathId <- getPathId h sp
    kindId <- getKindId h k
    void $ withBind (updateDefStmt h) (n, u, fileId, pathId, l, c, kindId) $ voidNextRow (updateDefStmt h)
    -- execute_ (conn h) "commit"

updateDefSQL :: T.Text
updateDefSQL = "replace into Definitions                     \
               \ (Name, USR, File, Path, Line, Column, Kind) \
               \ values (?, ?, ?, ?, ?, ?, ?)"

getDef :: DBHandle -> Identifier -> IO (Maybe DefInfo)
getDef h (Identifier _ usr) = do
    row <- query (conn h) sql (Only $ usr)
    return $ case row of
              (di : _) -> Just di
              _        -> Nothing
  where sql = "select D.Name, D.USR, F.Name, P.Path, D.Line, D.Column, K.Kind \
              \ from Definitions as D                                         \
              \ join Files as F on D.File = F.Id                              \
              \ join Paths as P on D.Path = P.Id                              \
              \ join Kinds as K on D.Kind = K.Id                              \
              \ where D.USR = ? limit 1"
  

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: DBHandle -> IO ()
ensureSchema h = defineMetadataTable h
              >> defineFilesTable h
              >> definePathsTable h
              >> defineBuildCommandsTable h
              >> defineBuildArgsTable h
              >> defineSourceFilesTable h
              >> defineKindsTable h
              >> defineDefinitionsTable h
              >> ensureVersion h

ensureVersion :: DBHandle -> IO ()
ensureVersion h = getDBVersion h >>= checkVersion
  where
    checkVersion (Just (major, minor))
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion _ = setDBVersion h

throwDBVersionError :: Int64 -> Int64 -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)
