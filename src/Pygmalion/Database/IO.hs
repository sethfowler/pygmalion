{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Database.IO
( ensureDB
, withDB
, withTransaction
, beginTransaction
, endTransaction
, resetInclusions
, resetMetadata
, updateInclusion
, getInclusions
, getIncluders
, updateSourceFile
, getAllSourceFiles
, getCommandInfo
, getSimilarCommandInfo
, updateDef
, getDef
, updateOverride
, getOverrided
, getOverriders
, getCallers
, getCallees
, updateReference
, getReferenced
, getReferences
, enableTracing
, DBHandle
) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Data.Hashable
import Data.Int
import Data.String
import qualified Data.Text as T
import Database.SQLite.Simple
import System.FilePath.Posix

import Control.Exception.Labeled
import Pygmalion.Core
import Pygmalion.Log

{-
 - Summary of database changes and new tables that we need:
 - * Remove the old before inserting the new (see below).
 -
 - One last note: we need to keep in mind that we need to be able to _remove_
 - things for all these tables. The database update model needs to move from an
 - "insert only" approach to a "remove the old and insert the new" approach. It
 - causes more database churn, but I don't see an alternative if we don't want
 - the user to have to periodically wipe out their database.
 -}

-- General database manipulation functions. These are thin wrappers around the
-- underlying database implementation that also verify that the database is
-- configured according to the correct schema and enable foreign keys.
data DBHandle = DBHandle
    { conn                      :: Connection
    , beginTransactionStmt      :: Statement
    , endTransactionStmt        :: Statement
    , updateInclusionStmt       :: Statement
    , resetInclusionsStmt       :: Statement
    , getInclusionsStmt         :: Statement
    , getIncludersStmt          :: Statement
    , updateSourceFileStmt      :: Statement
    , getCommandInfoStmt        :: Statement
    , getSimilarCommandInfoStmt :: Statement
    , updateDefStmt             :: Statement
    , getDefStmt                :: Statement
    , updateOverrideStmt        :: Statement
    , getOverridedStmt          :: Statement
    , getOverridersStmt         :: Statement
    , getCallersStmt            :: Statement
    , getCalleesStmt            :: Statement
    , updateReferenceStmt       :: Statement
    , resetReferencesStmt       :: Statement
    , getReferencedStmt         :: Statement
    , getReferencesStmt         :: Statement
    , insertFileStmt            :: Statement
    , insertPathStmt            :: Statement
    , insertCommandStmt         :: Statement
    , insertArgsStmt            :: Statement
    }

ensureDB :: IO ()
ensureDB = withDB (const . return $ ())

withDB :: (DBHandle -> IO a) -> IO a
withDB f = bracket (openDB dbFile) closeDB f

withTransaction :: DBHandle -> IO a -> IO a
withTransaction h f = bracket (execStatement h beginTransactionStmt ())
                              (const $ execStatement h endTransactionStmt ())
                              (const f)

beginTransaction :: DBHandle -> IO ()
beginTransaction h = execStatement h beginTransactionStmt ()

endTransaction :: DBHandle -> IO ()
endTransaction h = execStatement h endTransactionStmt ()

resetMetadata :: DBHandle -> SourceFile -> IO ()
resetMetadata h sf = resetReferences h sf

openDB :: FilePath -> IO DBHandle
openDB db = labeledCatch "openDB" $ do
  c <- open db
  tuneDB c
  ensureSchema c
  h <- DBHandle c <$> openStatement c (mkQueryT beginTransactionSQL)
                  <*> openStatement c (mkQueryT endTransactionSQL)
                  <*> openStatement c (mkQueryT updateInclusionSQL)
                  <*> openStatement c (mkQueryT resetInclusionsSQL)
                  <*> openStatement c (mkQueryT getInclusionsSQL)
                  <*> openStatement c (mkQueryT getIncludersSQL)
                  <*> openStatement c (mkQueryT updateSourceFileSQL)
                  <*> openStatement c (mkQueryT getCommandInfoSQL)
                  <*> openStatement c (mkQueryT getSimilarCommandInfoSQL)
                  <*> openStatement c (mkQueryT updateDefSQL)
                  <*> openStatement c (mkQueryT getDefSQL)
                  <*> openStatement c (mkQueryT updateOverrideSQL)
                  <*> openStatement c (mkQueryT getOverridedSQL)
                  <*> openStatement c (mkQueryT getOverridersSQL)
                  <*> openStatement c (mkQueryT getCallersSQL)
                  <*> openStatement c (mkQueryT getCalleesSQL)
                  <*> openStatement c (mkQueryT updateReferenceSQL)
                  <*> openStatement c (mkQueryT resetReferencesSQL)
                  <*> openStatement c (mkQueryT getReferencedSQL)
                  <*> openStatement c (mkQueryT getReferencesSQL)
                  <*> openStatement c (mkQueryT insertFileSQL)
                  <*> openStatement c (mkQueryT insertPathSQL)
                  <*> openStatement c (mkQueryT insertCommandSQL)
                  <*> openStatement c (mkQueryT insertArgsSQL)
  return h

closeDB :: DBHandle -> IO ()
closeDB h = do
  closeStatement (beginTransactionStmt h)
  closeStatement (endTransactionStmt h)
  closeStatement (updateInclusionStmt h)
  closeStatement (resetInclusionsStmt h)
  closeStatement (getInclusionsStmt h)
  closeStatement (getIncludersStmt h)
  closeStatement (updateSourceFileStmt h)
  closeStatement (getCommandInfoStmt h)
  closeStatement (getSimilarCommandInfoStmt h)
  closeStatement (updateDefStmt h)
  closeStatement (getDefStmt h)
  closeStatement (updateOverrideStmt h)
  closeStatement (getOverridedStmt h)
  closeStatement (getOverridersStmt h)
  closeStatement (getCallersStmt h)
  closeStatement (getCalleesStmt h)
  closeStatement (updateReferenceStmt h)
  closeStatement (resetReferencesStmt h)
  closeStatement (getReferencedStmt h)
  closeStatement (getReferencesStmt h)
  closeStatement (insertFileStmt h)
  closeStatement (insertPathStmt h)
  closeStatement (insertCommandStmt h)
  closeStatement (insertArgsStmt h)
  close (conn h)

enableTracing :: Connection -> IO ()
enableTracing c = setTrace c (Just $ logDebug . T.unpack)

tuneDB :: Connection -> IO ()
tuneDB c = do
  -- Tradeoffs: We don't care if the database is corrupted on power loss, as
  -- this data can always be rebuilt from the original source files. However,
  -- especially given libclang's instability, we do want to avoid corruption
  -- because of crashes. We try to optimize as much as possible within those
  -- constraints.
  execute_ c "pragma synchronous = normal"
  execute_ c "pragma journal_mode = wal"
  execute_ c "pragma locking_mode = exclusive"
  execute_ c "pragma page_size = 4096"
  execute_ c "pragma cache_size = 10000"

beginTransactionSQL :: T.Text
beginTransactionSQL = "begin transaction"

endTransactionSQL :: T.Text
endTransactionSQL = "end transaction"

voidNextRow :: Statement -> IO (Maybe (Only Int64))
voidNextRow = nextRow

execStatement :: ToRow a => DBHandle -> (DBHandle -> Statement) -> a -> IO ()
execStatement h q params = do 
    void $ withBind stmt params $ (voidNextRow stmt)
    reset stmt
  where stmt = q h

execQuery :: (ToRow a, FromRow r) => DBHandle -> (DBHandle -> Statement) -> a -> IO [r]
execQuery h q params = do
    res <- withBind stmt params go
    reset stmt
    return res
  where
    stmt = q h
    go = do
      row <- nextRow stmt
      case row of
        Just r  -> (:) <$> (return r) <*> go
        Nothing -> return []
  
execSingleRowQuery :: (ToRow a, FromRow r) => DBHandle -> (DBHandle -> Statement) -> a -> IO (Maybe r)
execSingleRowQuery h q params = do
    res <- withBind stmt params $ nextRow stmt
    reset stmt
    return res
  where
    stmt = q h

mkQuery :: String -> Query
mkQuery = fromString

mkQueryT :: T.Text -> Query
mkQueryT = mkQuery . T.unpack

-- Schema and operations for the Metadata table.
dbToolName :: String
dbToolName = "pygmalion"

dbMajorVersion, dbMinorVersion :: Int64
dbMajorVersion = 0
dbMinorVersion = 13

defineMetadataTable :: Connection -> IO ()
defineMetadataTable c = execute_ c sql
  where sql = "create table if not exists Metadata(               \
              \ Tool varchar(16) primary key not null,            \
              \ MajorVersion integer zerofill unsigned not null,  \
              \ MinorVersion integer zerofill unsigned not null)"

getDBVersion :: Connection -> IO (Maybe (Int64, Int64))
getDBVersion c = do
    row <- query c sql params
    return $ case row of
              [version] -> Just version
              _         -> Nothing
  where sql = "select MajorVersion, MinorVersion from Metadata \
              \ where Tool = ?"
        params = Only dbToolName

setDBVersion :: Connection -> IO ()
setDBVersion c = execute c sql params
  where sql =  "insert into Metadata (Tool, MajorVersion, MinorVersion) \
              \ values (?, ?, ?)"
        params = (dbToolName, dbMajorVersion, dbMinorVersion)

-- Schema and operations for the Files table.
defineFilesTable :: Connection -> IO ()
defineFilesTable c = execute_ c sql
  where sql = "create table if not exists Files(           \
               \ Hash integer primary key unique not null, \
               \ Name varchar(2048) not null)"

insertFileSQL :: T.Text
insertFileSQL = "insert or ignore into Files (Name, Hash) values (?, ?)"

-- Schema and operations for the Inclusions table.
defineInclusionsTable :: Connection -> IO ()
defineInclusionsTable c = execute_ c sql
  where sql = "create table if not exists Inclusions( \
               \ File integer not null,               \
               \ Inclusion integer not null,          \
               \ Direct integer not null,             \
               \ primary key (File, Inclusion))"

updateInclusion :: DBHandle -> Inclusion -> IO ()
updateInclusion h (Inclusion sf hf d) = do
    let sfHash = hash sf
    let hfHash = hash hf
    execStatement h updateInclusionStmt (sfHash, hfHash, d)

updateInclusionSQL :: T.Text
updateInclusionSQL = "replace into Inclusions (File, Inclusion, Direct) \
                    \ values (?, ?, ?)"

resetInclusions :: DBHandle -> SourceFile -> IO ()
resetInclusions h sf = do
  let sfHash = hash sf
  execStatement h resetInclusionsStmt (Only $ sfHash)

resetInclusionsSQL :: T.Text
resetInclusionsSQL = "delete from Inclusions where File = ?"

getInclusions :: DBHandle -> SourceFile -> IO [CommandInfo]
getInclusions h sf = execQuery h getInclusionsStmt (Only $ hash sf)

getInclusionsSQL :: T.Text
getInclusionsSQL = "select F.Name, W.Path, C.Command, A.Args, S.Language, S.LastIndexed \
                   \ from Inclusions as I                                               \
                   \ join SourceFiles as S on I.Inclusion = S.File                      \
                   \ join Files as F on S.File = F.Hash                                 \
                   \ join Paths as W on S.WorkingDirectory = W.Hash                     \
                   \ join BuildCommands as C on S.BuildCommand = C.Hash                 \
                   \ join BuildArgs as A on S.BuildArgs = A.Hash                        \
                   \ where I.File = ?"

getIncluders :: DBHandle -> SourceFile -> IO [CommandInfo]
getIncluders h sf = execQuery h getIncludersStmt (Only $ hash sf)

getIncludersSQL :: T.Text
getIncludersSQL = "select F.Name, W.Path, C.Command, A.Args, S.Language, S.LastIndexed \
                  \ from Inclusions as I                                               \
                  \ join SourceFiles as S on I.File = S.File                           \
                  \ join Files as F on S.File = F.Hash                                 \
                  \ join Paths as W on S.WorkingDirectory = W.Hash                     \
                  \ join BuildCommands as C on S.BuildCommand = C.Hash                 \
                  \ join BuildArgs as A on S.BuildArgs = A.Hash                        \
                  \ where I.Inclusion = ?"

-- Schema and operations for the Paths table.
definePathsTable :: Connection -> IO ()
definePathsTable c = execute_ c sql
  where sql =  "create table if not exists Paths(          \
               \ Hash integer primary key unique not null, \
               \ Path varchar(2048) not null)"

insertPathSQL :: T.Text
insertPathSQL = "insert or ignore into Paths (Path, Hash) values (?, ?)"

-- Schema and operations for the BuildCommands table.
defineBuildCommandsTable :: Connection -> IO ()
defineBuildCommandsTable c = execute_ c sql
  where sql =  "create table if not exists BuildCommands(  \
               \ Hash integer primary key unique not null, \
               \ Command varchar(2048) not null)"

insertCommandSQL :: T.Text
insertCommandSQL = "insert or ignore into BuildCommands (Command, Hash) values (?, ?)"

-- Schema and operations for the BuildArgs table.
defineBuildArgsTable :: Connection -> IO ()
defineBuildArgsTable c = execute_ c sql
  where sql =  "create table if not exists BuildArgs(      \
               \ Hash integer primary key unique not null, \
               \ Args varchar(2048) not null)"

insertArgsSQL :: T.Text
insertArgsSQL = "insert or ignore into BuildArgs (Args, Hash) values (?, ?)"

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: Connection -> IO ()
defineSourceFilesTable c = execute_ c sql
  where sql =  "create table if not exists SourceFiles(         \
               \ File integer primary key unique not null,      \
               \ WorkingDirectory integer not null,             \
               \ BuildCommand integer not null,                 \
               \ BuildArgs integer not null,                    \
               \ Language integer not null,                     \
               \ LastIndexed integer zerofill unsigned not null)"

updateSourceFileSQL :: T.Text
updateSourceFileSQL = "replace into SourceFiles                                       \
                      \(File, WorkingDirectory, BuildCommand, BuildArgs, Language, LastIndexed) \
                      \values (?, ?, ?, ?, ?, ?)"

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h (CommandInfo sf wd (Command cmd args) lang t) = do
    let sfHash = hash sf
    execStatement h insertFileStmt (sf, sfHash)
    let wdHash = hash wd
    execStatement h insertPathStmt (wd, wdHash)
    let cmdHash = hash cmd
    execStatement h insertCommandStmt (cmd, cmdHash)
    let argsJoined = T.intercalate " " args
    let argsHash = hash argsJoined
    execStatement h insertArgsStmt (argsJoined, argsHash)
    execStatement h updateSourceFileStmt (sfHash, wdHash, cmdHash, argsHash, fromEnum lang, t)

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ (conn h) sql
  where sql = "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed \
              \ from SourceFiles                                               \
              \ join Files as F on SourceFiles.File = F.Hash                   \
              \ join Paths as W on SourceFiles.WorkingDirectory = W.Hash       \
              \ join BuildCommands as C on SourceFiles.BuildCommand = C.Hash   \
              \ join BuildArgs as A on SourceFiles.BuildArgs = A.Hash"

getCommandInfo :: DBHandle -> SourceFile -> IO (Maybe CommandInfo)
getCommandInfo h sf = execSingleRowQuery h getCommandInfoStmt (Only $ hash sf)

getCommandInfoSQL :: T.Text
getCommandInfoSQL = "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed \
                    \ from SourceFiles                                               \
                    \ join Files as F on SourceFiles.File = F.Hash                   \
                    \ join Paths as W on SourceFiles.WorkingDirectory = W.Hash       \
                    \ join BuildCommands as C on SourceFiles.BuildCommand = C.Hash   \
                    \ join BuildArgs as A on SourceFiles.BuildArgs = A.Hash          \
                    \ where F.Hash = ? limit 1"

-- Eventually this should be more statistical, but right now it will just
-- return an arbitrary file from the same directory.
getSimilarCommandInfo :: DBHandle -> SourceFile -> IO (Maybe CommandInfo)
getSimilarCommandInfo h sf = do
    let path = (++ "%") . normalise . takeDirectory . unSourceFile $ sf
    res <- execSingleRowQuery h getSimilarCommandInfoStmt (Only path)
    return $ case res of
              Just ci -> Just $ ci { ciSourceFile = sf }
              _       -> Nothing

getSimilarCommandInfoSQL :: T.Text
getSimilarCommandInfoSQL = "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed \
                           \ from SourceFiles                                               \
                           \ join Files as F on SourceFiles.File = F.Hash                   \
                           \ join Paths as W on SourceFiles.WorkingDirectory = W.Hash       \
                           \ join BuildCommands as C on SourceFiles.BuildCommand = C.Hash   \
                           \ join BuildArgs as A on SourceFiles.BuildArgs = A.Hash          \
                           \ where F.Name like ? limit 1"

-- Schema and operations for the Definitions table.
defineDefinitionsTable :: Connection -> IO ()
defineDefinitionsTable c = execute_ c sql
  where sql =  "create table if not exists Definitions(       \
               \ USRHash integer primary key unique not null, \
               \ Name varchar(2048) not null,                 \
               \ USR varchar(2048) not null,                  \
               \ File integer not null,                       \
               \ Line integer not null,                       \
               \ Col integer not null,                        \
               \ Kind integer not null)"

updateDef :: DBHandle -> DefInfo -> IO ()
updateDef h (DefInfo n u (SourceLocation sf l c) k) = do
    let usrHash = hash u
    let sfHash = hash sf
    execStatement h insertFileStmt (sf, sfHash) -- FIXME: Maybe only updateSourceFile should do this?
    let kind = fromEnum k
    execStatement h updateDefStmt (usrHash, n, u, sfHash, l, c, kind)

updateDefSQL :: T.Text
updateDefSQL = "replace into Definitions               \
               \ (USRHash, Name, USR, File, Line, Col, Kind) \
               \ values (?, ?, ?, ?, ?, ?, ?)"

getDef :: DBHandle -> USR -> IO (Maybe DefInfo)
getDef h usr = execSingleRowQuery h getDefStmt (Only $ hash usr)

getDefSQL :: T.Text
getDefSQL = "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind \
            \ from Definitions as D                              \
            \ join Files as F on D.File = F.Hash                 \
            \ where D.USRHash = ? limit 1"
  
-- Schema and operations for the Overrides table.
defineOverridesTable :: Connection -> IO ()
defineOverridesTable c = execute_ c sql
  where sql = "create table if not exists Overrides( \
               \ Definition integer not null,        \
               \ Overrided integer not null,         \
               \ primary key (Definition, Overrided))"

updateOverride :: DBHandle -> Override -> IO ()
updateOverride h (Override defUSR overrideUSR) = do
    let defUSRHash = hash defUSR
    let overrideUSRHash = hash overrideUSR
    execStatement h updateOverrideStmt (defUSRHash, overrideUSRHash)

updateOverrideSQL :: T.Text
updateOverrideSQL = "replace into Overrides (Definition, Overrided) \
                    \ values (?, ?)"

getOverrided :: DBHandle -> USR -> IO [DefInfo]
getOverrided h usr = execQuery h getOverridedStmt (Only $ hash usr)

getOverridedSQL :: T.Text
getOverridedSQL = "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind \
                  \ from Overrides as O                                \
                  \ join Definitions as D on O.Overrided = D.USRHash   \
                  \ join Files as F on D.File = F.Hash                 \
                  \ where O.Definition = ?"

getOverriders :: DBHandle -> USR -> IO [DefInfo]
getOverriders h usr = execQuery h getOverridersStmt (Only $ hash usr)

getOverridersSQL :: T.Text
getOverridersSQL = "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind \
                   \ from Overrides as O                                \
                   \ join Definitions as D on O.Definition = D.USRHash  \
                   \ join Files as F on D.File = F.Hash                 \
                   \ where O.Overrided = ?"

-- Schema and operations for the References table.
defineReferencesTable :: Connection -> IO ()
defineReferencesTable c = execute_ c sql
  where sql = "create table if not exists Refs(                          \
               \ File integer not null,                                  \
               \ Line integer not null,                                  \
               \ Col integer not null,                                   \
               \ EndLine integer not null,                               \
               \ EndCol integer not null,                                \
               \ RefKind integer not null,                               \
               \ RefContext integer not null,                            \
               \ Ref integer not null,                                   \
               \ primary key (File, Line, Col, EndLine, EndCol, RefKind))"

updateReference :: DBHandle -> Reference -> IO ()
updateReference h (Reference (SourceRange sf l c el ec) k ctxUSR refUSR) = do
    let sfHash = hash sf
    let refUSRHash = hash refUSR
    let kind = fromEnum k
    let ctxUSRHash = hash ctxUSR
    execStatement h updateReferenceStmt (sfHash, l, c, el, ec, kind, ctxUSRHash, refUSRHash)

updateReferenceSQL :: T.Text
updateReferenceSQL = "replace into Refs (File, Line, Col, EndLine, EndCol, RefKind, RefContext, Ref) \
                     \ values (?, ?, ?, ?, ?, ?, ?, ?)"

resetReferences :: DBHandle -> SourceFile -> IO ()
resetReferences h sf = do
  let sfHash = hash sf
  execStatement h resetReferencesStmt (Only sfHash)

resetReferencesSQL :: T.Text
resetReferencesSQL = "delete from Refs where File = ?"

getReferenced :: DBHandle -> SourceLocation -> IO [SourceReferenced]
getReferenced h (SourceLocation sf l c) =
  execQuery h getReferencedStmt (hash sf, l, l, c, l, c)

-- Note below that the columns of the reference are [Col, EndCol).
getReferencedSQL :: T.Text
getReferencedSQL = "select D.Name, D.USR, DF.Name, D.Line, D.Col, D.Kind,     \
                   \   RF.Name, R.Line, R.Col, R.EndLine, R.EndCol, R.RefKind \
                   \ from Refs as R                                           \
                   \ join Definitions as D on R.Ref = D.USRHash               \
                   \ join Files as DF on D.File = DF.Hash                     \
                   \ join Files as RF on R.File = RF.Hash                     \
                   \ where R.File = ? and                                     \
                   \   ((? between R.Line and R.EndLine) and                  \
                   \    (? > R.Line or ? >= R.Col) and                        \
                   \    (? < R.EndLine or ? < R.EndCol))" 

getReferences :: DBHandle -> USR -> IO [SourceReference]
getReferences h usr = execQuery h getReferencesStmt (Only $ hash usr)

getReferencesSQL :: T.Text
getReferencesSQL = "select F.Name, R.Line, R.Col, R.RefKind, D.Name               \
                   \ from Refs as R                                               \
                   \ join Files as F on R.File = F.Hash                           \
                   \ join Definitions as D on R.RefContext = D.USRHash            \
                   \ where R.Ref = ?                                              \
                   \ order by F.Name, R.Line, R.Col, R.EndLine desc, R.EndCol desc"

getCallers :: DBHandle -> USR -> IO [Invocation]
getCallers h usr = execQuery h getCallersStmt (fromEnum CallExpr, fromEnum MacroExpansion, hash usr)

getCallersSQL :: T.Text
getCallersSQL = "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, FR.Name, R.Line, R.Col \
                \ from Refs as R                                                             \
                \ join Files as FR on R.File = FR.Hash                                       \
                \ join Definitions as D on R.RefContext = D.USRHash                          \
                \ join Files as F on D.File = F.Hash                                         \
                \ where R.RefKind in (?, ?) and R.Ref = ?                                    \
                \ order by FR.Name, R.Line, R.Col"

getCallees :: DBHandle -> USR -> IO [DefInfo]
getCallees h usr = execQuery h getCalleesStmt (fromEnum CallExpr, fromEnum MacroExpansion, hash usr)

getCalleesSQL :: T.Text
getCalleesSQL = "select distinct D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind \
                \ from Refs as R                                              \
                \ join Definitions as D on R.Ref = D.USRHash                  \
                \ join Files as F on D.File = F.Hash                          \
                \ where R.RefKind in (?, ?) and R.RefContext = ?              \
                \ order by F.Name, D.Line, D.Col"

-- Checks that the database has the correct schema and sets it up if needed.
ensureSchema :: Connection -> IO ()
ensureSchema c = defineMetadataTable c
              >> defineFilesTable c
              >> defineInclusionsTable c
              >> definePathsTable c
              >> defineBuildCommandsTable c
              >> defineBuildArgsTable c
              >> defineSourceFilesTable c
              >> defineDefinitionsTable c
              >> defineOverridesTable c
              >> defineReferencesTable c
              >> ensureVersion c

ensureVersion :: Connection -> IO ()
ensureVersion c = getDBVersion c >>= checkVersion
  where
    checkVersion (Just (major, minor))
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion _ = setDBVersion c

throwDBVersionError :: Int64 -> Int64 -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)
