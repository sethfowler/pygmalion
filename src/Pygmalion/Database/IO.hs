{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

module Pygmalion.Database.IO
( ensureDB
, withDB
, withTransaction
, beginTransaction
, endTransaction
, resetMetadata
, updateInclusion
, getInclusions
, getIncluders
, getIncluderInfo
, getInclusionHierarchy
, insertFileAndCheck
, updateSourceFile
, getAllSourceFiles
, getCommandInfo
, getSimilarCommandInfo
, updateDef
, getDef
, updateOverride
, getOverrided
, getOverriders
, getMembers
, getHierarchy
, getCallers
, getCallees
, updateReference
, getDeclReferenced
, getReferenced
, getReferences
, enableTracing
, DBHandle
) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.String
import qualified Data.Text as T
import qualified Database.SQLite3.Direct as Direct
import Database.SQLite.Simple
import System.FilePath.Posix

import Control.Exception.Labeled
import Pygmalion.Core
import Pygmalion.Database.Orphans ()
import Pygmalion.Dot
import Pygmalion.Hash
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
    , getDirectInclusionsStmt   :: Statement
    , getIncludersStmt          :: Statement
    , getIncluderInfoStmt       :: Statement
    , getDirectIncludersStmt    :: Statement
    , updateSourceFileStmt      :: Statement
    , getCommandInfoStmt        :: Statement
    , getSimilarCommandInfoStmt :: Statement
    , updateDefStmt             :: Statement
    , resetDefsStmt             :: Statement
    , getDefStmt                :: Statement
    , updateOverrideStmt        :: Statement
    , resetOverridesStmt        :: Statement
    , getOverridedStmt          :: Statement
    , getOverridersStmt         :: Statement
    , getMembersStmt            :: Statement
    , getCallersStmt            :: Statement
    , getCalleesStmt            :: Statement
    , updateReferenceStmt       :: Statement
    , resetReferencesStmt       :: Statement
    , getDeclReferencedStmt     :: Statement
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
resetMetadata h sf = do
  resetInclusions h sf
  resetOverrides h sf
  resetReferences h sf
  -- We need to reset definitions last since the rest of the reset code
  -- sometimes refers to the definitions table.
  resetDefs h sf

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
                  <*> openStatement c (mkQueryT getDirectInclusionsSQL)
                  <*> openStatement c (mkQueryT getIncludersSQL)
                  <*> openStatement c (mkQueryT getIncluderInfoSQL)
                  <*> openStatement c (mkQueryT getDirectIncludersSQL)
                  <*> openStatement c (mkQueryT updateSourceFileSQL)
                  <*> openStatement c (mkQueryT getCommandInfoSQL)
                  <*> openStatement c (mkQueryT getSimilarCommandInfoSQL)
                  <*> openStatement c (mkQueryT updateDefSQL)
                  <*> openStatement c (mkQueryT resetDefsSQL)
                  <*> openStatement c (mkQueryT getDefSQL)
                  <*> openStatement c (mkQueryT updateOverrideSQL)
                  <*> openStatement c (mkQueryT resetOverridesSQL)
                  <*> openStatement c (mkQueryT getOverridedSQL)
                  <*> openStatement c (mkQueryT getOverridersSQL)
                  <*> openStatement c (mkQueryT getMembersSQL)
                  <*> openStatement c (mkQueryT getCallersSQL)
                  <*> openStatement c (mkQueryT getCalleesSQL)
                  <*> openStatement c (mkQueryT updateReferenceSQL)
                  <*> openStatement c (mkQueryT resetReferencesSQL)
                  <*> openStatement c (mkQueryT getDeclReferencedSQL)
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
  closeStatement (getDirectInclusionsStmt h)
  closeStatement (getIncludersStmt h)
  closeStatement (getIncluderInfoStmt h)
  closeStatement (getDirectIncludersStmt h)
  closeStatement (updateSourceFileStmt h)
  closeStatement (getCommandInfoStmt h)
  closeStatement (getSimilarCommandInfoStmt h)
  closeStatement (updateDefStmt h)
  closeStatement (resetDefsStmt h)
  closeStatement (getDefStmt h)
  closeStatement (updateOverrideStmt h)
  closeStatement (resetOverridesStmt h)
  closeStatement (getOverridedStmt h)
  closeStatement (getOverridersStmt h)
  closeStatement (getMembersStmt h)
  closeStatement (getCallersStmt h)
  closeStatement (getCalleesStmt h)
  closeStatement (updateReferenceStmt h)
  closeStatement (resetReferencesStmt h)
  closeStatement (getDeclReferencedStmt h)
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
dbMinorVersion = 20

defineMetadataTable :: Connection -> IO ()
defineMetadataTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Metadata(              "
                       , "Tool varchar(16) primary key not null,            "
                       , "MajorVersion integer zerofill unsigned not null,  "
                       , "MinorVersion integer zerofill unsigned not null)" ]

getDBVersion :: Connection -> IO (Maybe (Int64, Int64))
getDBVersion c = do
    row <- query c (mkQueryT sql) params
    return $ case row of
              [version] -> Just version
              _         -> Nothing
  where sql = T.concat [ "select MajorVersion, MinorVersion from Metadata "
                       , "where Tool = ?" ]
        params = Only dbToolName

setDBVersion :: Connection -> IO ()
setDBVersion c = execute c (mkQueryT sql) params
  where sql =  T.concat [ "insert into Metadata (Tool, MajorVersion, MinorVersion) "
                        , "values (?, ?, ?)" ]
        params = (dbToolName, dbMajorVersion, dbMinorVersion)

-- Schema and operations for the Files table.
defineFilesTable :: Connection -> IO ()
defineFilesTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Files(         "
                       , "Hash integer primary key unique not null, "
                       , "Name varchar(2048) not null)" ]

insertFileAndCheck :: DBHandle -> SourceFile -> IO Bool
insertFileAndCheck h sf = do
  let sfHash = hash sf
  execStatement h insertFileStmt (sf, sfHash)
  (== 0) <$> Direct.changes (connectionHandle . conn $ h)

insertFileSQL :: T.Text
insertFileSQL = "insert or ignore into Files (Name, Hash) values (?, ?)"

-- Schema and operations for the Inclusions table.
defineInclusionsTable :: Connection -> IO ()
defineInclusionsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Inclusions( "
                       , "File integer not null,                 "
                       , "Inclusion integer not null,            "
                       , "Direct integer not null,               "
                       , "primary key (File, Inclusion))" ]

updateInclusion :: DBHandle -> Inclusion -> IO ()
updateInclusion h (Inclusion hci sf d) = do
  let hfHash = hash (ciSourceFile hci)
  let sfHash = hash sf
  execStatement h updateInclusionStmt (sfHash, hfHash, d)

updateInclusionSQL :: T.Text
updateInclusionSQL = T.concat
  [ "replace into Inclusions (File, Inclusion, Direct) "
  , "values (?, ?, ?)" ]

resetInclusions :: DBHandle -> SourceFile -> IO ()
resetInclusions h sf = do
  let sfHash = hash sf
  execStatement h resetInclusionsStmt (Only $ sfHash)

resetInclusionsSQL :: T.Text
resetInclusionsSQL = "delete from Inclusions where File = ?"

getInclusions :: DBHandle -> SourceFile -> IO [SourceFile]
getInclusions h sf = do
  is <- execQuery h getInclusionsStmt (Only $ hash sf)
  return $ map unwrapSourceFile is

getInclusionsSQL :: T.Text
getInclusionsSQL = T.concat
  [ "select F.Name                           "
  , "from Inclusions as I                    "
  , "join Files as F on I.Inclusion = F.Hash "
  , "where I.File = ?" ]

getDirectInclusions :: DBHandle -> SourceFile -> IO [SourceFile]
getDirectInclusions h sf = do
  is <- execQuery h getDirectInclusionsStmt (Only $ hash sf)
  return $ map unwrapSourceFile is

getDirectInclusionsSQL :: T.Text
getDirectInclusionsSQL = T.concat
  [ "select F.Name                                 "
  , "from Inclusions as I                          "
  , "join Files as F on I.Inclusion = F.Hash       "
  , "where I.Direct = 1 and I.File = ?" ]

getIncluders :: DBHandle -> SourceFile -> IO [SourceFile]
getIncluders h sf = do
  is <- execQuery h getIncludersStmt (Only $ hash sf)
  return $ map unwrapSourceFile is

getIncludersSQL :: T.Text
getIncludersSQL = T.concat
  [ "select F.Name                      "
  , "from Inclusions as I               "
  , "join Files as F on I.File = F.Hash "
  , "where I.Inclusion = ?" ]

getIncluderInfo :: DBHandle -> SourceFile -> IO [CommandInfo]
getIncluderInfo h sf = execQuery h getIncluderInfoStmt (Only $ hash sf)

getIncluderInfoSQL :: T.Text
getIncluderInfoSQL = T.concat
  [ "select F.Name, W.Path, C.Command, A.Args, S.Language, S.LastIndexed, S.SHA "
  , "from Inclusions as I                                                       "
  , "join SourceFiles as S on I.File = S.File                                   "
  , "join Files as F on S.File = F.Hash                                         "
  , "join Paths as W on S.WorkingPath = W.Hash                                  "
  , "join BuildCommands as C on S.BuildCommand = C.Hash                         "
  , "join BuildArgs as A on S.BuildArgs = A.Hash                                "
  , "where I.Inclusion = ?" ]

getDirectIncluders :: DBHandle -> SourceFile -> IO [SourceFile]
getDirectIncluders h sf = do
  is <- execQuery h getDirectIncludersStmt (Only $ hash sf)
  return $ map unwrapSourceFile is

getDirectIncludersSQL :: T.Text
getDirectIncludersSQL = T.concat
  [ "select F.Name                           "
  , "from Inclusions as I                    "
  , "join Files as F on I.File = F.Hash "
  , "where I.Direct = 1 and I.Inclusion = ?" ]

getInclusionHierarchy :: DBHandle -> SourceFile -> IO String
getInclusionHierarchy h sf = asDot <$> generateHierarchy
  where
    generateHierarchy = do
      let nid = fromIntegral . hash $ sf
      let node = mkHighlightedNode nid sf []
      let g = addNode node mkGraph

      -- Find includers.
      irs <- getDirectIncluders h sf
      g' <- foldM (expandHierarchy mkEdge getDirectIncluders nid) g irs

      -- Find inclusions.
      ics <- getDirectInclusions h sf
      g'' <- foldM (expandHierarchy mkReverseEdge getDirectInclusions nid) g' ics

      return g''
      
    expandHierarchy newEdgeF nextLevelF superNodeId g sf' = do
      let nid = fromIntegral . hash $ sf'
      let node = mkNode nid sf' []
      let edge = newEdgeF superNodeId nid
      let g' = (addEdge edge) . (addNode node) $ g
      
      is <- nextLevelF h sf'
      g'' <- foldM (expandHierarchy newEdgeF nextLevelF nid) g' is
      return g''

-- Schema and operations for the Paths table.
definePathsTable :: Connection -> IO ()
definePathsTable c = execute_ c (mkQueryT sql)
  where sql =  T.concat [ "create table if not exists Paths(         "
                        , "Hash integer primary key unique not null, "
                        , "Path varchar(2048) not null)" ]

insertPathSQL :: T.Text
insertPathSQL = "insert or ignore into Paths (Path, Hash) values (?, ?)"

-- Schema and operations for the BuildCommands table.
defineBuildCommandsTable :: Connection -> IO ()
defineBuildCommandsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists BuildCommands( "
                       , "Hash integer primary key unique not null, "
                       , "Command varchar(2048) not null)" ]

insertCommandSQL :: T.Text
insertCommandSQL = "insert or ignore into BuildCommands (Command, Hash) values (?, ?)"

-- Schema and operations for the BuildArgs table.
defineBuildArgsTable :: Connection -> IO ()
defineBuildArgsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists BuildArgs(     "
                       , "Hash integer primary key unique not null, "
                       , "Args varchar(2048) not null)" ]

insertArgsSQL :: T.Text
insertArgsSQL = "insert or ignore into BuildArgs (Args, Hash) values (?, ?)"

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: Connection -> IO ()
defineSourceFilesTable c = execute_ c (mkQueryT sql)
  where sql =  T.concat [ "create table if not exists SourceFiles(         "
                        , "File integer primary key unique not null,       "
                        , "WorkingPath integer not null,                   "
                        , "BuildCommand integer not null,                  "
                        , "BuildArgs integer not null,                     "
                        , "Language integer not null,                      "
                        , "LastIndexed integer zerofill unsigned not null, "
                        , "SHA varchar(20) not null)" ]

updateSourceFileSQL :: T.Text
updateSourceFileSQL = T.concat
  [ "replace into SourceFiles                                                 "
  , "(File, WorkingPath, BuildCommand, BuildArgs, Language, LastIndexed, SHA) "
  , "values (?, ?, ?, ?, ?, ?, ?)" ]

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h (CommandInfo sf wd cmd args lang t sha) = do
    let sfHash = hash sf
    execStatement h insertFileStmt (sf, sfHash)
    let wdHash = hash wd
    execStatement h insertPathStmt (wd, wdHash)
    let cmdHash = hash cmd
    execStatement h insertCommandStmt (cmd, cmdHash)
    let argsJoined = B.intercalate "\n" args
    let argsHash = hash argsJoined
    execStatement h insertArgsStmt (argsJoined, argsHash)
    execStatement h updateSourceFileStmt (sfHash, wdHash, cmdHash, argsHash, fromEnum lang, t, sha)

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ (conn h) (mkQueryT sql)
  where sql = T.concat [ "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed, SHA "
                       , "from SourceFiles                                                     "
                       , "join Files as F on SourceFiles.File = F.Hash                         "
                       , "join Paths as W on SourceFiles.WorkingPath = W.Hash                  "
                       , "join BuildCommands as C on SourceFiles.BuildCommand = C.Hash         "
                       , "join BuildArgs as A on SourceFiles.BuildArgs = A.Hash" ]

getCommandInfo :: DBHandle -> SourceFile -> IO (Maybe CommandInfo)
getCommandInfo h sf = execSingleRowQuery h getCommandInfoStmt (Only $ hash sf)

getCommandInfoSQL :: T.Text
getCommandInfoSQL = T.concat
  [ "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed, SHA "
  , "from SourceFiles                                                     "
  , "join Files as F on SourceFiles.File = F.Hash                         "
  , "join Paths as W on SourceFiles.WorkingPath = W.Hash                  "
  , "join BuildCommands as C on SourceFiles.BuildCommand = C.Hash         "
  , "join BuildArgs as A on SourceFiles.BuildArgs = A.Hash                "
  , "where F.Hash = ? limit 1" ]

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
getSimilarCommandInfoSQL = T.concat
  [ "select F.Name, W.Path, C.Command, A.Args, Language, LastIndexed, SHA "
  , "from SourceFiles                                                     "
  , "join Files as F on SourceFiles.File = F.Hash                         "
  , "join Paths as W on SourceFiles.WorkingPath = W.Hash                  "
  , "join BuildCommands as C on SourceFiles.BuildCommand = C.Hash         "
  , "join BuildArgs as A on SourceFiles.BuildArgs = A.Hash                "
  , "where F.Name like ? limit 1" ]

-- Schema and operations for the Definitions table.
defineDefinitionsTable :: Connection -> IO ()
defineDefinitionsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Definitions(      "
                       , "USRHash integer primary key unique not null, "
                       , "Name varchar(2048) not null,                 "
                       , "USR varchar(2048) not null,                  "
                       , "File integer not null,                       "
                       , "Line integer not null,                       "
                       , "Col integer not null,                        "
                       , "Kind integer not null,                       "
                       , "Context integer not null)" ]

updateDef :: DBHandle -> DefUpdate -> IO ()
updateDef h (DefUpdate n u sfHash l c k ctx) = do
    let usrHash = hash u
    let kind = fromEnum k
    execStatement h updateDefStmt (usrHash, n, u, sfHash, l, c, kind, ctx)

updateDefSQL :: T.Text
updateDefSQL = T.concat
  [ "replace into Definitions                             "
  , "(USRHash, Name, USR, File, Line, Col, Kind, Context) "
  , "values (?, ?, ?, ?, ?, ?, ?, ?)" ]

resetDefs :: DBHandle -> SourceFile -> IO ()
resetDefs h sf = do
  let sfHash = hash sf
  execStatement h resetDefsStmt (Only sfHash)

resetDefsSQL :: T.Text
resetDefsSQL = "delete from Definitions where File = ?"

getDef :: DBHandle -> SourceLocation -> IO [DefInfo]
getDef h sl = go =<< getReferenced h sl
  where
    go Nothing   = return []  
    go (Just sr) = do
      mayDef <- execSingleRowQuery h getDefStmt (Only . hash . diUSR . sdDef $ sr)
      case (mayDef, sdKind sr) of
        (Nothing, _) -> return []
        (Just def, DynamicCallExpr) -> do os <- getTransitiveOverriders def
                                          return (def : os)
        (Just def, _) -> return [def]

    getTransitiveOverriders :: DefInfo -> IO [DefInfo]
    getTransitiveOverriders di = do
      -- This is a slow algorithm. Rewrite.
      os <- (getOverriders' h (diUSR di)) :: IO [DefInfo]
      os' <- (mapM getTransitiveOverriders os) :: IO [[DefInfo]]
      return $ os ++ (concat os')

getDefSQL :: T.Text
getDefSQL = T.concat
  [ "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, "
  , "  coalesce(C.USR, x'')                               "
  , "from Definitions as D                                "
  , "join Files as F on D.File = F.Hash                   "
  , "left join Definitions as C on D.Context = C.USRHash  "
  , "where D.USRHash = ? limit 1" ]
  
-- Schema and operations for the Overrides table.
defineOverridesTable :: Connection -> IO ()
defineOverridesTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Overrides(           "
                       , "Definition integer primary key unique not null, "
                       , "Overrided integer not null)" ]

updateOverride :: DBHandle -> Override -> IO ()
updateOverride h (Override defUSRHash overrideUSRHash) =
    execStatement h updateOverrideStmt (defUSRHash, overrideUSRHash)

updateOverrideSQL :: T.Text
updateOverrideSQL = T.concat
  [ "replace into Overrides (Definition, Overrided) "
  , "values (?, ?)" ]

resetOverrides :: DBHandle -> SourceFile -> IO ()
resetOverrides h sf = do
  let sfHash = hash sf
  execStatement h resetOverridesStmt (Only sfHash)

resetOverridesSQL :: T.Text
resetOverridesSQL = T.concat
  [ "delete from Overrides where Definition in         "
  , "(select USRHash from Definitions where File = ?)" ]

getOverrided :: DBHandle -> SourceLocation -> IO [DefInfo]
getOverrided h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = hash . diUSR . sdDef $ sd in
               execQuery h getOverridedStmt (Only usrHash)

getOverrided' :: DBHandle -> USR -> IO [DefInfo]
getOverrided' h usr = execQuery h getOverridedStmt (Only . hash $ usr)

getOverridedSQL :: T.Text
getOverridedSQL = T.concat
  [ "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, "
  , "  coalesce(C.USR, x'')                               "
  , "from Overrides as O                                  "
  , "join Definitions as D on O.Overrided = D.USRHash     "
  , "join Files as F on D.File = F.Hash                   "
  , "left join Definitions as C on D.Context = C.USRHash  "
  , "where O.Definition = ?" ]

getOverriders :: DBHandle -> SourceLocation -> IO [DefInfo]
getOverriders h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = hash . diUSR . sdDef $ sd in
               execQuery h getOverridersStmt (Only usrHash)

getOverriders' :: DBHandle -> USR -> IO [DefInfo]
getOverriders' h usr = execQuery h getOverridersStmt (Only . hash $ usr)

getOverridersSQL :: T.Text
getOverridersSQL = T.concat
  [ "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, "
  , "  coalesce(C.USR, x'')                               "
  , "from Overrides as O                                  "
  , "join Definitions as D on O.Definition = D.USRHash    "
  , "join Files as F on D.File = F.Hash                   "
  , "left join Definitions as C on D.Context = C.USRHash  "
  , "where O.Overrided = ?" ]

getHierarchy :: DBHandle -> SourceLocation -> IO String
getHierarchy h sl = do
    maySd <- getReferenced h sl
    case maySd of
      Nothing -> return []
      Just sd -> asDot <$> generateHierarchy (sdDef sd)
  where
    generateHierarchy di = do
      let usr = diUSR di
      let name = diIdentifier di
      let node = mkHighlightedNode (fromIntegral . hash $ usr) name []
      let g = addNode node mkGraph

      -- Find subclasses.
      ovs <- getOverriders' h usr
      g' <- foldM (expandHierarchy mkEdge getOverriders' di) g ovs

      -- Find superclasses.
      ods <- getOverrided' h usr
      g'' <- foldM (expandHierarchy mkReverseEdge getOverrided' di) g' ods

      return g''
      
    expandHierarchy newEdgeF nextLevelF superDI g di = do
      let usr = diUSR di
      let usrHash = fromIntegral . hash $ usr
      let node = mkNode usrHash (diIdentifier di) []
      let edge = newEdgeF (fromIntegral . hash . diUSR $ superDI) usrHash
      let g' = (addEdge edge) . (addNode node) $ g
      
      os <- nextLevelF h usr
      g'' <- foldM (expandHierarchy newEdgeF nextLevelF di) g' os
      return g''

getMembers :: DBHandle -> SourceLocation -> IO [DefInfo]
getMembers h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = hash . diUSR . sdDef $ sd in
               execQuery h getMembersStmt (Only usrHash)

getMembersSQL :: T.Text
getMembersSQL = T.concat
  [ "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, C.USR "
  , "from Definitions as D                                      "
  , "join Files as F on D.File = F.Hash                         "
  , "join Definitions as C on D.Context = C.USRHash             "
  , "where D.Context = ?" ]

-- Schema and operations for the References table.
defineReferencesTable :: Connection -> IO ()
defineReferencesTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
  where
    sql = T.concat [ "create table if not exists Refs(           "
                   , "RefId integer unique primary key not null, "
                   , "File integer not null,                     "
                   , "Line integer not null,                     "
                   , "Col integer not null,                      "
                   , "EndLine integer not null,                  "
                   , "EndCol integer not null,                   "
                   , "RefKind integer not null,                  "
                   , "RefVia integer not null,                   "
                   , "RefDecl integer not null,                  "
                   , "RefContext integer not null,               "
                   , "Ref integer not null)" ]
    indexSQL = "create index if not exists RefsFileIndex on Refs(File)"

updateReference :: DBHandle -> ReferenceUpdate -> IO ()
updateReference h ReferenceUpdate {..} = do
  let kind = fromEnum rfuKind
  logInfo "About to handle refupdate"
  execStatement h updateReferenceStmt (rfuId, rfuFileHash, rfuLine, rfuCol,
                                       rfuEndLine, rfuEndCol, kind,
                                       rfuViaHash, rfuDeclHash,
                                       rfuContextHash, rfuUSRHash)
  logInfo "Finished with refupdate"
  
updateReferenceSQL :: T.Text
updateReferenceSQL = T.concat
  [ "replace into Refs                            "
  , " (RefId, File, Line, Col, EndLine, EndCol,   "
  , "  RefKind, RefVia, RefDecl, RefContext, Ref) "
  , "values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" ]

resetReferences :: DBHandle -> SourceFile -> IO ()
resetReferences h sf = do
  let sfHash = hash sf
  execStatement h resetReferencesStmt (Only sfHash)

resetReferencesSQL :: T.Text
resetReferencesSQL = "delete from Refs where File = ?"

-- FIXME: Crappy implementation.
getDeclReferenced :: DBHandle -> SourceLocation -> IO [DefInfo]
getDeclReferenced h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> execQuery h getDeclReferencedStmt (Only $ sdDeclHash sd)

getDeclReferencedSQL :: T.Text
getDeclReferencedSQL = T.concat
  [ "select D.Name, D.USR, RF.Name, R.Line, R.Col, R.RefKind, coalesce(C.USR, x'') "
  , "from Refs as R                                                                "
  , "join Definitions as D on R.Ref = D.USRHash                                    "
  , "join Files as RF on R.File = RF.Hash                                          "
  , "left join Definitions as C on R.RefContext = C.USRHash                        "
  , "where R.RefId = ?" ]

getReferenced :: DBHandle -> SourceLocation -> IO (Maybe SourceReferenced)
getReferenced h (SourceLocation sf l c) = do
    rs <- execQuery h getReferencedStmt (hash sf, l, l, c, l, c)

    when (multipleItems rs) $ do
      logDebug "Multiple referenced entities:"
      mapM_ (logDebug . show) rs

    narrowReferenced rs

  where

    multipleItems []       = False
    multipleItems (_ : []) = False
    multipleItems _        = True

-- Note below that the columns of the reference are [Col, EndCol).
getReferencedSQL :: T.Text
getReferencedSQL = T.concat
  [ "select D.Name, D.USR, DF.Name, D.Line, D.Col, D.Kind,        "
  , "  coalesce(C.USR, x''), RF.Name, R.Line, R.Col, R.EndLine,   "
  , "  R.EndCol, R.RefKind, R.RefVia, R.RefDecl                   "
  , "from Refs as R                                               "
  , "join Definitions as D on R.Ref = D.USRHash                   "
  , "join Files as DF on D.File = DF.Hash                         "
  , "join Files as RF on R.File = RF.Hash                         "
  , "left join Definitions as C on D.Context = C.USRHash          "
  , "where R.File = ? and                                         "
  , "  ((? between R.Line and R.EndLine) and                      "
  , "   (? > R.Line or ? >= R.Col) and                            "
  , "   (? < R.EndLine or ? < R.EndCol))" ]


narrowReferenced :: [SourceReferenced] -> IO (Maybe SourceReferenced)
narrowReferenced = return . filterNarrowest . filterCall . filterExpansion
  where
    filterExpansion rs = let exps = filter ((== MacroExpansion) . sdKind) rs in
                         if null exps then rs else exps
    -- filterCall is a hack until #109 gets fixed.
    filterCall rs      = case filterNarrowest rs of
                           Nothing -> []
                           Just n  -> if sdKind n /= DynamicCallExpr &&
                                         any ((== DynamicCallExpr) . sdKind) rs
                                      then filter (/= n) rs
                                      else rs
    filterNarrowest [] = Nothing
    filterNarrowest rs = Just $ minimumBy (comparing $ rangeSize . sdRange) rs

    rangeSize (SourceRange _ l c el ec) = (el - l, ec - c)

getReferences :: DBHandle -> SourceLocation -> IO [SourceReference]
getReferences h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = hash . diUSR . sdDef $ sd in
               execQuery h getReferencesStmt (Only usrHash)

getReferencesSQL :: T.Text
getReferencesSQL = T.concat
  [ "select F.Name, R.Line, R.Col, R.RefKind, D.Name              "
  , "from Refs as R                                               "
  , "join Files as F on R.File = F.Hash                           "
  , "join Definitions as D on R.RefContext = D.USRHash            "
  , "where R.Ref = ?                                              "
  , "order by F.Name, R.Line, R.Col, R.EndLine desc, R.EndCol desc" ]

getCallers :: DBHandle -> SourceLocation -> IO [Invocation]
getCallers h sl = do
    maySd <- getReferenced h sl
    case maySd of
      Nothing -> return []
      Just sd -> go CallExpr DynamicCallExpr MacroExpansion (diUSR . sdDef $ sd)
  where
    go !k1 !k2 !k3 !nextUSR = do
      is <- execQuery h getCallersStmt (fromEnum k1, fromEnum k2, fromEnum k3, hash nextUSR)
      os <- getOverrided' h nextUSR
      foldM accumCallers is os
    accumCallers :: [Invocation] -> DefInfo -> IO [Invocation]
    accumCallers is o = do
      is' <- go DynamicCallExpr DynamicCallExpr MacroExpansion (diUSR o)
      return $ is' ++ is

getCallersSQL :: T.Text
getCallersSQL = T.concat
  [ "select D.Name, D.USR, F.Name, D.Line, D.Col, D.Kind, "
  , "       coalesce(C.USR, x''), FR.Name, R.Line, R.Col  "
  , "from Refs as R                                       "
  , "join Files as FR on R.File = FR.Hash                 "
  , "join Definitions as D on R.RefContext = D.USRHash    "
  , "join Files as F on D.File = F.Hash                   "
  , "left join Definitions as C on D.Context = C.USRHash  "
  , "where R.Ref = ? and R.RefKind in (?, ?, ?)           "
  , "order by FR.Name, R.Line, R.Col" ]

getCallees :: DBHandle -> SourceLocation -> IO [DefInfo]
getCallees h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = hash . diUSR . sdDef $ sd in
               execQuery h getCalleesStmt (fromEnum CallExpr, fromEnum MacroExpansion, usrHash)

getCalleesSQL :: T.Text
getCalleesSQL = T.concat
  [ "select distinct D.Name, D.USR, F.Name, D.Line, D.Col, "
  , "  D.Kind, coalesce(C.USR, x'')                        "
  , "from Refs as R                                        "
  , "join Definitions as D on R.Ref = D.USRHash            "
  , "join Files as F on D.File = F.Hash                    "
  , "left join Definitions as C on D.Context = C.USRHash   "
  , "where R.RefContext = ? and R.RefKind in (?, ?)        "
  , "order by F.Name, D.Line, D.Col" ]

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
