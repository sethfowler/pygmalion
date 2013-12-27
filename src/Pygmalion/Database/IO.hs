{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

module Pygmalion.Database.IO
( ensureDB
, ensureStagingDB
, withDB
, withTransaction
, beginTransaction
, endTransaction
, commitStagedUpdates
, resetMetadata
, updateFile
, getLastMTime
, getAllFiles
, updateInclusion
, getInclusions
, getIncluders
, getDirectIncluders
, getDirectIncluderHashes
, getDirectInclusions
, getDirectInclusionHashes
, getInclusionHierarchy
, updateSourceFile
, getAllSourceFiles
, getCommandInfo
, getSimilarCommandInfo
, updateDef
, stageDefUpdate
, getDef
, updateOverride
, stageOverrideUpdate
, getOverrided
, getOverriders
, getMembers
, getHierarchy
, getCallers
, getCallees
, updateReference
, stageReferenceUpdate
, getDeclReferenced
, getReferenced
, getReferences
, enableTracing
, DBHandle
) where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Monad
import qualified Data.ByteString as B
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.String
import qualified Data.Text as T
import Database.SQLite.Simple
import System.FilePath.Posix

import Control.Exception.Labeled
import Pygmalion.Core
import Pygmalion.Database.Orphans ()
import Pygmalion.Dot
import Pygmalion.Log

-- General database manipulation functions. These are thin wrappers around the
-- underlying database implementation that also verify that the database is
-- configured according to the correct schema and enable foreign keys.
data DBHandle = DBHandle
    { conn                            :: Connection
    , beginTransactionStmt            :: Statement
    , endTransactionStmt              :: Statement
    , updateInclusionStmt             :: Statement
    , resetInclusionsStmt             :: Statement
    , getDirectInclusionsStmt         :: Statement
    , getDirectInclusionHashesStmt    :: Statement
    , getDirectIncludersStmt          :: Statement
    , getDirectIncluderHashesStmt     :: Statement
    , updateSourceFileStmt            :: Statement
    , getCommandInfoStmt              :: Statement
    , getSimilarCommandInfoStmt       :: Statement
    , updateDefStmt                   :: Statement
    , stageDefUpdateStmt              :: Statement
    , resetDefsStmt                   :: Statement
    , getDefStmt                      :: Statement
    , updateOverrideStmt              :: Statement
    , stageOverrideUpdateStmt         :: Statement
    , resetOverridesStmt              :: Statement
    , getOverridedStmt                :: Statement
    , getOverridersStmt               :: Statement
    , getMembersStmt                  :: Statement
    , getCallersStmt                  :: Statement
    , getCalleesStmt                  :: Statement
    , updateReferenceStmt             :: Statement
    , stageReferenceUpdateStmt        :: Statement
    , resetReferencesStmt             :: Statement
    , getDeclReferencedStmt           :: Statement
    , getDeclsReferencedInFileStmt    :: Statement
    , getReferencedStmt               :: Statement
    , getReferencesStmt               :: Statement
    , updateFileStmt                  :: Statement
    , getLastMTimeStmt                :: Statement
    , insertPathStmt                  :: Statement
    , insertCommandStmt               :: Statement
    , insertArgsStmt                  :: Statement
    }

ensureDB :: IO ()
ensureDB = withDB (const . return $ ())

withDB :: (DBHandle -> IO a) -> IO a
withDB = bracket (openDB dbFile) closeDB

withTransaction :: DBHandle -> IO a -> IO a
withTransaction h = bracket_ (execStatement h beginTransactionStmt ())
                             (execStatement h endTransactionStmt ())

beginTransaction :: DBHandle -> IO ()
beginTransaction h = execStatement h beginTransactionStmt ()

endTransaction :: DBHandle -> IO ()
endTransaction h = execStatement h endTransactionStmt ()

commitStagedUpdates :: DBHandle -> IO ()
commitStagedUpdates h = do
  let c = conn h
  execute_ c "replace into Definitions select * from Staging.DefinitionsStaging"
  execute_ c "replace into Overrides select * from Staging.OverridesStaging"
  execute_ c "replace into Refs select * from Staging.RefsStaging"
  execute_ c "drop table Staging.DefinitionsStaging"
  execute_ c "drop table Staging.OverridesStaging"
  execute_ c "drop table Staging.RefsStaging"
  defineDefinitionsStagingTable' c
  defineOverridesStagingTable' c
  defineReferencesStagingTable' c
 
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
  execute_ c (mkQueryT $ T.concat ["attach database \"", T.pack stagingDBFile, "\" as Staging"])
  DBHandle c <$> openStatement c (mkQueryT beginTransactionSQL)
             <*> openStatement c (mkQueryT endTransactionSQL)
             <*> openStatement c (mkQueryT updateInclusionSQL)
             <*> openStatement c (mkQueryT resetInclusionsSQL)
             <*> openStatement c (mkQueryT getDirectInclusionsSQL)
             <*> openStatement c (mkQueryT getDirectInclusionHashesSQL)
             <*> openStatement c (mkQueryT getDirectIncludersSQL)
             <*> openStatement c (mkQueryT getDirectIncluderHashesSQL)
             <*> openStatement c (mkQueryT updateSourceFileSQL)
             <*> openStatement c (mkQueryT getCommandInfoSQL)
             <*> openStatement c (mkQueryT getSimilarCommandInfoSQL)
             <*> openStatement c (mkQueryT updateDefSQL)
             <*> openStatement c (mkQueryT stageDefUpdateSQL)
             <*> openStatement c (mkQueryT resetDefsSQL)
             <*> openStatement c (mkQueryT getDefSQL)
             <*> openStatement c (mkQueryT updateOverrideSQL)
             <*> openStatement c (mkQueryT stageOverrideUpdateSQL)
             <*> openStatement c (mkQueryT resetOverridesSQL)
             <*> openStatement c (mkQueryT getOverridedSQL)
             <*> openStatement c (mkQueryT getOverridersSQL)
             <*> openStatement c (mkQueryT getMembersSQL)
             <*> openStatement c (mkQueryT getCallersSQL)
             <*> openStatement c (mkQueryT getCalleesSQL)
             <*> openStatement c (mkQueryT updateReferenceSQL)
             <*> openStatement c (mkQueryT stageReferenceUpdateSQL)
             <*> openStatement c (mkQueryT resetReferencesSQL)
             <*> openStatement c (mkQueryT getDeclReferencedSQL)
             <*> openStatement c (mkQueryT getDeclsReferencedInFileSQL)
             <*> openStatement c (mkQueryT getReferencedSQL)
             <*> openStatement c (mkQueryT getReferencesSQL)
             <*> openStatement c (mkQueryT updateFileSQL)
             <*> openStatement c (mkQueryT getLastMTimeSQL)
             <*> openStatement c (mkQueryT insertPathSQL)
             <*> openStatement c (mkQueryT insertCommandSQL)
             <*> openStatement c (mkQueryT insertArgsSQL)

closeDB :: DBHandle -> IO ()
closeDB h = do
  closeStatement (beginTransactionStmt h)
  closeStatement (endTransactionStmt h)
  closeStatement (updateInclusionStmt h)
  closeStatement (resetInclusionsStmt h)
  closeStatement (getDirectInclusionsStmt h)
  closeStatement (getDirectInclusionHashesStmt h)
  closeStatement (getDirectIncludersStmt h)
  closeStatement (getDirectIncluderHashesStmt h)
  closeStatement (updateSourceFileStmt h)
  closeStatement (getCommandInfoStmt h)
  closeStatement (getSimilarCommandInfoStmt h)
  closeStatement (updateDefStmt h)
  closeStatement (stageDefUpdateStmt h)
  closeStatement (resetDefsStmt h)
  closeStatement (getDefStmt h)
  closeStatement (updateOverrideStmt h)
  closeStatement (stageOverrideUpdateStmt h)
  closeStatement (resetOverridesStmt h)
  closeStatement (getOverridedStmt h)
  closeStatement (getOverridersStmt h)
  closeStatement (getMembersStmt h)
  closeStatement (getCallersStmt h)
  closeStatement (getCalleesStmt h)
  closeStatement (updateReferenceStmt h)
  closeStatement (stageReferenceUpdateStmt h)
  closeStatement (resetReferencesStmt h)
  closeStatement (getDeclReferencedStmt h)
  closeStatement (getDeclsReferencedInFileStmt h)
  closeStatement (getReferencedStmt h)
  closeStatement (getReferencesStmt h)
  closeStatement (updateFileStmt h)
  closeStatement (getLastMTimeStmt h)
  closeStatement (insertPathStmt h)
  closeStatement (insertCommandStmt h)
  closeStatement (insertArgsStmt h)
  close (conn h)

ensureStagingDB :: IO ()
ensureStagingDB = do
  c <- open stagingDBFile
  tuneDB c
  ensureSchema c
  close c

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

voidNextRow :: Statement -> IO (Maybe (Only Int))
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

dbMajorVersion, dbMinorVersion :: Int
dbMajorVersion = 0
dbMinorVersion = 27

defineMetadataTable :: Connection -> IO ()
defineMetadataTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists Metadata(              "
                       , "Tool text primary key not null,                   "
                       , "MajorVersion integer zerofill unsigned not null,  "
                       , "MinorVersion integer zerofill unsigned not null)" ]

getDBVersion :: Connection -> IO (Maybe (Int, Int))
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
defineFilesTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
  where
    sql = T.concat [ "create table if not exists Files(          "
                   , "Hash integer primary key unique not null,  "
                   , "Name text not null collate nocase,         "
                   , "LastMTime integer not null,                "
                   , "VersionHash integer not null)"             ]
    indexSQL = "create index if not exists FilesNameIndex on Files(Name collate nocase)"

updateFile :: DBHandle -> SourceFile -> Time -> TimeHash -> IO ()
updateFile h sf mt vh = execStatement h updateFileStmt (sf, stableHash sf, mt, vh)

updateFileSQL :: T.Text
updateFileSQL =
  "replace into Files (Name, Hash, LastMTime, VersionHash) values (?, ?, ?, ?)"

getLastMTime :: DBHandle -> SourceFileHash -> IO (Maybe Time)
getLastMTime h sfHash = do
  v <- execSingleRowQuery h getLastMTimeStmt (Only sfHash)
  case v of
    Just (Only t) -> return $ Just t
    Nothing       -> return Nothing

getLastMTimeSQL :: T.Text
getLastMTimeSQL = "select LastMTime from Files where Hash=?"

getAllFiles :: DBHandle -> IO [(SourceFile, Time, TimeHash)]
getAllFiles h = query_ (conn h) (mkQueryT sql)
  where
    sql = "select Name, LastMTime, VersionHash from Files" 

-- Schema and operations for the Inclusions table.
defineInclusionsTable :: Connection -> IO ()
defineInclusionsTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
  where
    sql = T.concat [ "create table if not exists Inclusions( "
                   , "Inclusion integer not null,            "
                   , "Includer integer not null,             "
                   , "primary key (Includer, Inclusion))"    ]
    indexSQL = "create index if not exists InclusionsInclusionIndex on Inclusions(Inclusion)"

updateInclusion :: DBHandle -> Inclusion -> IO ()
updateInclusion h (Inclusion inclusion includerHash) = do
  let incHash = stableHash inclusion
  execStatement h updateInclusionStmt (incHash, includerHash)

updateInclusionSQL :: T.Text
updateInclusionSQL = T.concat
  [ "insert or ignore into Inclusions (Inclusion, Includer) "
  , "values (?, ?)"                                         ]

resetInclusions :: DBHandle -> SourceFile -> IO ()
resetInclusions h sf = do
  let sfHash = stableHash sf
  execStatement h resetInclusionsStmt (Only sfHash)

resetInclusionsSQL :: T.Text
resetInclusionsSQL = "delete from Inclusions where Includer = ?"

getInclusions :: DBHandle -> SourceFile -> IO [SourceFile]
getInclusions = getDirectInclusions  -- TODO FIXME

getDirectInclusions :: DBHandle -> SourceFile -> IO [SourceFile]
getDirectInclusions h sf = do
  is <- execQuery h getDirectInclusionsStmt (Only $ stableHash sf)
  return $ map unwrapSourceFile is

getDirectInclusionsSQL :: T.Text
getDirectInclusionsSQL = T.concat
  [ "select F.Name                                 "
  , "from Inclusions as I                          "
  , "join Files as F on I.Inclusion = F.Hash       "
  , "where I.Includer = ?" ]

getDirectInclusionHashes :: DBHandle -> SourceFileHash -> IO [SourceFileHash]
getDirectInclusionHashes h sfHash = do
  hashes <- execQuery h getDirectInclusionHashesStmt (Only sfHash)
  return $ map unwrapSourceFileHash hashes

getDirectInclusionHashesSQL :: T.Text
getDirectInclusionHashesSQL = "select Inclusion from Inclusions where Includer = ?"

getIncluders :: DBHandle -> SourceFile -> IO [SourceFile]
getIncluders = getDirectIncluders  -- TODO FIXME

getDirectIncluders :: DBHandle -> SourceFile -> IO [SourceFile]
getDirectIncluders h sf = do
  is <- execQuery h getDirectIncludersStmt (Only $ stableHash sf)
  return $ map unwrapSourceFile is

getDirectIncludersSQL :: T.Text
getDirectIncludersSQL = T.concat
  [ "select F.Name                           "
  , "from Inclusions as I                    "
  , "join Files as F on I.Includer = F.Hash  "
  , "where I.Inclusion = ?"                  ]

getDirectIncluderHashes :: DBHandle -> SourceFileHash -> IO [SourceFileHash]
getDirectIncluderHashes h sfHash = do
  hashes <- execQuery h getDirectIncluderHashesStmt (Only sfHash)
  return $ map unwrapSourceFileHash hashes

getDirectIncluderHashesSQL :: T.Text
getDirectIncluderHashesSQL = "select Includer from Inclusions where Inclusion = ?"

getInclusionHierarchy :: DBHandle -> SourceFile -> IO String
getInclusionHierarchy h sf = asDot <$> generateHierarchy
  where
    generateHierarchy = do
      let nid = stableHash sf
      let node = mkHighlightedNode nid sf []
      let g = addUniqueNode node mkGraph

      -- Find includers.
      irs <- getDirectIncluders h sf
      g' <- foldM (expandHierarchy mkEdge
                                   (getDeclsReferencedInFile h)
                                   getDirectIncluders nid)
                  g irs

      -- Find inclusions.
      ics <- getDirectInclusions h sf
      foldM (expandHierarchy mkReverseEdge
                             (flip (getDeclsReferencedInFile h))
                             getDirectInclusions nid)
            g' ics
      
    expandHierarchy newEdgeF refsF nextLevelF superNodeId g sf' = do
      let nid = stableHash sf'

      -- If this inclusion already exists in the graph, bail.
      if nid `nodeElem` g
        then do let edge = newEdgeF superNodeId nid
                return $ addEdge edge g
        else do -- Find references to this inclusion in the original source file.
                refs <- refsF sf' sf

                -- Add this inclusion as a node in the graph.
                let node = mkNode nid sf' [map diIdentifier refs]
                let edge = newEdgeF superNodeId nid
                let g' = addEdge edge . addUniqueNode node $ g

                -- Add its inclusions in turn.
                is <- nextLevelF h sf'
                foldM (expandHierarchy newEdgeF refsF nextLevelF nid) g' is

-- Schema and operations for the Paths table.
definePathsTable :: Connection -> IO ()
definePathsTable c = execute_ c (mkQueryT sql)
  where sql =  T.concat [ "create table if not exists Paths(         "
                        , "Hash integer primary key unique not null, "
                        , "Path text not null)" ]

insertPathSQL :: T.Text
insertPathSQL = "insert or ignore into Paths (Path, Hash) values (?, ?)"

-- Schema and operations for the BuildCommands table.
defineBuildCommandsTable :: Connection -> IO ()
defineBuildCommandsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists BuildCommands( "
                       , "Hash integer primary key unique not null, "
                       , "Command text not null)" ]

insertCommandSQL :: T.Text
insertCommandSQL = "insert or ignore into BuildCommands (Command, Hash) values (?, ?)"

-- Schema and operations for the BuildArgs table.
defineBuildArgsTable :: Connection -> IO ()
defineBuildArgsTable c = execute_ c (mkQueryT sql)
  where sql = T.concat [ "create table if not exists BuildArgs(     "
                       , "Hash integer primary key unique not null, "
                       , "Args text not null)" ]

insertArgsSQL :: T.Text
insertArgsSQL = "insert or ignore into BuildArgs (Args, Hash) values (?, ?)"

-- Schema and operations for the SourceFiles table.
defineSourceFilesTable :: Connection -> IO ()
defineSourceFilesTable c = execute_ c (mkQueryT sql)
  where sql =  T.concat [ "create table if not exists SourceFiles(        "
                        , "File integer primary key unique not null,      "
                        , "WorkingPath integer not null,                  "
                        , "BuildCommand integer not null,                 "
                        , "BuildArgs integer not null,                    "
                        , "Language integer not null)"                    ]

updateSourceFileSQL :: T.Text
updateSourceFileSQL = T.concat
  [ "replace into SourceFiles                                            "
  , "(File, WorkingPath, BuildCommand, BuildArgs, Language) "
  , "values (?, ?, ?, ?, ?)" ]

updateSourceFile :: DBHandle -> CommandInfo -> IO ()
updateSourceFile h CommandInfo {..} = do
  let sfHash = stableHash ciSourceFile
  let wdHash = stableHash ciWorkingPath
  execStatement h insertPathStmt (ciWorkingPath, wdHash)
  let cmdHash = stableHash ciCommand
  execStatement h insertCommandStmt (ciCommand, cmdHash)
  let argsJoined = B.intercalate "\n" ciArgs
  let argsHash = stableHash argsJoined
  execStatement h insertArgsStmt (argsJoined, argsHash)
  execStatement h updateSourceFileStmt (sfHash, wdHash, cmdHash, argsHash,
                                        fromEnum ciLanguage)

getAllSourceFiles :: DBHandle -> IO [CommandInfo]
getAllSourceFiles h = query_ (conn h) (mkQueryT sql)
  where
    sql = T.concat
          [ "select F.Name, W.Path, C.Command, A.Args, Language           "
          , "from SourceFiles                                             "
          , "join Files as F on SourceFiles.File = F.Hash                 "
          , "join Paths as W on SourceFiles.WorkingPath = W.Hash          "
          , "join BuildCommands as C on SourceFiles.BuildCommand = C.Hash "
          , "join BuildArgs as A on SourceFiles.BuildArgs = A.Hash"       ]

getCommandInfo :: DBHandle -> SourceFileHash -> IO (Maybe CommandInfo)
getCommandInfo h sfHash = execSingleRowQuery h getCommandInfoStmt (Only sfHash)

getCommandInfoSQL :: T.Text
getCommandInfoSQL = T.concat
  [ "select F.Name, W.Path, C.Command, A.Args, Language           "
  , "from SourceFiles                                             "
  , "join Files as F on SourceFiles.File = F.Hash                 "
  , "join Paths as W on SourceFiles.WorkingPath = W.Hash          "
  , "join BuildCommands as C on SourceFiles.BuildCommand = C.Hash "
  , "join BuildArgs as A on SourceFiles.BuildArgs = A.Hash        "
  , "where SourceFiles.File = ? limit 1"                          ]

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
  [ "select F.Name, W.Path, C.Command, A.Args, SF.Language "
  , "from Files as F                                       "
  , "join SourceFiles as SF on F.Hash = SF.File            "
  , "join Paths as W on SF.WorkingPath = W.Hash            "
  , "join BuildCommands as C on SF.BuildCommand = C.Hash   "
  , "join BuildArgs as A on SF.BuildArgs = A.Hash          "
  , "where F.Name like ? limit 1"                          ]

-- Schema and operations for the Definitions table.
defineDefinitionsTable :: Connection -> IO ()
defineDefinitionsTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
    execute_ c (mkQueryT indexSQL')
  where
    sql = T.concat [ "create table if not exists Definitions(      "
                   , "USRHash integer primary key unique not null, "
                   , "Name text not null,                          "
                   , "File integer not null,                       "
                   , "Line integer not null,                       "
                   , "Col integer not null,                        "
                   , "Kind integer not null,                       "
                   , "Context integer not null)" ]
    indexSQL = "create index if not exists DefsFileIndex on Definitions(File)"
    indexSQL' = "create index if not exists DefsContextIndex on Definitions(Context)"

defineDefinitionsStagingTable :: Connection -> IO ()
defineDefinitionsStagingTable c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists DefinitionsStaging( "
                   , "USRHash integer not null,                      "
                   , "Name text not null,                            "
                   , "File integer not null,                         "
                   , "Line integer not null,                         "
                   , "Col integer not null,                          "
                   , "Kind integer not null,                         "
                   , "Context integer not null)" ]

defineDefinitionsStagingTable' :: Connection -> IO ()
defineDefinitionsStagingTable' c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists Staging.DefinitionsStaging( "
                   , "USRHash integer not null,                      "
                   , "Name text not null,                            "
                   , "File integer not null,                         "
                   , "Line integer not null,                         "
                   , "Col integer not null,                          "
                   , "Kind integer not null,                         "
                   , "Context integer not null)" ]

updateDef :: DBHandle -> DefUpdate -> IO ()
updateDef h (DefUpdate n usrHash sfHash l c k ctx) = do
    let kind = fromEnum k
    execStatement h updateDefStmt (usrHash, n, sfHash, l, c, kind, ctx)

updateDefSQL :: T.Text
updateDefSQL = T.concat
  [ "replace into Definitions                             "
  , "(USRHash, Name, File, Line, Col, Kind, Context) "
  , "values (?, ?, ?, ?, ?, ?, ?)" ]

stageDefUpdate :: DBHandle -> DefUpdate -> IO ()
stageDefUpdate h (DefUpdate n usrHash sfHash l c k ctx) = do
    let kind = fromEnum k
    execStatement h stageDefUpdateStmt (usrHash, n, sfHash, l, c, kind, ctx)

stageDefUpdateSQL :: T.Text
stageDefUpdateSQL = T.concat
  [ "insert into Staging.DefinitionsStaging          "
  , "(USRHash, Name, File, Line, Col, Kind, Context) "
  , "values (?, ?, ?, ?, ?, ?, ?)" ]

resetDefs :: DBHandle -> SourceFile -> IO ()
resetDefs h sf = do
  let sfHash = stableHash sf
  execStatement h resetDefsStmt (Only sfHash)

resetDefsSQL :: T.Text
resetDefsSQL = "delete from Definitions where File = ?"

getDef :: DBHandle -> SourceLocation -> IO [DefInfo]
getDef h sl = go =<< getReferenced h sl
  where
    go Nothing   = return []  
    go (Just sr) = do
      mayDef <- execSingleRowQuery h getDefStmt (Only . diUSR . sdDef $ sr)
      case (mayDef, sdKind sr) of
        (Nothing, _) -> return []
        (Just def, DynamicCallExpr) ->
          do os <- getTransitiveOverriders def
             os' <- filterM (inheritsFrom (sdViaHash sr) . diContext) os
             return (def : os')
        (Just def, _) -> return [def]

    getTransitiveOverriders :: DefInfo -> IO [DefInfo]
    getTransitiveOverriders di = do
      -- This is a slow algorithm. Rewrite.
      os <- (getOverriders' h (diUSR di)) :: IO [DefInfo]
      os' <- (mapM getTransitiveOverriders os) :: IO [[DefInfo]]
      return $ os ++ (concat os')

    inheritsFrom :: USRHash -> USRHash -> IO Bool
    inheritsFrom via oHash = do
      -- In C++, a superclass pointer 'S* Sp' can point to a subclass
      -- instance, but a subclass pointer cannot point to a superclass
      -- instance. That means that 'Sp->foo()' can only invoke
      -- the implementation of 'foo' on the class pointed to or some
      -- subclass of it. However, this is complicated by the fact that
      -- the class 'S' may not implement 'foo' - it may not override
      -- its superclass's implementation! That means to get the true
      -- set of possible methods, we must filter the subclass
      -- 'foo' implementations to make sure the class they're defined
      -- on inherits from 'S'.
      supers <- getOverrided' h oHash
      --putStrLn $ "Looking for " ++ show via ++ ": " ++ show oHash
      --        ++ " inherits from " ++ show supers
      case map diUSR supers of
        []         -> return False
        supHashes  -> if via `elem` supHashes
                        then return True
                        else or <$> mapM (inheritsFrom via) supHashes

getDefSQL :: T.Text
getDefSQL = T.concat
  [ "select D.Name, D.USRHash, F.Name, D.Line, D.Col, D.Kind, "
  , "       coalesce(C.USRHash, 0)                            "
  , "from Definitions as D                                    "
  , "join Files as F on D.File = F.Hash                       "
  , "left join Definitions as C on D.Context = C.USRHash      "
  , "where D.USRHash = ? limit 1" ]
  
-- Schema and operations for the Overrides table.
defineOverridesTable :: Connection -> IO ()
defineOverridesTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
  where
    sql = T.concat [ "create table if not exists Overrides(           "
                   , "Definition integer primary key unique not null, "
                   , "Overrided integer not null)" ]
    indexSQL = "create index if not exists OverridesOverridedIndex on Overrides(Overrided)"

defineOverridesStagingTable :: Connection -> IO ()
defineOverridesStagingTable c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists OverridesStaging(           "
                   , "Definition integer not null, "
                   , "Overrided integer not null)" ]

defineOverridesStagingTable' :: Connection -> IO ()
defineOverridesStagingTable' c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists Staging.OverridesStaging( "
                   , "Definition integer not null, "
                   , "Overrided integer not null)" ]

updateOverride :: DBHandle -> Override -> IO ()
updateOverride h (Override defUSRHash overrideUSRHash) =
    execStatement h updateOverrideStmt (defUSRHash, overrideUSRHash)

updateOverrideSQL :: T.Text
updateOverrideSQL = T.concat
  [ "replace into Overrides (Definition, Overrided) "
  , "values (?, ?)" ]

stageOverrideUpdate :: DBHandle -> Override -> IO ()
stageOverrideUpdate h (Override defUSRHash overrideUSRHash) =
    execStatement h stageOverrideUpdateStmt (defUSRHash, overrideUSRHash)

stageOverrideUpdateSQL :: T.Text
stageOverrideUpdateSQL = T.concat
  [ "insert into Staging.OverridesStaging (Definition, Overrided) "
  , "values (?, ?)" ]

resetOverrides :: DBHandle -> SourceFile -> IO ()
resetOverrides h sf = do
  let sfHash = stableHash sf
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
    Just sd -> let usrHash = diUSR . sdDef $ sd in
               execQuery h getOverridedStmt (Only usrHash)

getOverrided' :: DBHandle -> USRHash -> IO [DefInfo]
getOverrided' h usr = execQuery h getOverridedStmt (Only usr)

getOverridedSQL :: T.Text
getOverridedSQL = T.concat
  [ "select D.Name, D.USRHash, F.Name, D.Line, D.Col, D.Kind, "
  , "       coalesce(C.USRHash, 0)                            "
  , "from Overrides as O                                      "
  , "join Definitions as D on O.Overrided = D.USRHash         "
  , "join Files as F on D.File = F.Hash                       "
  , "left join Definitions as C on D.Context = C.USRHash      "
  , "where O.Definition = ?" ]

getOverriders :: DBHandle -> SourceLocation -> IO [DefInfo]
getOverriders h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = diUSR . sdDef $ sd in
               execQuery h getOverridersStmt (Only usrHash)

getOverriders' :: DBHandle -> USRHash -> IO [DefInfo]
getOverriders' h usr = execQuery h getOverridersStmt (Only usr)

getOverridersSQL :: T.Text
getOverridersSQL = T.concat
  [ "select D.Name, D.USRHash, F.Name, D.Line, D.Col, D.Kind, "
  , "       coalesce(C.USRHash, 0)                            "
  , "from Overrides as O                                      "
  , "join Definitions as D on O.Definition = D.USRHash        "
  , "join Files as F on D.File = F.Hash                       "
  , "left join Definitions as C on D.Context = C.USRHash      "
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
      let usrHash = fromIntegral usr
      let name = diIdentifier di
      members <- getMembersForUSR h usr
      let node = mkHighlightedNode usrHash name [(map diIdentifier members)]
      let g = addUniqueNode node mkGraph

      -- Find subclasses.
      ovs <- getOverriders' h usr
      g' <- foldM (expandHierarchy mkEdge getOverriders' di) g ovs

      -- Find superclasses.
      ods <- getOverrided' h usr
      g'' <- foldM (expandHierarchy mkReverseEdge getOverrided' di) g' ods

      return g''
      
    expandHierarchy newEdgeF nextLevelF superDI g di = do
      let usr = diUSR di
      let usrHash = fromIntegral usr
      let name = diIdentifier di
      members <- getMembersForUSR h usr
      let node = mkNode usrHash name [(map diIdentifier members)]
      let edge = newEdgeF (fromIntegral . diUSR $ superDI) usrHash
      let g' = (addEdge edge) . (addNode node) $ g
      
      os <- nextLevelF h usr
      g'' <- foldM (expandHierarchy newEdgeF nextLevelF di) g' os
      return g''

getMembers :: DBHandle -> SourceLocation -> IO [DefInfo]
getMembers h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = diUSR . sdDef $ sd in
               execQuery h getMembersStmt (Only usrHash)

getMembersForUSR :: DBHandle -> USRHash -> IO [DefInfo]
getMembersForUSR h usr = execQuery h getMembersStmt (Only usr)

getMembersSQL :: T.Text
getMembersSQL = T.concat
  [ "select D.Name, D.USRHash, F.Name, D.Line, D.Col, D.Kind, C.USRHash "
  , "from Definitions as D                                              "
  , "join Files as F on D.File = F.Hash                                 "
  , "join Definitions as C on D.Context = C.USRHash                     "
  , "where D.Context = ?" ]

-- Schema and operations for the References table.
defineReferencesTable :: Connection -> IO ()
defineReferencesTable c = do
    execute_ c (mkQueryT sql)
    execute_ c (mkQueryT indexSQL)
    execute_ c (mkQueryT indexSQL')
    execute_ c (mkQueryT indexSQL'')
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
    indexSQL' = "create index if not exists RefsRefIndex on Refs(Ref)"
    indexSQL'' = "create index if not exists RefsRefContextIndex on Refs(RefContext)"

defineReferencesStagingTable :: Connection -> IO ()
defineReferencesStagingTable c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists RefsStaging( "
                   , "RefId integer not null,                 "
                   , "File integer not null,                  "
                   , "Line integer not null,                  "
                   , "Col integer not null,                   "
                   , "EndLine integer not null,               "
                   , "EndCol integer not null,                "
                   , "RefKind integer not null,               "
                   , "RefVia integer not null,                "
                   , "RefDecl integer not null,               "
                   , "RefContext integer not null,            "
                   , "Ref integer not null)" ]

defineReferencesStagingTable' :: Connection -> IO ()
defineReferencesStagingTable' c = do
    execute_ c (mkQueryT sql)
  where
    sql = T.concat [ "create table if not exists Staging.RefsStaging( "
                   , "RefId integer not null,                 "
                   , "File integer not null,                  "
                   , "Line integer not null,                  "
                   , "Col integer not null,                   "
                   , "EndLine integer not null,               "
                   , "EndCol integer not null,                "
                   , "RefKind integer not null,               "
                   , "RefVia integer not null,                "
                   , "RefDecl integer not null,               "
                   , "RefContext integer not null,            "
                   , "Ref integer not null)" ]

updateReference :: DBHandle -> ReferenceUpdate -> IO ()
updateReference h ReferenceUpdate {..} = do
  let kind = fromEnum rfuKind
  execStatement h updateReferenceStmt (rfuId, rfuFileHash, rfuLine, rfuCol,
                                       rfuEndLine, rfuEndCol, kind,
                                       rfuViaHash, rfuDeclHash,
                                       rfuContextHash, rfuUSRHash)
 
updateReferenceSQL :: T.Text
updateReferenceSQL = T.concat
  [ "replace into Refs                            "
  , " (RefId, File, Line, Col, EndLine, EndCol,   "
  , "  RefKind, RefVia, RefDecl, RefContext, Ref) "
  , "values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" ]

stageReferenceUpdate :: DBHandle -> ReferenceUpdate -> IO ()
stageReferenceUpdate h ReferenceUpdate {..} = do
  let kind = fromEnum rfuKind
  execStatement h stageReferenceUpdateStmt (rfuId, rfuFileHash, rfuLine, rfuCol,
                                            rfuEndLine, rfuEndCol, kind,
                                            rfuViaHash, rfuDeclHash,
                                            rfuContextHash, rfuUSRHash)
 
stageReferenceUpdateSQL :: T.Text
stageReferenceUpdateSQL = T.concat
  [ "insert into Staging.RefsStaging              "
  , " (RefId, File, Line, Col, EndLine, EndCol,   "
  , "  RefKind, RefVia, RefDecl, RefContext, Ref) "
  , "values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" ]

resetReferences :: DBHandle -> SourceFile -> IO ()
resetReferences h sf = do
  let sfHash = stableHash sf
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
  [ "select D.Name, D.USRHash, RF.Name, R.Line, R.Col, R.RefKind, "
  , "       coalesce(C.USRHash, 0)                                "
  , "from Refs as R                                               "
  , "join Definitions as D on R.Ref = D.USRHash                   "
  , "join Files as RF on R.File = RF.Hash                         "
  , "left join Definitions as C on R.RefContext = C.USRHash       "
  , "where R.RefId = ?" ]

-- Search for declarations referenced by the first file which are
-- located in the second file. Used by getInclusionHierarchy.
getDeclsReferencedInFile :: DBHandle -> SourceFile -> SourceFile -> IO [DefInfo]
getDeclsReferencedInFile h sf hf = execQuery h getDeclsReferencedInFileStmt (stableHash sf, stableHash hf)

getDeclsReferencedInFileSQL :: T.Text
getDeclsReferencedInFileSQL = T.concat
  [ "select distinct D.Name, D.USRHash, RF.Name, RHdr.Line, RHdr.Col, "
  , "                RHdr.RefKind, coalesce(C.USRHash, 0)             "
  , "from Refs as RSrc                                                "
  , "join Refs as RHdr on RSrc.RefDecl = RHdr.RefId                   "
  , "join Definitions as D on RHdr.Ref = D.USRHash                    "
  , "join Files as RF on RHdr.File = RF.Hash                          "
  , "left join Definitions as C on RHdr.RefContext = C.USRHash        "
  , "where RSrc.File = ? and RHdr.File = ?" ]

getReferenced :: DBHandle -> SourceLocation -> IO (Maybe SourceReferenced)
getReferenced h (SourceLocation sf l c) = do
    rs <- execQuery h getReferencedStmt (stableHash sf, l, l, c, l, c)

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
  [ "select D.Name, D.USRHash, DF.Name, D.Line, D.Col, D.Kind,  "
  , "       coalesce(C.USRHash, 0), RF.Name, R.Line, R.Col,     "
  , "       R.EndLine, R.EndCol, R.RefKind, R.RefVia, R.RefDecl "
  , "from Refs as R                                             "
  , "join Definitions as D on R.Ref = D.USRHash                 "
  , "join Files as DF on D.File = DF.Hash                       "
  , "join Files as RF on R.File = RF.Hash                       "
  , "left join Definitions as C on D.Context = C.USRHash        "
  , "where R.File = ? and                                       "
  , "  ((? between R.Line and R.EndLine) and                    "
  , "   (? > R.Line or ? >= R.Col) and                          "
  , "   (? < R.EndLine or ? < R.EndCol))" ]


narrowReferenced :: [SourceReferenced] -> IO (Maybe SourceReferenced)
narrowReferenced = return . filterCall . filterExpansion
  where
    filterExpansion rs = let exps = filter ((== MacroExpansion) . sdKind) rs in
                         if null exps then rs else exps

    -- filterCall is a hack until #109 gets fixed.
    filterCall rs =
      case (filterNarrowest rs, take 1 . filter isDynamicCall $ rs) of
        (Just n, [c])
          | sdDeclHash n == sdDeclHash c -> Just c
          | otherwise                    -> Just n
        (mn, _) -> mn

    filterNarrowest [] = Nothing
    filterNarrowest rs = decideTies
                       . head
                       . groupBy ((==) `on` sdRangeSize)
                       . sortBy (comparing sdRangeSize)
                       $ rs

    decideTies :: [SourceReferenced] -> Maybe SourceReferenced
    decideTies []                   = Nothing
    decideTies [r]                  = Just r
    decideTies (r:rs)
      | sdKind r == CallExpr        = Just r
      | sdKind r == DynamicCallExpr = Just r
      | otherwise                   = decideTies rs

    sdRangeSize = rangeSize . sdRange
    rangeSize (SourceRange _ l c el ec) = (el - l, ec - c)

    isDynamicCall = (== DynamicCallExpr) . sdKind

getReferences :: DBHandle -> SourceLocation -> IO [SourceReference]
getReferences h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = diUSR . sdDef $ sd in
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
      is <- execQuery h getCallersStmt (fromEnum k1, fromEnum k2, fromEnum k3, nextUSR)
      os <- getOverrided' h nextUSR
      foldM accumCallers is os
    accumCallers :: [Invocation] -> DefInfo -> IO [Invocation]
    accumCallers is o = do
      is' <- go DynamicCallExpr DynamicCallExpr MacroExpansion (diUSR o)
      return $ is' ++ is

getCallersSQL :: T.Text
getCallersSQL = T.concat
  [ "select D.Name, D.USRHash, F.Name, D.Line, D.Col, D.Kind, "
  , "       coalesce(C.USRHash, 0), FR.Name, R.Line, R.Col    "
  , "from Refs as R                                           "
  , "join Files as FR on R.File = FR.Hash                     "
  , "join Definitions as D on R.RefContext = D.USRHash        "
  , "join Files as F on D.File = F.Hash                       "
  , "left join Definitions as C on D.Context = C.USRHash      "
  , "where R.Ref = ? and R.RefKind in (?, ?, ?)               "
  , "order by FR.Name, R.Line, R.Col" ]

getCallees :: DBHandle -> SourceLocation -> IO [DefInfo]
getCallees h sl = do
  maySd <- getReferenced h sl
  case maySd of
    Nothing -> return []
    Just sd -> let usrHash = diUSR . sdDef $ sd in
               execQuery h getCalleesStmt (fromEnum CallExpr, fromEnum MacroExpansion, usrHash)

getCalleesSQL :: T.Text
getCalleesSQL = T.concat
  [ "select distinct D.Name, D.USRHash, F.Name, D.Line, D.Col, "
  , "                D.Kind, coalesce(C.USRHash, 0)            "
  , "from Refs as R                                            "
  , "join Definitions as D on R.Ref = D.USRHash                "
  , "join Files as F on D.File = F.Hash                        "
  , "left join Definitions as C on D.Context = C.USRHash       "
  , "where R.RefContext = ? and R.RefKind in (?, ?)            "
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
              >> defineDefinitionsStagingTable c
              >> defineOverridesTable c
              >> defineOverridesStagingTable c
              >> defineReferencesTable c
              >> defineReferencesStagingTable c
              >> ensureVersion c

ensureVersion :: Connection -> IO ()
ensureVersion c = getDBVersion c >>= checkVersion
  where
    checkVersion (Just (major, minor))
                 | (major, minor) == (dbMajorVersion, dbMinorVersion) = return ()
                 | otherwise = throwDBVersionError major minor
    checkVersion _ = setDBVersion c

throwDBVersionError :: Int -> Int -> IO ()
throwDBVersionError major minor  =  error $ "Database version "
                                 ++ (show major) ++ "." ++ (show minor)
                                 ++ " is different than required version "
                                 ++ (show dbMajorVersion) ++ "."
                                 ++ (show dbMinorVersion)
