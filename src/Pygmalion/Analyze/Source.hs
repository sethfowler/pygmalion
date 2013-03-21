{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Pygmalion.Analyze.Source
( getIncludes
, getDefs
) where

import Clang.Alloc.Storable()
import qualified Clang.Cursor as Cursor
import Clang.File
import Clang.FFI (TranslationUnit)
import qualified Clang.Source as Source
import Clang.TranslationUnit
import Clang.Traversal
import Control.DeepSeq
import Control.Exception
import Data.IORef
import Data.Typeable
import Foreign.StablePtr
import System.IO

import Pygmalion.Core

getIncludes :: CommandInfo -> IO (Maybe [FilePath])
getIncludes = runAnalysis includesAnalysis ([] :: [FilePath])

getDefs :: CommandInfo -> IO (Maybe [String])
getDefs = runAnalysis defsAnalysis ([] :: [String])

type Analysis a = IORef a -> TranslationUnit -> IO ()
runAnalysis :: NFData a => Analysis a -> a -> CommandInfo -> IO (Maybe a)
runAnalysis f st ci = do
  stRef <- newIORef (force st)
  result <- try $ withTranslationUnit ci (f stRef)
  case result of
    Right _                 -> readIORef stRef >>= return . Just . force
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> TranslationUnit -> IO ()
includesAnalysis hsRef tu = withStablePtr hsRef $ \hsRefPtr ->
    getInclusions tu (visitInclusions hsRefPtr) unused
  where
    visitInclusions :: StablePtr (IORef [FilePath]) -> InclusionVisitor Bool
    visitInclusions hsRefPtr file _ _ = do
      let name = getName file
      case null name of
        True -> putStrLn "Got null filename"
        False -> do
          hsRef_ <- deRefStablePtr hsRefPtr
          modifyIORef hsRef_ $ \hs -> force (name : hs)
      return $ Just True

defsAnalysis :: IORef [String] -> TranslationUnit -> IO ()
defsAnalysis _ tu = visitChildren (getCursor tu) kidVisitor unused >> return ()
  where
    kidVisitor cursor _ usrData = do
      let cKind = Cursor.getKind cursor
      let nameString = show (Cursor.getDisplayName cursor)
      let kindString = show (Cursor.getCursorKindSpelling cKind)
      let loc = Cursor.getLocation cursor
      putStrLn "about to call getSpellingLocation" >> hFlush stdout
      let (file, ln, col, _) = Source.getSpellingLocation loc
      let fileString = case file of
                        Nothing -> ""
                        Just f  -> show $ Source.getFilename f
      let lineString = show ln
      let colString = show col
      putStrLn $ "Name: " ++ nameString ++ " Kind: " ++ kindString
               ++ " Loc: " ++ fileString ++ " " ++ lineString ++ ":" ++ colString
      return (usrData, ChildVisit_Continue)

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr v = bracket (newStablePtr v) (freeStablePtr)

withTranslationUnit :: CommandInfo -> (TranslationUnit -> IO ()) -> IO ()
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ sf

unused :: Maybe Bool
unused = Just True

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException
