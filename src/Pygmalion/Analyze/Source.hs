{-# LANGUAGE DeriveDataTypeable #-}

module Pygmalion.Analyze.Source
( runSourceAnalyses
, getIdentifier
) where

import Clang.Alloc.Storable()
import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import Clang.File as File
import Clang.Monad
import qualified Clang.Source as Source
import qualified Clang.String as CStr
import Clang.TranslationUnit
import Clang.Traversal
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.IORef
import Data.Typeable
import System.FilePath.Posix

import Data.Bool.Predicate
import Pygmalion.Analyze.Extension
import Pygmalion.Core

runSourceAnalyses :: CommandInfo -> IO (Maybe ([FilePath], [DefInfo]))
runSourceAnalyses ci = do
  includesRef <- newIORef []
  defsRef <- newIORef []
  result <- try $ withTranslationUnit ci $ do
                    tu <- getTranslationUnit
                    includesAnalysis includesRef tu
                    defsAnalysis defsRef tu
  case result of
    Right _ -> (,) <$> readIORef includesRef <*> readIORef defsRef >>= return . Just
    Left (ClangException _) -> return Nothing

getIdentifier :: CommandInfo -> SourceLocation -> IO (Maybe Identifier)
getIdentifier ci sl = do
  result <- try $ withTranslationUnit ci $ inspectIdentifier sl
  case result of
    Right identifier        -> return identifier
    Left (ClangException _) -> return Nothing

includesAnalysis :: IORef [FilePath] -> TranslationUnit -> ClangApp ()
includesAnalysis isRef tu = void $ getInclusions tu visitInclusions
  where
    visitInclusions :: InclusionVisitor
    visitInclusions file _  = do
      f <- File.getName file
      name <- CStr.unpack f
      when (isLocalHeader name) $ liftIO . modifyIORef isRef $! ((normalise name) :)

isLocalHeader :: FilePath -> Bool
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension .&&. (not . null)

defsAnalysis :: IORef [DefInfo] -> TranslationUnit -> ClangApp ()
defsAnalysis dsRef tu = do
    cursor <- getCursor tu
    void $ visitChildren cursor kidVisitor
  where
    kidVisitor :: ChildVisitor
    kidVisitor cursor _ = do
      loc <- C.getLocation cursor
      (f, ln, col, _) <- Source.getSpellingLocation loc
      file <- case f of Just validF -> File.getName validF >>= CStr.unpack
                        Nothing     -> return ""
      when (inProject file) $ do
        cKind <- C.getKind cursor
        defined <- isDef cursor cKind
        when defined $ do
          usr <- XRef.getUSR cursor >>= CStr.unpack
          name <- fqn cursor
          kind <- C.getCursorKindSpelling cKind >>= CStr.unpack
          def <- return $ DefInfo (Identifier name usr)
                            (SourceLocation (normalise file) ln col)
                            kind
          defEvaled <- liftIO . evaluate $ def
          liftIO . modifyIORef dsRef $! (defEvaled :)
      return $ if inProject file then ChildVisit_Recurse
                                 else ChildVisit_Continue

{-
-- Was the following; still evaluating the tradeoffs.
      return $ case cKind of
                  C.Cursor_FunctionDecl -> ChildVisit_Continue
                  C.Cursor_CXXMethod    -> ChildVisit_Continue
                  _                     -> ChildVisit_Recurse
-}

inProject :: FilePath -> Bool
inProject = isRelative .&&. isValid .&&. (not . null)

isDef :: C.Cursor -> C.CursorKind -> ClangApp Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> ClangApp String
fqn cursor = (intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> (cursorName c) <*> (C.getSemanticParent c >>= go)

cursorName :: C.Cursor -> ClangApp String
cursorName c = C.getDisplayName c >>= CStr.unpack >>= anonymize
  where anonymize [] = return "<anonymous>"
        anonymize s  = return s

inspectIdentifier :: SourceLocation -> ClangApp (Maybe Identifier)
inspectIdentifier (SourceLocation f ln col) = do
    tu <- getTranslationUnit
    file <- File.getFile tu f
    loc <- Source.getLocation tu file ln col
    cursor <- Source.getCursor tu loc
    defCursor <- C.getDefinition cursor
    isNullDef <- C.isNullCursor defCursor
    if isNullDef then do C.getReferenced cursor >>= reportIdentifier
                 else reportIdentifier defCursor
  where
    reportIdentifier cursor = do
      name <- fqn cursor
      usr <- XRef.getUSR cursor >>= CStr.unpack
      -- liftIO $ putStrLn $ "In file: " ++ f ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
      return $ if null usr then Nothing
                           else Just $ Identifier name usr

withTranslationUnit :: CommandInfo -> ClangApp a -> IO a
withTranslationUnit (CommandInfo sf _ (Command _ args) _) f = do
    withCreateIndex False False $ \index -> do
      withParse index (Just sf) args [] [TranslationUnit_None] f bail
  where
    bail = throw . ClangException $ "Libclang couldn't parse " ++ sf

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException
