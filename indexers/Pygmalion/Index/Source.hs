{-# LANGUAGE Haskell2010, DeriveDataTypeable, OverloadedStrings, RankNTypes #-}

module Pygmalion.Index.Source
( runSourceAnalyses
, getLookupInfo
, displayAST
) where

import qualified Clang.CrossReference as XRef
import qualified Clang.Cursor as C
import qualified Clang.Diagnostic as Diag
import qualified Clang.File as File
import Clang.Monad
import qualified Clang.Source as Source
import qualified Clang.String as CS
import Clang.TranslationUnit
import Clang.Traversal
import qualified Clang.Type as T
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe
import Data.Typeable

import Data.Bool.Predicate
import Pygmalion.Core
import Pygmalion.Hash
import Pygmalion.Index.Result
import Pygmalion.Log
import Pygmalion.RPC.Client

runSourceAnalyses :: CommandInfo -> RPCConnection -> IO ()
runSourceAnalyses ci conn = do
  result <- try $ withTranslationUnit ci $ \tu -> do
                    inclusionsAnalysis conn ci tu
                    defsAnalysis conn ci tu
  case result of
    Right _ -> return ()
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e) >> return ()

getLookupInfo :: CommandInfo -> SourceLocation -> IO LookupInfo
getLookupInfo ci sl = do
  result <- try $ withTranslationUnit ci $ inspectIdentifier sl
  case result of
    Right r                 -> return r
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e ) >> return GotNothing

displayAST :: CommandInfo -> IO ()
displayAST ci = do
  result <- try $ withTranslationUnit ci $ doDisplayAST
  case result of
    Right _                 -> return ()
    Left (ClangException e) -> logWarn ("Clang exception: " ++ e )

inclusionsAnalysis :: RPCConnection -> CommandInfo -> TranslationUnit
                   -> ClangApp s ()
inclusionsAnalysis conn ci tu = void $ getInclusions tu
                                       (inclusionsVisitor conn ci)

inclusionsVisitor :: RPCConnection -> CommandInfo -> InclusionVisitor s
inclusionsVisitor conn ci file iStack = do
    ic <- File.getName file >>= CS.unpackByteString
    let mkInclusion' = mkInclusion ic
    case iStack of
      []       -> return () -- The source file itself.
      (_ : []) -> liftIO $ runRPC (rpcFoundInclusion (mkInclusion' True)) conn
      (_ : _)  -> liftIO $ runRPC (rpcFoundInclusion (mkInclusion' False)) conn
  where
    mkInclusion ic d =
      Inclusion (ci { ciArgs = (ciArgs ci) ++ (incArgs . ciLanguage $ ci)
                    , ciLastIndexed = 0
                    , ciSourceFile = ic
                    }) ic d
    incArgs CLanguage       = ["-x", "c"]
    incArgs CPPLanguage     = ["-x", "c++"]
    incArgs UnknownLanguage = []

defsAnalysis :: RPCConnection -> CommandInfo -> TranslationUnit -> ClangApp s ()
defsAnalysis conn ci tu = do
    cursor <- getCursor tu
    void $ visitChildren cursor (defsVisitor conn ci tu)

defsVisitor :: RPCConnection -> CommandInfo -> TranslationUnit -> ChildVisitor s
defsVisitor conn ci tu cursor _ = do
  let thisFile = ciSourceFile ci
  loc <- getCursorLocation cursor
  -- TODO: What to do about inclusions that aren't normal inclusions?
  -- Ones that are intended to be multiply included, etc?
  case (thisFile == slFile loc) of
    True -> do  cKind <- C.getKind cursor
                defC <- C.getDefinition cursor
                defIsNull <- C.isNullCursor defC
                refC <- C.getReferenced cursor
                refIsNull <- C.isNullCursor refC
                cursorIsDef <- isDef cursor cKind
                cursorIsRef <- C.isReference cKind
                cursorIsDecl <- C.isDeclaration cKind
                cursorIsPV <- if cKind == C.Cursor_CXXMethod then C.isPureVirtualCppMethod cursor
                                                             else return False

                -- Record references.
                -- TODO: Ignore CallExpr children that start at the same
                -- position as the CallExpr. This always refers to the same
                -- thing as the CallExpr itself. We don't want to just ignore
                -- the CallExpr though, because e.g. constructor calls are
                -- represented as CallExprs with no children.
                -- TODO: Support LabelRefs.
                let goodRef = cKind `elem` [C.Cursor_CallExpr,
                                            C.Cursor_DeclRefExpr,
                                            C.Cursor_MemberRefExpr,
                                            C.Cursor_TypeRef,
                                            C.Cursor_MacroExpansion,
                                            C.Cursor_MacroDefinition,
                                            C.Cursor_CXXMethod]
                                           || cursorIsDef
                                           || cursorIsRef
                                           || cursorIsDecl
                when (goodRef && not (defIsNull && refIsNull)) $ do
                    -- Prefer definitions to references when available.
                    let referToC = if defIsNull then refC else defC
                    referToUSR <- XRef.getUSR referToC >>= CS.unpackByteString

                    -- Determine the end of the extent of this cursor.
                    extent <- C.getExtent cursor
                    endLoc <- Source.getEnd extent
                    (_, endLn, endCol, _) <- Source.getSpellingLocation endLoc

                    -- Determine the context.
                    ctxC <- getContext tu cursor
                    ctxUSR <- XRef.getUSR ctxC >>= CS.unpackByteString

                    -- Record.
                    refKind <- if cKind == C.Cursor_CallExpr then analyzeCallKind cursor
                                                             else return $ toSourceKind cKind
                    reference <- return $! ReferenceUpdate (hash . slFile $ loc)
                                                           (slLine loc)
                                                           (slCol loc)
                                                           endLn
                                                           endCol
                                                           refKind
                                                           (hash ctxUSR)
                                                           (hash referToUSR)
                    liftIO $ runRPC (rpcFoundRef reference) conn
                
                -- Record method overrides.
                -- TODO: I seem to recall that in C++11 you can override
                -- constructors. Add support for that if so.
                when (cKind `elem` [C.Cursor_CXXMethod, C.Cursor_Destructor]) $ do
                  overrides <- C.getOverriddenCursors cursor
                  overrideUSRs <- mapM (CS.unpackByteString <=< XRef.getUSR) overrides
                  usr <- XRef.getUSR cursor >>= CS.unpackByteString
                  forM_ overrideUSRs $ \oUSR -> do
                    override <- return $! Override (hash usr) (hash oUSR)
                    liftIO $ runRPC (rpcFoundOverride override) conn

                -- Record class inheritance ("overrides").
                when (cKind == C.Cursor_ClassDecl) $
                  void $ visitChildren cursor (classVisitor conn cursor)

                -- Record definitions.
                -- TODO: Support labels.
                let goodDef = cKind `elem` [C.Cursor_MacroDefinition,
                                            C.Cursor_VarDecl]
                                           || cursorIsDef
                                           || cursorIsPV
                when goodDef $ do
                    usr <- XRef.getUSR cursor >>= CS.unpackByteString
                    name <- fqn cursor
                    let kind = toSourceKind cKind
                    def <- return $! DefUpdate name
                                               usr
                                               (hash . slFile $ loc)
                                               (slLine loc)
                                               (slCol loc)
                                               kind
                    liftIO $ runRPC (rpcFoundDef def) conn

                -- Recurse (most of the time).
                case cKind of
                  C.Cursor_MacroDefinition -> return ChildVisit_Continue
                  _                        -> return ChildVisit_Recurse

    -- Don't recurse into out-of-project header files.
    False -> return ChildVisit_Continue

classVisitor :: RPCConnection -> C.Cursor -> ChildVisitor s
classVisitor conn thisClassC cursor _ = do
  cKind <- C.getKind cursor
  case cKind of
    C.Cursor_CXXBaseSpecifier -> do
      thisClassUSR <- XRef.getUSR thisClassC >>= CS.unpackByteString
      defC <- C.getDefinition cursor
      baseUSR <- XRef.getUSR defC >>= CS.unpackByteString
      override <- return $! Override (hash thisClassUSR) (hash baseUSR)
      liftIO $ runRPC (rpcFoundOverride override) conn
      return ChildVisit_Break
    _ -> return ChildVisit_Continue

getCursorLocation :: C.Cursor -> ClangApp s SourceLocation
getCursorLocation cursor = do
  loc <- C.getLocation cursor
  (f, ln, col, _) <- Source.getSpellingLocation loc
  file <- case f of Just f' -> File.getName f' >>= CS.unpackByteString
                    Nothing -> return ""
  return $! SourceLocation file ln col

analyzeCallKind :: C.Cursor -> ClangApp s SourceKind
analyzeCallKind c = do
  isDynamicCall <- C.isDynamicCall c

  if isDynamicCall then do
    baseExpr <- C.getBaseExpression c
    baseExprKind <- C.getKind baseExpr
    isVirtual <- isVirtualCall baseExpr baseExprKind
    return $ if isVirtual then DynamicCallExpr else CallExpr
  else 
    return CallExpr

isDef :: C.Cursor -> C.CursorKind -> ClangApp s Bool
isDef c k = do
  q1 <- C.isDefinition c
  return $ q1 && not (k == C.Cursor_CXXAccessSpecifier)

fqn :: C.Cursor -> ClangApp s Identifier 
fqn cursor = (B.intercalate "::" . reverse) <$> go cursor
  where go c = do isNull <- C.isNullCursor c
                  isTU <- C.getKind c >>= C.isTranslationUnit
                  if isNull || isTU then return [] else go' c
        go' c =  (:) <$> (cursorName c) <*> (C.getSemanticParent c >>= go)

getContext :: TranslationUnit -> C.Cursor -> ClangApp s C.Cursor
getContext tu cursor = do
    isNull <- C.isNullCursor cursor
    cKind <- C.getKind cursor
    go cursor cKind isNull
  where
    go c _ isNull | isNull = return c
    go c k _ | k `elem` [C.Cursor_FunctionDecl, C.Cursor_CXXMethod, C.Cursor_Constructor, C.Cursor_Destructor, C.Cursor_TranslationUnit, C.Cursor_ClassDecl, C.Cursor_StructDecl, C.Cursor_EnumDecl, C.Cursor_Namespace] = return c
    go c k _ | k == C.Cursor_MacroExpansion = do
      extent <- C.getExtent c
      loc <- Source.getStart extent
      (f, ln, col, _) <- Source.getSpellingLocation loc
      case f of
         Nothing -> return c
         Just file -> do newLoc <- Source.getLocation tu file ln col
                         srcCursor <- Source.getCursor tu newLoc
                         srcCKind <- C.getKind srcCursor
                         case srcCKind of
                           C.Cursor_MacroExpansion -> return c  -- Will loop infinitely otherwise.
                           _                       -> getContext tu srcCursor
    go c _ _ = C.getSemanticParent c >>= getContext tu

cursorName :: C.Cursor -> ClangApp s Identifier
cursorName c = C.getDisplayName c >>= CS.unpackByteString >>= anonymize
  where anonymize s | B.null s  = return "<anonymous>"
                    | otherwise = return s

inspectIdentifier :: SourceLocation -> TranslationUnit -> ClangApp s LookupInfo
inspectIdentifier (SourceLocation f ln col) tu = do
    dumpDiagnostics tu
    file <- File.getFile tu (unSourceFile f)
    loc <- Source.getLocation tu file ln col
    cursor <- Source.getCursor tu loc
    cKind <- C.getKind cursor

    -- kind <- C.getCursorKindSpelling cKind >>= CS.unpack
    -- liftIO $ logDebug $ "Cursor kind is " ++ kind

    defCursor <- if cKind == C.Cursor_MacroDefinition then return cursor
                                                      else C.getDefinition cursor
    isNullDef <- C.isNullCursor defCursor
    if isNullDef then do C.getReferenced cursor >>= reportIdentifier
                 else reportIdentifier defCursor
  where
    reportIdentifier cursor = do
      -- dumpSubtree cursor
      -- liftIO $ logDebug $ "In file: " ++ (B.unpack f) ++ ":" ++ (show ln) ++ ":" ++ (show col) ++ " got name: " ++ name ++ " usr: " ++ usr
      isNull <- C.isNullCursor cursor
      case isNull of
        False -> do usr <- XRef.getUSR cursor >>= CS.unpackByteString
                    kind <- C.getKind cursor 
                    cursorIsDef <- isDef cursor kind
                    di <- createDefInfo cursor usr kind
                    if cursorIsDef && isJust di
                      then return (GotDef $ fromJust di)
                      else if isJust di then return (GotDecl usr $ fromJust di)
                                        else return (GotUSR usr)
        True -> return GotNothing
    createDefInfo cursor usr k = do
      name <- C.getDisplayName cursor >>= CS.unpackByteString
      let kind = toSourceKind k
      loc <- C.getLocation cursor
      (df, dl, dc, _) <- Source.getSpellingLocation loc
      file <- case df of Just valid -> File.getName valid >>= CS.unpackByteString
                         Nothing    -> return ""
      return $ if (not $ B.null file) then Just (DefInfo name usr (SourceLocation file dl dc) kind)
                                      else Nothing

-- We need to decide on a policy, but it'd be good to figure out a way to let
-- the user display these, and maybe always display errors.
dumpDiagnostics :: TranslationUnit -> ClangApp s ()
dumpDiagnostics tu = do
    opts <- Diag.defaultDisplayOptions
    dias <- Diag.getDiagnostics tu
    forM_ dias $ \dia -> do
      severity <- Diag.getSeverity dia
      when (isError severity) $ do
        diaStr <- Diag.formatDiagnostic opts dia >>= CS.unpack
        liftIO $ logInfo $ "Diagnostic: " ++ diaStr
  where
    isError = (== Diag.Diagnostic_Error) .||. (== Diag.Diagnostic_Fatal)

doDisplayAST :: TranslationUnit -> ClangApp s ()
doDisplayAST tu = getCursor tu >>= dumpSubtree

dumpSubtree :: C.Cursor -> ClangApp s ()
dumpSubtree cursor = do
    dump 0 cursor
    void $ visitChildren cursor (dumpVisitor 0)
    liftIO $ putStrLn "Finished recursing"
  where dumpVisitor :: Int -> ChildVisitor s
        dumpVisitor i c _ = do dump i c
                               void $ visitChildren c (dumpVisitor $ i + 1)
                               return ChildVisit_Continue
        dump :: Int -> C.Cursor -> ClangApp s ()
        dump i c = do
          -- Get location.
          loc <- C.getLocation c
          (_, ln, col, _) <- Source.getSpellingLocation loc

          -- Get extent.
          extent <- C.getExtent c
          (_, startLn, startCol, _) <- Source.getStart extent >>= Source.getSpellingLocation
          (_, endLn, endCol, _) <- Source.getEnd extent >>= Source.getSpellingLocation

          -- Get metadata.
          name <- C.getDisplayName c >>= CS.unpack
          usr <- XRef.getUSR c >>= CS.unpack
          cKind <- C.getKind c
          kind <- C.getCursorKindSpelling cKind >>= CS.unpack

          -- Get definition metadata.
          defCursor <- C.getDefinition c
          defName <- C.getDisplayName defCursor >>= CS.unpack
          defUSR <- XRef.getUSR defCursor >>= CS.unpack
          refCursor <- C.getReferenced c
          refName <- C.getDisplayName refCursor >>= CS.unpack
          refUSR <- XRef.getUSR refCursor >>= CS.unpack

          -- Get type.
          typ <- C.getType c
          sTyp <- T.getTypeSpelling typ >>= CS.unpack
          typKind <- T.getKind typ
          sTypKind <- T.getTypeKindSpelling typKind >>= CS.unpack

          -- Special stuff for CallExpr
          when (cKind == C.Cursor_CallExpr) $ do
            baseExpr <- C.getBaseExpression c
            baseName <- C.getDisplayName baseExpr >>= CS.unpack
            baseExprKind <- C.getKind baseExpr
            baseType <- C.getType baseExpr
            baseTypeKind <- T.getKind baseType 
            isVirtual <- isVirtualCall baseExpr baseExprKind
            baseTypeSpelling <- T.getTypeSpelling baseType >>= CS.unpack
            baseKindSpelling <- T.getTypeKindSpelling baseTypeKind >>= CS.unpack
            liftIO $ putStrLn $ (replicate i ' ') ++ "[==] CallExpr base: " ++ baseName ++ " type: " ++ baseTypeSpelling ++ "/" ++ baseKindSpelling ++ " virtual? " ++ (show isVirtual)

          -- Display.
          liftIO $ putStrLn $ (replicate i ' ') ++ "[" ++ kind ++ "] " ++ name ++ " (" ++ usr ++ ") " ++
                              "{" ++ sTyp ++ "/" ++ sTypKind ++ "} @ " ++
                              "<" ++ (show ln) ++ "," ++ (show col) ++ "> " ++
                              (show startLn) ++ "," ++ (show startCol) ++ " -> " ++
                              (show endLn) ++ "," ++ (show endCol) ++ " " ++
                              "definition [" ++ defName ++ "/" ++ defUSR ++ "] " ++
                              "reference [" ++ refName ++ "/" ++ refUSR ++ "]"

isVirtualCall :: C.Cursor -> C.CursorKind -> ClangApp s Bool
isVirtualCall c C.Cursor_CallExpr = do baseRef <- C.getReferenced c
                                       baseRefType <- C.getType baseRef
                                       retType <- T.getResultType baseRefType
                                       retKind <- T.getKind retType 
                                       return (retKind `elem` [T.Type_LValueReference, T.Type_Pointer, T.Type_Unexposed, T.Type_Invalid])
isVirtualCall c C.Cursor_MemberRefExpr = do baseRef <- C.getReferenced c
                                            baseRefType <- C.getType baseRef
                                            baseRefKind <- T.getKind baseRefType
                                            return (baseRefKind `elem` [T.Type_LValueReference, T.Type_Pointer, T.Type_Unexposed, T.Type_Invalid])
isVirtualCall c C.Cursor_DeclRefExpr = do baseRef <- C.getReferenced c
                                          baseRefType <- C.getType baseRef
                                          baseRefKind <- T.getKind baseRefType
                                          return (baseRefKind `elem` [T.Type_LValueReference, T.Type_Pointer, T.Type_Unexposed, T.Type_Invalid])
isVirtualCall _ _ = return True

withTranslationUnit :: CommandInfo -> (forall s. TranslationUnit -> ClangApp s a) -> IO a
withTranslationUnit ci f = do
    withCreateIndex False False $ \index -> do
      setGlobalOptions index GlobalOpt_ThreadBackgroundPriorityForAll
      withParse index (Just . unSourceFile $ sf) clangArgs [] [TranslationUnit_DetailedPreprocessingRecord] f bail
  where
    sf = ciSourceFile ci
    bail = throw . ClangException $ "Libclang couldn't parse " ++ (unSourceFile sf)
    clangArgs = map BU.toString (ciArgs $ ci)
    -- FIXME: Is something along these lines useful? Internet claims so but this
    -- may be outdated information, as things seems to work OK without it.
    --clangArgs = map BU.toString ("-I/usr/local/Cellar/llvm/3.2/lib/clang/3.2/include" : args)

data ClangException = ClangException String
  deriving (Show, Typeable)
instance Exception ClangException

toSourceKind :: C.CursorKind -> SourceKind
toSourceKind C.Cursor_UnexposedDecl                      = UnexposedDecl
toSourceKind C.Cursor_StructDecl                         = StructDecl
toSourceKind C.Cursor_UnionDecl                          = UnionDecl
toSourceKind C.Cursor_ClassDecl                          = ClassDecl
toSourceKind C.Cursor_EnumDecl                           = EnumDecl
toSourceKind C.Cursor_FieldDecl                          = FieldDecl
toSourceKind C.Cursor_EnumConstantDecl                   = EnumConstantDecl
toSourceKind C.Cursor_FunctionDecl                       = FunctionDecl
toSourceKind C.Cursor_VarDecl                            = VarDecl
toSourceKind C.Cursor_ParmDecl                           = ParmDecl
toSourceKind C.Cursor_ObjCInterfaceDecl                  = ObjCInterfaceDecl
toSourceKind C.Cursor_ObjCCategoryDecl                   = ObjCCategoryDecl
toSourceKind C.Cursor_ObjCProtocolDecl                   = ObjCProtocolDecl
toSourceKind C.Cursor_ObjCPropertyDecl                   = ObjCPropertyDecl
toSourceKind C.Cursor_ObjCIvarDecl                       = ObjCIvarDecl
toSourceKind C.Cursor_ObjCInstanceMethodDecl             = ObjCInstanceMethodDecl
toSourceKind C.Cursor_ObjCClassMethodDecl                = ObjCClassMethodDecl
toSourceKind C.Cursor_ObjCImplementationDecl             = ObjCImplementationDecl
toSourceKind C.Cursor_ObjCCategoryImplDecl               = ObjCCategoryImplDecl
toSourceKind C.Cursor_TypedefDecl                        = TypedefDecl
toSourceKind C.Cursor_CXXMethod                          = CXXMethod
toSourceKind C.Cursor_Namespace                          = Namespace
toSourceKind C.Cursor_LinkageSpec                        = LinkageSpec
toSourceKind C.Cursor_Constructor                        = Constructor
toSourceKind C.Cursor_Destructor                         = Destructor
toSourceKind C.Cursor_ConversionFunction                 = ConversionFunction
toSourceKind C.Cursor_TemplateTypeParameter              = TemplateTypeParameter
toSourceKind C.Cursor_NonTypeTemplateParameter           = NonTypeTemplateParameter
toSourceKind C.Cursor_TemplateTemplateParameter          = TemplateTemplateParameter
toSourceKind C.Cursor_FunctionTemplate                   = FunctionTemplate
toSourceKind C.Cursor_ClassTemplate                      = ClassTemplate
toSourceKind C.Cursor_ClassTemplatePartialSpecialization = ClassTemplatePartialSpecialization
toSourceKind C.Cursor_NamespaceAlias                     = NamespaceAlias
toSourceKind C.Cursor_UsingDirective                     = UsingDirective
toSourceKind C.Cursor_UsingDeclaration                   = UsingDeclaration
toSourceKind C.Cursor_TypeAliasDecl                      = TypeAliasDecl
toSourceKind C.Cursor_ObjCSynthesizeDecl                 = ObjCSynthesizeDecl
toSourceKind C.Cursor_ObjCDynamicDecl                    = ObjCDynamicDecl
toSourceKind C.Cursor_CXXAccessSpecifier                 = CXXAccessSpecifier
toSourceKind C.Cursor_FirstDecl                          = FirstDecl
toSourceKind C.Cursor_LastDecl                           = LastDecl
toSourceKind C.Cursor_FirstRef                           = FirstRef
toSourceKind C.Cursor_ObjCSuperClassRef                  = ObjCSuperClassRef
toSourceKind C.Cursor_ObjCProtocolRef                    = ObjCProtocolRef
toSourceKind C.Cursor_ObjCClassRef                       = ObjCClassRef
toSourceKind C.Cursor_TypeRef                            = TypeRef
toSourceKind C.Cursor_CXXBaseSpecifier                   = CXXBaseSpecifier
toSourceKind C.Cursor_TemplateRef                        = TemplateRef
toSourceKind C.Cursor_NamespaceRef                       = NamespaceRef
toSourceKind C.Cursor_MemberRef                          = MemberRef
toSourceKind C.Cursor_LabelRef                           = LabelRef
toSourceKind C.Cursor_OverloadedDeclRef                  = OverloadedDeclRef
toSourceKind C.Cursor_LastRef                            = LastRef
toSourceKind C.Cursor_FirstInvalid                       = FirstInvalid
toSourceKind C.Cursor_InvalidFile                        = InvalidFile
toSourceKind C.Cursor_NoDeclFound                        = NoDeclFound
toSourceKind C.Cursor_NotImplemented                     = NotImplemented
toSourceKind C.Cursor_InvalidCode                        = InvalidCode
toSourceKind C.Cursor_LastInvalid                        = LastInvalid
toSourceKind C.Cursor_FirstExpr                          = FirstExpr
toSourceKind C.Cursor_UnexposedExpr                      = UnexposedExpr
toSourceKind C.Cursor_DeclRefExpr                        = DeclRefExpr
toSourceKind C.Cursor_MemberRefExpr                      = MemberRefExpr
toSourceKind C.Cursor_CallExpr                           = CallExpr
toSourceKind C.Cursor_ObjCMessageExpr                    = ObjCMessageExpr
toSourceKind C.Cursor_BlockExpr                          = BlockExpr
toSourceKind C.Cursor_IntegerLiteral                     = IntegerLiteral
toSourceKind C.Cursor_FloatingLiteral                    = FloatingLiteral
toSourceKind C.Cursor_ImaginaryLiteral                   = ImaginaryLiteral
toSourceKind C.Cursor_StringLiteral                      = StringLiteral
toSourceKind C.Cursor_CharacterLiteral                   = CharacterLiteral
toSourceKind C.Cursor_ParenExpr                          = ParenExpr
toSourceKind C.Cursor_UnaryOperator                      = UnaryOperator
toSourceKind C.Cursor_ArraySubscriptExpr                 = ArraySubscriptExpr
toSourceKind C.Cursor_BinaryOperator                     = BinaryOperator
toSourceKind C.Cursor_CompoundAssignOperator             = CompoundAssignOperator
toSourceKind C.Cursor_ConditionalOperator                = ConditionalOperator
toSourceKind C.Cursor_CStyleCastExpr                     = CStyleCastExpr
toSourceKind C.Cursor_CompoundLiteralExpr                = CompoundLiteralExpr
toSourceKind C.Cursor_InitListExpr                       = InitListExpr
toSourceKind C.Cursor_AddrLabelExpr                      = AddrLabelExpr
toSourceKind C.Cursor_StmtExpr                           = StmtExpr
toSourceKind C.Cursor_GenericSelectionExpr               = GenericSelectionExpr
toSourceKind C.Cursor_GNUNullExpr                        = GNUNullExpr
toSourceKind C.Cursor_CXXStaticCastExpr                  = CXXStaticCastExpr
toSourceKind C.Cursor_CXXDynamicCastExpr                 = CXXDynamicCastExpr
toSourceKind C.Cursor_CXXReinterpretCastExpr             = CXXReinterpretCastExpr
toSourceKind C.Cursor_CXXConstCastExpr                   = CXXConstCastExpr
toSourceKind C.Cursor_CXXFunctionalCastExpr              = CXXFunctionalCastExpr
toSourceKind C.Cursor_CXXTypeidExpr                      = CXXTypeidExpr
toSourceKind C.Cursor_CXXBoolLiteralExpr                 = CXXBoolLiteralExpr
toSourceKind C.Cursor_CXXNullPtrLiteralExpr              = CXXNullPtrLiteralExpr
toSourceKind C.Cursor_CXXThisExpr                        = CXXThisExpr
toSourceKind C.Cursor_CXXThrowExpr                       = CXXThrowExpr
toSourceKind C.Cursor_CXXNewExpr                         = CXXNewExpr
toSourceKind C.Cursor_CXXDeleteExpr                      = CXXDeleteExpr
toSourceKind C.Cursor_UnaryExpr                          = UnaryExpr
toSourceKind C.Cursor_ObjCStringLiteral                  = ObjCStringLiteral
toSourceKind C.Cursor_ObjCEncodeExpr                     = ObjCEncodeExpr
toSourceKind C.Cursor_ObjCSelectorExpr                   = ObjCSelectorExpr
toSourceKind C.Cursor_ObjCProtocolExpr                   = ObjCProtocolExpr
toSourceKind C.Cursor_ObjCBridgedCastExpr                = ObjCBridgedCastExpr
toSourceKind C.Cursor_PackExpansionExpr                  = PackExpansionExpr
toSourceKind C.Cursor_SizeOfPackExpr                     = SizeOfPackExpr
toSourceKind C.Cursor_LastExpr                           = LastExpr
toSourceKind C.Cursor_FirstStmt                          = FirstStmt
toSourceKind C.Cursor_UnexposedStmt                      = UnexposedStmt
toSourceKind C.Cursor_LabelStmt                          = LabelStmt
toSourceKind C.Cursor_CompoundStmt                       = CompoundStmt
toSourceKind C.Cursor_CaseStmt                           = CaseStmt
toSourceKind C.Cursor_DefaultStmt                        = DefaultStmt
toSourceKind C.Cursor_IfStmt                             = IfStmt
toSourceKind C.Cursor_SwitchStmt                         = SwitchStmt
toSourceKind C.Cursor_WhileStmt                          = WhileStmt
toSourceKind C.Cursor_DoStmt                             = DoStmt
toSourceKind C.Cursor_ForStmt                            = ForStmt
toSourceKind C.Cursor_GotoStmt                           = GotoStmt
toSourceKind C.Cursor_IndirectGotoStmt                   = IndirectGotoStmt
toSourceKind C.Cursor_ContinueStmt                       = ContinueStmt
toSourceKind C.Cursor_BreakStmt                          = BreakStmt
toSourceKind C.Cursor_ReturnStmt                         = ReturnStmt
toSourceKind C.Cursor_AsmStmt                            = AsmStmt
toSourceKind C.Cursor_ObjCAtTryStmt                      = ObjCAtTryStmt
toSourceKind C.Cursor_ObjCAtCatchStmt                    = ObjCAtCatchStmt
toSourceKind C.Cursor_ObjCAtFinallyStmt                  = ObjCAtFinallyStmt
toSourceKind C.Cursor_ObjCAtThrowStmt                    = ObjCAtThrowStmt
toSourceKind C.Cursor_ObjCAtSynchronizedStmt             = ObjCAtSynchronizedStmt
toSourceKind C.Cursor_ObjCAutoreleasePoolStmt            = ObjCAutoreleasePoolStmt
toSourceKind C.Cursor_ObjCForCollectionStmt              = ObjCForCollectionStmt
toSourceKind C.Cursor_CXXCatchStmt                       = CXXCatchStmt
toSourceKind C.Cursor_CXXTryStmt                         = CXXTryStmt
toSourceKind C.Cursor_CXXForRangeStmt                    = CXXForRangeStmt
toSourceKind C.Cursor_SEHTryStmt                         = SEHTryStmt
toSourceKind C.Cursor_SEHExceptStmt                      = SEHExceptStmt
toSourceKind C.Cursor_SEHFinallyStmt                     = SEHFinallyStmt
toSourceKind C.Cursor_NullStmt                           = NullStmt
toSourceKind C.Cursor_DeclStmt                           = DeclStmt
toSourceKind C.Cursor_LastStmt                           = LastStmt
toSourceKind C.Cursor_TranslationUnit                    = TranslationUnit
toSourceKind C.Cursor_FirstAttr                          = FirstAttr
toSourceKind C.Cursor_UnexposedAttr                      = UnexposedAttr
toSourceKind C.Cursor_IBActionAttr                       = IBActionAttr
toSourceKind C.Cursor_IBOutletAttr                       = IBOutletAttr
toSourceKind C.Cursor_IBOutletCollectionAttr             = IBOutletCollectionAttr
toSourceKind C.Cursor_CXXFinalAttr                       = CXXFinalAttr
toSourceKind C.Cursor_CXXOverrideAttr                    = CXXOverrideAttr
toSourceKind C.Cursor_AnnotateAttr                       = AnnotateAttr
toSourceKind C.Cursor_LastAttr                           = LastAttr
toSourceKind C.Cursor_PreprocessingDirective             = PreprocessingDirective
toSourceKind C.Cursor_MacroDefinition                    = MacroDefinition
toSourceKind C.Cursor_MacroExpansion                     = MacroExpansion
toSourceKind C.Cursor_MacroInstantiation                 = MacroInstantiation
toSourceKind C.Cursor_InclusionDirective                 = InclusionDirective
toSourceKind C.Cursor_FirstPreprocessing                 = FirstPreprocessing
toSourceKind C.Cursor_LastPreprocessing                  = LastPreprocessing
