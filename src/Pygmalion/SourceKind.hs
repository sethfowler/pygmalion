{-# OPTIONS_GHC -Werror #-} -- Fail to compile if CursorKind has changed.

module Pygmalion.SourceKind
( SourceKind (..)
, toSourceKind
) where

import Clang.Cursor
import Control.Applicative
import Data.Serialize
import Database.SQLite.Simple (FromRow(..), field)

data SourceKind = UnexposedDecl
                | StructDecl
                | UnionDecl
                | ClassDecl
                | EnumDecl
                | FieldDecl
                | EnumConstantDecl
                | FunctionDecl
                | VarDecl
                | ParmDecl
                | ObjCInterfaceDecl
                | ObjCCategoryDecl
                | ObjCProtocolDecl
                | ObjCPropertyDecl
                | ObjCIvarDecl
                | ObjCInstanceMethodDecl
                | ObjCClassMethodDecl
                | ObjCImplementationDecl
                | ObjCCategoryImplDecl
                | TypedefDecl
                | CXXMethod
                | Namespace
                | LinkageSpec
                | Constructor
                | Destructor
                | ConversionFunction
                | TemplateTypeParameter
                | NonTypeTemplateParameter
                | TemplateTemplateParameter
                | FunctionTemplate
                | ClassTemplate
                | ClassTemplatePartialSpecialization
                | NamespaceAlias
                | UsingDirective
                | UsingDeclaration
                | TypeAliasDecl
                | ObjCSynthesizeDecl
                | ObjCDynamicDecl
                | CXXAccessSpecifier
                | FirstDecl
                | LastDecl
                | FirstRef
                | ObjCSuperClassRef
                | ObjCProtocolRef
                | ObjCClassRef
                | TypeRef
                | CXXBaseSpecifier
                | TemplateRef
                | NamespaceRef
                | MemberRef
                | LabelRef
                | OverloadedDeclRef
                | LastRef
                | FirstInvalid
                | InvalidFile
                | NoDeclFound
                | NotImplemented
                | InvalidCode
                | LastInvalid
                | FirstExpr
                | UnexposedExpr
                | DeclRefExpr
                | MemberRefExpr
                | CallExpr
                | DynamicCallExpr
                | ObjCMessageExpr
                | BlockExpr
                | IntegerLiteral
                | FloatingLiteral
                | ImaginaryLiteral
                | StringLiteral
                | CharacterLiteral
                | ParenExpr
                | UnaryOperator
                | ArraySubscriptExpr
                | BinaryOperator
                | CompoundAssignOperator
                | ConditionalOperator
                | CStyleCastExpr
                | CompoundLiteralExpr
                | InitListExpr
                | AddrLabelExpr
                | StmtExpr
                | GenericSelectionExpr
                | GNUNullExpr
                | CXXStaticCastExpr
                | CXXDynamicCastExpr
                | CXXReinterpretCastExpr
                | CXXConstCastExpr
                | CXXFunctionalCastExpr
                | CXXTypeidExpr
                | CXXBoolLiteralExpr
                | CXXNullPtrLiteralExpr
                | CXXThisExpr
                | CXXThrowExpr
                | CXXNewExpr
                | CXXDeleteExpr
                | UnaryExpr
                | ObjCStringLiteral
                | ObjCEncodeExpr
                | ObjCSelectorExpr
                | ObjCProtocolExpr
                | ObjCBridgedCastExpr
                | PackExpansionExpr
                | SizeOfPackExpr
                | LastExpr
                | FirstStmt
                | UnexposedStmt
                | LabelStmt
                | CompoundStmt
                | CaseStmt
                | DefaultStmt
                | IfStmt
                | SwitchStmt
                | WhileStmt
                | DoStmt
                | ForStmt
                | GotoStmt
                | IndirectGotoStmt
                | ContinueStmt
                | BreakStmt
                | ReturnStmt
                | AsmStmt
                | ObjCAtTryStmt
                | ObjCAtCatchStmt
                | ObjCAtFinallyStmt
                | ObjCAtThrowStmt
                | ObjCAtSynchronizedStmt
                | ObjCAutoreleasePoolStmt
                | ObjCForCollectionStmt
                | CXXCatchStmt
                | CXXTryStmt
                | CXXForRangeStmt
                | SEHTryStmt
                | SEHExceptStmt
                | SEHFinallyStmt
                | NullStmt
                | DeclStmt
                | LastStmt
                | TranslationUnit
                | FirstAttr
                | UnexposedAttr
                | IBActionAttr
                | IBOutletAttr
                | IBOutletCollectionAttr
                | CXXFinalAttr
                | CXXOverrideAttr
                | AnnotateAttr
                | LastAttr
                | PreprocessingDirective
                | MacroDefinition
                | MacroExpansion
                | MacroInstantiation
                | InclusionDirective
                | FirstPreprocessing
                | LastPreprocessing
                deriving (Enum, Eq, Ord, Read, Show)

instance Serialize SourceKind where
  put = put . fromEnum
  get = toEnum <$> get

instance FromRow SourceKind where
  fromRow = toEnum <$> field

toSourceKind :: CursorKind -> SourceKind
toSourceKind Cursor_UnexposedDecl                      = UnexposedDecl
toSourceKind Cursor_StructDecl                         = StructDecl
toSourceKind Cursor_UnionDecl                          = UnionDecl
toSourceKind Cursor_ClassDecl                          = ClassDecl
toSourceKind Cursor_EnumDecl                           = EnumDecl
toSourceKind Cursor_FieldDecl                          = FieldDecl
toSourceKind Cursor_EnumConstantDecl                   = EnumConstantDecl
toSourceKind Cursor_FunctionDecl                       = FunctionDecl
toSourceKind Cursor_VarDecl                            = VarDecl
toSourceKind Cursor_ParmDecl                           = ParmDecl
toSourceKind Cursor_ObjCInterfaceDecl                  = ObjCInterfaceDecl
toSourceKind Cursor_ObjCCategoryDecl                   = ObjCCategoryDecl
toSourceKind Cursor_ObjCProtocolDecl                   = ObjCProtocolDecl
toSourceKind Cursor_ObjCPropertyDecl                   = ObjCPropertyDecl
toSourceKind Cursor_ObjCIvarDecl                       = ObjCIvarDecl
toSourceKind Cursor_ObjCInstanceMethodDecl             = ObjCInstanceMethodDecl
toSourceKind Cursor_ObjCClassMethodDecl                = ObjCClassMethodDecl
toSourceKind Cursor_ObjCImplementationDecl             = ObjCImplementationDecl
toSourceKind Cursor_ObjCCategoryImplDecl               = ObjCCategoryImplDecl
toSourceKind Cursor_TypedefDecl                        = TypedefDecl
toSourceKind Cursor_CXXMethod                          = CXXMethod
toSourceKind Cursor_Namespace                          = Namespace
toSourceKind Cursor_LinkageSpec                        = LinkageSpec
toSourceKind Cursor_Constructor                        = Constructor
toSourceKind Cursor_Destructor                         = Destructor
toSourceKind Cursor_ConversionFunction                 = ConversionFunction
toSourceKind Cursor_TemplateTypeParameter              = TemplateTypeParameter
toSourceKind Cursor_NonTypeTemplateParameter           = NonTypeTemplateParameter
toSourceKind Cursor_TemplateTemplateParameter          = TemplateTemplateParameter
toSourceKind Cursor_FunctionTemplate                   = FunctionTemplate
toSourceKind Cursor_ClassTemplate                      = ClassTemplate
toSourceKind Cursor_ClassTemplatePartialSpecialization = ClassTemplatePartialSpecialization
toSourceKind Cursor_NamespaceAlias                     = NamespaceAlias
toSourceKind Cursor_UsingDirective                     = UsingDirective
toSourceKind Cursor_UsingDeclaration                   = UsingDeclaration
toSourceKind Cursor_TypeAliasDecl                      = TypeAliasDecl
toSourceKind Cursor_ObjCSynthesizeDecl                 = ObjCSynthesizeDecl
toSourceKind Cursor_ObjCDynamicDecl                    = ObjCDynamicDecl
toSourceKind Cursor_CXXAccessSpecifier                 = CXXAccessSpecifier
toSourceKind Cursor_FirstDecl                          = FirstDecl
toSourceKind Cursor_LastDecl                           = LastDecl
toSourceKind Cursor_FirstRef                           = FirstRef
toSourceKind Cursor_ObjCSuperClassRef                  = ObjCSuperClassRef
toSourceKind Cursor_ObjCProtocolRef                    = ObjCProtocolRef
toSourceKind Cursor_ObjCClassRef                       = ObjCClassRef
toSourceKind Cursor_TypeRef                            = TypeRef
toSourceKind Cursor_CXXBaseSpecifier                   = CXXBaseSpecifier
toSourceKind Cursor_TemplateRef                        = TemplateRef
toSourceKind Cursor_NamespaceRef                       = NamespaceRef
toSourceKind Cursor_MemberRef                          = MemberRef
toSourceKind Cursor_LabelRef                           = LabelRef
toSourceKind Cursor_OverloadedDeclRef                  = OverloadedDeclRef
toSourceKind Cursor_LastRef                            = LastRef
toSourceKind Cursor_FirstInvalid                       = FirstInvalid
toSourceKind Cursor_InvalidFile                        = InvalidFile
toSourceKind Cursor_NoDeclFound                        = NoDeclFound
toSourceKind Cursor_NotImplemented                     = NotImplemented
toSourceKind Cursor_InvalidCode                        = InvalidCode
toSourceKind Cursor_LastInvalid                        = LastInvalid
toSourceKind Cursor_FirstExpr                          = FirstExpr
toSourceKind Cursor_UnexposedExpr                      = UnexposedExpr
toSourceKind Cursor_DeclRefExpr                        = DeclRefExpr
toSourceKind Cursor_MemberRefExpr                      = MemberRefExpr
toSourceKind Cursor_CallExpr                           = CallExpr
toSourceKind Cursor_ObjCMessageExpr                    = ObjCMessageExpr
toSourceKind Cursor_BlockExpr                          = BlockExpr
toSourceKind Cursor_IntegerLiteral                     = IntegerLiteral
toSourceKind Cursor_FloatingLiteral                    = FloatingLiteral
toSourceKind Cursor_ImaginaryLiteral                   = ImaginaryLiteral
toSourceKind Cursor_StringLiteral                      = StringLiteral
toSourceKind Cursor_CharacterLiteral                   = CharacterLiteral
toSourceKind Cursor_ParenExpr                          = ParenExpr
toSourceKind Cursor_UnaryOperator                      = UnaryOperator
toSourceKind Cursor_ArraySubscriptExpr                 = ArraySubscriptExpr
toSourceKind Cursor_BinaryOperator                     = BinaryOperator
toSourceKind Cursor_CompoundAssignOperator             = CompoundAssignOperator
toSourceKind Cursor_ConditionalOperator                = ConditionalOperator
toSourceKind Cursor_CStyleCastExpr                     = CStyleCastExpr
toSourceKind Cursor_CompoundLiteralExpr                = CompoundLiteralExpr
toSourceKind Cursor_InitListExpr                       = InitListExpr
toSourceKind Cursor_AddrLabelExpr                      = AddrLabelExpr
toSourceKind Cursor_StmtExpr                           = StmtExpr
toSourceKind Cursor_GenericSelectionExpr               = GenericSelectionExpr
toSourceKind Cursor_GNUNullExpr                        = GNUNullExpr
toSourceKind Cursor_CXXStaticCastExpr                  = CXXStaticCastExpr
toSourceKind Cursor_CXXDynamicCastExpr                 = CXXDynamicCastExpr
toSourceKind Cursor_CXXReinterpretCastExpr             = CXXReinterpretCastExpr
toSourceKind Cursor_CXXConstCastExpr                   = CXXConstCastExpr
toSourceKind Cursor_CXXFunctionalCastExpr              = CXXFunctionalCastExpr
toSourceKind Cursor_CXXTypeidExpr                      = CXXTypeidExpr
toSourceKind Cursor_CXXBoolLiteralExpr                 = CXXBoolLiteralExpr
toSourceKind Cursor_CXXNullPtrLiteralExpr              = CXXNullPtrLiteralExpr
toSourceKind Cursor_CXXThisExpr                        = CXXThisExpr
toSourceKind Cursor_CXXThrowExpr                       = CXXThrowExpr
toSourceKind Cursor_CXXNewExpr                         = CXXNewExpr
toSourceKind Cursor_CXXDeleteExpr                      = CXXDeleteExpr
toSourceKind Cursor_UnaryExpr                          = UnaryExpr
toSourceKind Cursor_ObjCStringLiteral                  = ObjCStringLiteral
toSourceKind Cursor_ObjCEncodeExpr                     = ObjCEncodeExpr
toSourceKind Cursor_ObjCSelectorExpr                   = ObjCSelectorExpr
toSourceKind Cursor_ObjCProtocolExpr                   = ObjCProtocolExpr
toSourceKind Cursor_ObjCBridgedCastExpr                = ObjCBridgedCastExpr
toSourceKind Cursor_PackExpansionExpr                  = PackExpansionExpr
toSourceKind Cursor_SizeOfPackExpr                     = SizeOfPackExpr
toSourceKind Cursor_LastExpr                           = LastExpr
toSourceKind Cursor_FirstStmt                          = FirstStmt
toSourceKind Cursor_UnexposedStmt                      = UnexposedStmt
toSourceKind Cursor_LabelStmt                          = LabelStmt
toSourceKind Cursor_CompoundStmt                       = CompoundStmt
toSourceKind Cursor_CaseStmt                           = CaseStmt
toSourceKind Cursor_DefaultStmt                        = DefaultStmt
toSourceKind Cursor_IfStmt                             = IfStmt
toSourceKind Cursor_SwitchStmt                         = SwitchStmt
toSourceKind Cursor_WhileStmt                          = WhileStmt
toSourceKind Cursor_DoStmt                             = DoStmt
toSourceKind Cursor_ForStmt                            = ForStmt
toSourceKind Cursor_GotoStmt                           = GotoStmt
toSourceKind Cursor_IndirectGotoStmt                   = IndirectGotoStmt
toSourceKind Cursor_ContinueStmt                       = ContinueStmt
toSourceKind Cursor_BreakStmt                          = BreakStmt
toSourceKind Cursor_ReturnStmt                         = ReturnStmt
toSourceKind Cursor_AsmStmt                            = AsmStmt
toSourceKind Cursor_ObjCAtTryStmt                      = ObjCAtTryStmt
toSourceKind Cursor_ObjCAtCatchStmt                    = ObjCAtCatchStmt
toSourceKind Cursor_ObjCAtFinallyStmt                  = ObjCAtFinallyStmt
toSourceKind Cursor_ObjCAtThrowStmt                    = ObjCAtThrowStmt
toSourceKind Cursor_ObjCAtSynchronizedStmt             = ObjCAtSynchronizedStmt
toSourceKind Cursor_ObjCAutoreleasePoolStmt            = ObjCAutoreleasePoolStmt
toSourceKind Cursor_ObjCForCollectionStmt              = ObjCForCollectionStmt
toSourceKind Cursor_CXXCatchStmt                       = CXXCatchStmt
toSourceKind Cursor_CXXTryStmt                         = CXXTryStmt
toSourceKind Cursor_CXXForRangeStmt                    = CXXForRangeStmt
toSourceKind Cursor_SEHTryStmt                         = SEHTryStmt
toSourceKind Cursor_SEHExceptStmt                      = SEHExceptStmt
toSourceKind Cursor_SEHFinallyStmt                     = SEHFinallyStmt
toSourceKind Cursor_NullStmt                           = NullStmt
toSourceKind Cursor_DeclStmt                           = DeclStmt
toSourceKind Cursor_LastStmt                           = LastStmt
toSourceKind Cursor_TranslationUnit                    = TranslationUnit
toSourceKind Cursor_FirstAttr                          = FirstAttr
toSourceKind Cursor_UnexposedAttr                      = UnexposedAttr
toSourceKind Cursor_IBActionAttr                       = IBActionAttr
toSourceKind Cursor_IBOutletAttr                       = IBOutletAttr
toSourceKind Cursor_IBOutletCollectionAttr             = IBOutletCollectionAttr
toSourceKind Cursor_CXXFinalAttr                       = CXXFinalAttr
toSourceKind Cursor_CXXOverrideAttr                    = CXXOverrideAttr
toSourceKind Cursor_AnnotateAttr                       = AnnotateAttr
toSourceKind Cursor_LastAttr                           = LastAttr
toSourceKind Cursor_PreprocessingDirective             = PreprocessingDirective
toSourceKind Cursor_MacroDefinition                    = MacroDefinition
toSourceKind Cursor_MacroExpansion                     = MacroExpansion
toSourceKind Cursor_MacroInstantiation                 = MacroInstantiation
toSourceKind Cursor_InclusionDirective                 = InclusionDirective
toSourceKind Cursor_FirstPreprocessing                 = FirstPreprocessing
toSourceKind Cursor_LastPreprocessing                  = LastPreprocessing
