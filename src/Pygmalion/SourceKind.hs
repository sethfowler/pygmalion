{-# OPTIONS_GHC -Werror #-} -- Fail to compile if CursorKind has changed.

module Pygmalion.SourceKind
( SourceKind (..)
) where

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
                | DynamicCallExpr  -- Pygmalion custom kind.
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
                | SourceFile  -- Pygmalion custom kind.
                | FirstPreprocessing
                | LastPreprocessing
                deriving (Enum, Eq, Ord, Read, Show)

instance Serialize SourceKind where
  put = put . fromEnum
  get = toEnum <$> get

instance FromRow SourceKind where
  fromRow = toEnum <$> field
