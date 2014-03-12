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
                | VariableRef
                | InvalidFile
                | NoDeclFound
                | NotImplemented
                | InvalidCode
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
                | LambdaExpr
                | ObjCBoolLiteralExpr
                | ObjCSelfExpr
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
                | MSAsmStmt
                | NullStmt
                | DeclStmt
                | OMPParallelDirective
                | TranslationUnit
                | UnexposedAttr
                | IBActionAttr
                | IBOutletAttr
                | IBOutletCollectionAttr
                | CXXFinalAttr
                | CXXOverrideAttr
                | AnnotateAttr
                | AsmLabelAttr
                | PackedAttr
                | PreprocessingDirective
                | MacroDefinition
                | MacroExpansion
                | InclusionDirective
                | SourceFile  -- Pygmalion custom kind.
                | ModuleImportDecl
                  deriving (Enum, Eq, Ord, Read, Show)

instance Serialize SourceKind where
  put = put . fromEnum
  get = toEnum <$> get

instance FromRow SourceKind where
  fromRow = toEnum <$> field
