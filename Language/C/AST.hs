{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.AST
  
  where
  
  import Data.Either
  import Text.Printf
  import Data.Generics
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Get some consistent naming structure up in here.
  
  -- | A translation unit is a nonempty list of external declarations (C99 9.6).
  type CTranslationUnit   
    = [CExternal]
  
  -- | A block item is either a declaration or statement (C99 6.8.3).
  -- | Block items make up the bodies of compound statements.
  type BlockItem 
    = Either CDeclaration CStatement
  
  -- | A statement specifies an action to be performed (C99 6.8.3).
  data CStatement 
    = BreakStmt 
    | CaseStmt CExpr CStatement
    | CompoundStmt [BlockItem]
    | ContinueStmt
    | DefaultStmt CStatement
    | DoWhileStmt CStatement CExpr
    | EmptyStmt
    | ExpressionStmt CExpr
    -- | This is the old style of for loops.
    | ForStmt (Maybe CExpr) (Maybe CExpr) (Maybe CExpr) CStatement
    -- | This is the new style defined in C99.
    | ForDeclStmt CDeclaration (Maybe CExpr) (Maybe CExpr) CStatement
    | GotoStmt String
    | IfStmt CExpr CStatement (Maybe CStatement)
    | LabeledStmt String [CAttribute] CStatement
    | ReturnStmt (Maybe CExpr)
    | SwitchStmt CExpr CStatement
    | WhileStmt CExpr CStatement
    deriving (Eq, Show, Typeable, Data)
  
  -- | A C function (C99 6.9.1).
  data CFunction = CFunction [Specifier] CDeclarator CStatement
    deriving (Eq, Show, Typeable, Data)
  
  -- | External declarations (C99 6.9). Wraps either a 'CFunction' or 'CDeclaration'.
  data CExternal 
    = FunctionDecl CFunction
    | ExternDecl CDeclaration
    deriving (Eq, Show, Typeable, Data)
  
  -- | C expressions (C99 6.5).
  -- Please note that the comma operator is currently unimplemented.
  data CExpr
    = Constant CLiteral
    | Comma [CExpr]
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    | Cast CDeclaration CExpr
    | UnaryOp String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    | SizeOfType CDeclaration
    | CBuiltin BuiltinExpr
    deriving (Eq, Show, Typeable, Data)
  
  -- TODO: Expand this to include __builtin_offsetof and __builtin_types_compatible_p
  data BuiltinExpr
    = BuiltinVaArg CExpr CDeclaration
    deriving (Eq, Show, Typeable, Data)
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Eq, Typeable, Data)
    
  instance Show CLiteral where
    show (CInteger i) = show i
    show (CChar c) = show c
    show (CFloat f) = show f
    show (CString s) = s
  
  data StorageSpecifier
    = SAuto
    | SRegister
    | SStatic
    | SExtern
    | STypedef
    | SAttribute CAttribute
    deriving (Eq, Show, Typeable, Data)
    
  data TypeQualifier
    = QConst
    | QRestrict
    | QVolatile
    | FInline
    deriving (Eq, Typeable, Data)
  
  instance Show TypeQualifier where
    show QConst = "const"
    show QRestrict = "restrict"
    show QVolatile = "volatile"
    show FInline = "inline"
  
  data Specifier 
    = TSpec TypeSpecifier
    | TQual TypeQualifier
    | SSpec StorageSpecifier
    deriving (Eq, Show, Typeable, Data)
    
  data Enumerator 
    = EnumIdent String
    | EnumAssign String CExpr
    deriving (Eq, Show, Typeable, Data)
  
  data TypeSpecifier
     = TVoid
     | TChar
     | TShort
     | TInt
     | TLong
     | TFloat
     | TDouble
     | TSigned
     | TUnsigned
     | TBool
     | TBuiltin String
     | TStructOrUnion (Maybe String) Bool [CDeclaration] [CAttribute]
     | TEnumeration (Maybe String) [Enumerator] [CAttribute]
     | TTypedef String CDeclaration
     | TTypeOfExpr CExpr
     deriving (Eq, Show, Typeable, Data)
  
  data CAttribute = CAttribute [CExpr]
    deriving (Eq, Show, Typeable, Data)
  
  data CSize 
    = Sized CExpr
    deriving (Eq, Show, Typeable, Data)
  
  data CDeclaration 
    = CDeclaration [Specifier] [(Maybe CDeclarator, Maybe Initializer, Maybe CSize)]
    deriving (Eq, Show, Typeable, Data)
    
  type AsmName = Maybe String
  
  data CDeclarator
   = Named String [DerivedDeclarator] AsmName [CAttribute]
   | Abstract [DerivedDeclarator] [CAttribute]
   deriving (Eq, Show, Typeable, Data)
  
  data Initializer 
    = InitExpression CExpr
    | InitList [Initializer]
    deriving (Eq, Show, Typeable, Data)
  
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] (Maybe CExpr)
   | Function [CDeclaration] Bool
   deriving (Eq, Show, Typeable, Data)
   