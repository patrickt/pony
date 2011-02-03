{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.AST
  
  where
  
  import Data.Either
  import Data.Generics
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Get some consistent naming structure up in here.
  
  -- | A translation unit is a nonempty list of external declarations (C99 9.6).
  type CTranslationUnit   
    = [CExternal]
  
  -- | A block item is either a declaration or statement (C99 6.8.3).
  -- Block items make up the bodies of compound statements.
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
    | ForStmt (Maybe CExpr) (Maybe CExpr) (Maybe CExpr) CStatement
    -- | New-style for-loops (C99 6.8.5.3.1).
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
  -- | GNU/clang built-in functions that are exposed after preprocessing.
  data BuiltinExpr
    -- | Corresponds to @__builtin_va_arg(id, type)@.
    = BuiltinVaArg CExpr CDeclaration
    deriving (Eq, Show, Typeable, Data)
  
  -- | C literals.
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
  
  -- | Storage class specifiers (C99 6.7.1).
  -- As an extension, @__attribute__(())@ is considered a storage specifier.
  data StorageSpecifier
    = SAuto
    | SRegister
    | SStatic
    | SExtern
    | STypedef
    | SAttribute CAttribute
    deriving (Eq, Show, Typeable, Data)
  
  -- | Type qualifiers (C99 6.7.3) and function specifiers (C99 6.7.4).
  -- Please note that the 'FInline' qualifier must only be applied to functions.
  data TypeQualifier
    = QConst
    | QRestrict
    | QVolatile
    | FInline
    deriving (Eq, Show, Typeable, Data)
  
  -- | C qualifiers and specifiers.
  data Specifier 
    = TSpec TypeSpecifier
    | TQual TypeQualifier
    | SSpec StorageSpecifier
    deriving (Eq, Show, Typeable, Data)
  
  -- | C enumeration specifiers (C99 6.7.2.2).
  data Enumerator 
    = EnumIdent String
    | EnumAssign String CExpr
    deriving (Eq, Show, Typeable, Data)
  
  -- | C type specifiers (6.7.2).
  -- As a GNU extension, @typeof(expr)@ is supported.
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
     -- | Corresponds to the @__builtin_va_arg@ type.
     | TBuiltin String
     | TStructOrUnion (Maybe String) Bool [CDeclaration] [CAttribute]
     | TEnumeration (Maybe String) [Enumerator] [CAttribute]
     | TTypedef String CDeclaration
     | TTypeOfExpr CExpr
     deriving (Eq, Show, Typeable, Data)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute = CAttribute [CExpr]
    deriving (Eq, Show, Typeable, Data)
  
  -- TODO: Remove this, as it is no longer necessary
  data CSize 
    = Sized CExpr
    deriving (Eq, Show, Typeable, Data)
  
  -- | C declarations (C99 6.7).
  -- Though this definition is rather byzantine in its structure, it has 
  -- distinct advantage in that it can encapsulate structure declarations,
  -- parameter declarations, and type names. This method of structuring declarations 
  -- was innovated by Benedikt Huber.
  data CDeclaration 
    = CDeclaration [Specifier] [(Maybe CDeclarator, Maybe Initializer, Maybe CSize)]
    deriving (Eq, Show, Typeable, Data)
    
  type AsmName = Maybe String
  
  -- TODO: Unify this declaration.
  -- | C declarators (C99 6.7.5 and 6.7.6).
  data CDeclarator
   = Named String [DerivedDeclarator] AsmName [CAttribute]
   | Abstract [DerivedDeclarator] [CAttribute]
   deriving (Eq, Show, Typeable, Data)
  
  -- | Initialization types can contain one expression or a bracketed list of initializers.
  data Initializer 
    = InitExpression CExpr
    | InitList [Initializer]
    deriving (Eq, Show, Typeable, Data)
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] (Maybe CExpr)
   | Function [CDeclaration] Bool
   deriving (Eq, Show, Typeable, Data)
   