{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.AST
  
  where
  
  import Data.Generics
  import Language.C.Literals
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Get some consistent naming structure up in here.
  
  -- | A translation unit is a nonempty list of external declarations (C99 9.6).
  type CTranslationUnit   
    = [CExternal]
  
  -- | A block item is either a declaration or statement (C99 6.8.3).
  -- Block items make up the bodies of compound statements ('CompoundStmt').
  type BlockItem 
    = Either CDeclaration CStatement
  
  -- | A statement specifies an action to be performed in sequence (C99 6.8.3).
  -- Unless specified otherwise, the semantics of statements (e.g. that 
  -- @break@ and @continue@ may only appear inside @for@/@while@ loops) are 
  -- not enforced by the parser, but will fail to compile in any modern C compiler.
  data CStatement
    -- | The @break@ statement. Should only appear inside loop constructs.
    = BreakStmt 
    -- | The @case@ statement, taking the form of @case expr: statement@.
    -- Should only appear inside the bodies of @switch@ statements.
    | CaseStmt CExpr CStatement
    -- | Compound statements are blocks of code (C99 6.8.2). They are composed
    -- of either 'CDeclaration' or 'CStatement' types, wrapped by the 
    -- 'BlockItem' type synonym.
    | CompoundStmt [BlockItem]
    -- | The @continue@ statement. Should only appear inside loop constructs.
    | ContinueStmt 
    -- | The @default@ statement, taking the form of @default: statement@.
    -- Should only appear inside of loop constructs.
    | DefaultStmt CStatement
    -- | The do-while loop, taking the form of @do statement while expr@.
    | DoWhileStmt CStatement CExpr
    -- | The empty statement (i.e. just a semicolon.) May disappear in the future;
    -- if so, 'ExpressionStmt' will have a 'Maybe' 'CExpr' as its body.
    | EmptyStmt
    -- | A simple expression statement, i.e. that which evaluates its body
    -- and discards the result.
    | ExpressionStmt CExpr
    -- | Old-style @for@ loops (C99 6.8.5.3). 
    | ForStmt (Maybe CExpr) (Maybe CExpr) (Maybe CExpr) CStatement
    -- | New-style @for@-loops (C99 6.8.5.3.1).
    | ForDeclStmt CDeclaration (Maybe CExpr) (Maybe CExpr) CStatement
    -- | The @goto@ statement.
    | GotoStmt String
    -- | The @if@ statement, taking the form of @if (cond) statement else? statement?@.
    | IfStmt CExpr CStatement (Maybe CStatement)
    -- | Labeled statements. As a GNU extension, labels may have the @__attribute__((unused))@ 
    -- attribute added to them to avoid warnings when compiling with @-Wall@.
    | LabeledStmt String [CAttribute] CStatement
    -- | The @return@ statement, of the form @return expr?@
    | ReturnStmt (Maybe CExpr)
    -- | The @switch@ statement, which should take the form of @switch (constant) compound-stmt@.
    | SwitchStmt CExpr CStatement
    -- | The @while@ statement, of the form @while expr statement@.
    | WhileStmt CExpr CStatement
    deriving (Eq, Show, Typeable, Data)
  
  -- | A C function (C99 6.9.1).
  -- Invariant: The final 'CStatement' will always be a 'CompoundStmt', and the 
  -- provided 'CDeclarator' will always be named.
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
    | Cast CTypeName CExpr
    | UnaryOp String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    -- | Whereas sizeof(variable) parses as a function call, sizeof(type) needs its own node.
    | SizeOfType CTypeName
    | CBuiltin BuiltinExpr
    deriving (Eq, Show, Typeable, Data)
  
  -- TODO: Expand this to include __builtin_offsetof and __builtin_types_compatible_p
  -- | GNU/clang built-in functions that are exposed after preprocessing.
  data BuiltinExpr
    -- | Corresponds to @__builtin_va_arg(id, type)@.
    = BuiltinVaArg CExpr CTypeName
    deriving (Eq, Show, Typeable, Data)
  
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
     | TStructOrUnion (Maybe String) Bool [CField] [CAttribute]
     | TEnumeration (Maybe String) [Enumerator] [CAttribute]
     | TTypedef String CTypeName
     | TTypeOfExpr CExpr
     deriving (Eq, Show, Typeable, Data)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute = CAttribute [CExpr]
    deriving (Eq, Show, Typeable, Data)
  
  -- | Record type that wraps the various fields a declaration may have.
  data DeclInfo = DeclInfo {
    contents :: Maybe CDeclarator,
    initVal :: Maybe Initializer,
    size :: Maybe CExpr
  } deriving (Show, Eq, Typeable, Data)
  
  
  -- | C declarations (C99 6.7).
  -- This method of structuring declarations was innovated by Benedikt Huber.
  data CDeclaration 
    = CDeclaration [Specifier] [DeclInfo]
    deriving (Eq, Show, Typeable, Data)
  
  -- | Represents C type names. These have a number of invariants: there will 
  -- be at least one 'Specifier', at most one 'DeclInfo', which may contain a 
  -- declarator (if it is not 'Nothing', it will be unnamed) and will not have an 'Initializer' or be sized..
  newtype CTypeName = CTypeName CDeclaration deriving (Show, Eq, Typeable, Data)
  
  -- | Represents C parameters. There will be at least one 'Specifier', and only 
  -- one 'DeclInfo', which will contain a possibly-named declarator
  -- and no initializer or size.
  newtype CParameter = CParameter CDeclaration deriving (Show, Eq, Typeable, Data)
  
  -- | Represents fields of structs or unions. There will be at least one specifier,
  -- at least one 'DeclInfo', all of which will not have an initVal (but may be 
  -- named, sized, named and sized, or unnamed and sized.)
  newtype CField = CField CDeclaration deriving (Show, Eq, Typeable, Data)
  
  -- As a GNU extension, the user can specify the assembly name for a C function 
  -- or variable.
  type AsmName = Maybe String
  
  -- | C declarators, both abstract and concrete (C99 6.7.5 and 6.7.6).
  data CDeclarator
   = CDeclarator (Maybe String) [DerivedDeclarator] AsmName [CAttribute]
   deriving (Eq, Show, Typeable, Data)
  
  -- | C initializers (C99 6.7.8). Initialization types can contain one 
  -- expression or a bracketed list of initializers.
  data Initializer 
    = InitExpression CExpr
    | InitList [Initializer]
    deriving (Eq, Show, Typeable, Data)
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  -- In the future, Apple's extension for blocks (declared with @^@) may be added.
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] (Maybe CExpr)
   | Function [CParameter] Bool
   deriving (Eq, Show, Typeable, Data)
