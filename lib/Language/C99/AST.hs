module Language.C99.AST
  ( CTranslationUnit (..)
  , CAsmArgument (..)
  , CAsmName 
  , CAsmOperand (..)
  , CAttribute (..)
  , CBlockItem (..)
  , CBuiltinExpr (..)
  , CDeclaration (..)
  , CDeclarator (..)
  , CDeclInfo (..)
  , CDerivedDeclarator (..) 
  , CDesignator (..)
  , CEnumerator (..)
  , CExpr (..)
  , CExternal (..)
  , CField (..)
  , CFunction (..)
  , CInitializer (..)
  , CInitializerSubfield (..)
  , CParameter (..)
  , CSpecifier (..)
  , CStatement(..) 
  , CStorageSpecifier (..)
  , CStringLiteral (..)
  , CTypeName (..)
  , CTypeQualifier (..)
  , CTypeSpecifier (..)
  )
  
  where
  
  import GHC.Generics
  import Data.Default
  import Data.Generics hiding (Generic)
  import Language.C99.Literals
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Rename CAsmOperand and CAsmArgument to something more descriptive.
  
  -- | A translation unit is a nonempty list of external declarations (C99 9.6).
  newtype CTranslationUnit = CTranslationUnit [CExternal]
    deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | A block item is either a declaration or statement (C99 6.8.3).
  -- Block items make up the bodies of compound statements ('CompoundStmt').
  data CBlockItem
    = BlockDeclaration CDeclaration
    | BlockStatement CStatement
    deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | A statement specifies an action to be performed in sequence (C99 6.8.3).
  -- Unless specified otherwise, the semantics of statements (e.g. that 
  -- @break@ and @continue@ may only appear inside @for@/@while@ loops) are 
  -- not enforced by the parser, but will fail to compile in any modern C compiler.
  data CStatement 
    -- | The GCC syntax for inline assembly.
    = AsmStmt (Maybe CTypeQualifier) CAsmOperand
    -- | The @break@ statement. Should only appear inside loop constructs.
    | BreakStmt 
    -- | The @case@ statement, taking the form of @case expr: statement@.
    -- Should only appear inside the bodies of @switch@ statements.
    | CaseStmt CExpr CStatement
    -- | Compound statements are blocks of code (C99 6.8.2). They are composed
    -- of either 'CDeclaration' or 'CStatement' types, wrapped by the 
    -- 'BlockItem' type synonym.
    | CompoundStmt [CBlockItem]
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
    | GotoStmt CExpr
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
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | There are two types of inline assembly: the first is the standard call to @asm()@, 
  -- which is identical to a function call in syntax (except for the fact that it can only 
  -- take a string as its parameter). The second is GCC assembly syntax, which takes the form of
  -- @asm( instructions : output-operands* : input-operands : clobbered-registers* );@
  data CAsmOperand 
    = Simple CStringLiteral
    | GCCAsm CStringLiteral [CAsmArgument] [CAsmArgument] [CStringLiteral]

    deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | Represents an output or input value in GCC assembly syntax. Takes the form of 
  -- | @string (variable)?@.
  data CAsmArgument 
    = CAsmArgument CStringLiteral (Maybe CExpr)
    deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | A C function (C99 6.9.1).
  -- Invariant: The final 'CStatement' will always be a 'CompoundStmt', and the 
  -- provided 'CDeclarator' will always be named.
  data CFunction = CFunction [CSpecifier] CDeclarator CStatement
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | External declarations (C99 6.9). Wraps either a 'CFunction' or 'CDeclaration'.
  data CExternal 
    = FunctionDecl CFunction
    | ExternDecl CDeclaration
    | ExternFunCall CExpr
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | C expressions (C99 6.5).
  -- Please note that the comma operator is currently unimplemented.
  data CExpr
    = Constant CLiteral
    | Comma [CExpr]
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    | CCast CTypeName CExpr
    | PostfixOp CExpr String
    | UnaryOp  String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    -- | Whereas sizeof(variable) parses as a function call, sizeof(type) needs its own node.
    | SizeOfType CTypeName
    | CBuiltin CBuiltinExpr
    | CParen CExpr -- parenthesized expressions
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | A string literal newtype to provide a modicum of type safety in the AST.
  newtype CStringLiteral = CStringLiteral {
    getExpr :: CExpr
  } deriving (Eq, Typeable, Data, Show, Generic)
  
  -- TODO: Expand this to include __builtin_offsetof and __builtin_types_compatible_p
  -- | GNU/clang built-in functions that are exposed after preprocessing.
  data CBuiltinExpr
    -- | Corresponds to @__builtin_va_arg(id, type)@.
    = BuiltinVaArg CExpr CTypeName
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | Storage class specifiers (C99 6.7.1).
  -- As an extension, @__attribute__(())@ is considered a storage specifier.
  data CStorageSpecifier
    = CAuto
    | CRegister
    | CStatic
    | CExtern
    | CTypedef
    | CAttr CAttribute
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | Type qualifiers (C99 6.7.3) and function specifiers (C99 6.7.4).
  -- Please note that the 'FInline' qualifier must only be applied to functions.
  data CTypeQualifier
    = CConst
    | CRestrict
    | CVolatile
    | CInline
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | C qualifiers and specifiers.
  data CSpecifier 
    = TSpec CTypeSpecifier
    | TQual CTypeQualifier
    | SSpec CStorageSpecifier
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | C type specifiers (6.7.2).
  -- As a GNU extension, @typeof(expr)@ is supported.
  data CTypeSpecifier
     = TVoid
     | TChar
     | TShort
     | TInt
     | TLong
     | TInt128
     | TUInt128
     | TFloat
     | TDouble
     | TSigned
     | TUnsigned
     | TBool
     -- | Corresponds to the @__builtin_va_arg@ type.
     | TBuiltin String
     | TStructOrUnion (Maybe String) Bool [CField] [CAttribute]
     | TEnumeration (Maybe String) [CEnumerator] [CAttribute]
     | TTypedef String CTypeName
     | TTypeOfExpr CExpr
     deriving (Eq, Show, Typeable, Data, Generic)

  -- | C enumeration specifiers (C99 6.7.2.2).
  data CEnumerator 
    = EnumIdent String
    | EnumAssign String CExpr
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute = CAttribute [CExpr]
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | Record type that wraps the various fields a declaration may have.
  data CDeclInfo = CDeclInfo {
    contents :: Maybe CDeclarator,
    initVal :: Maybe CInitializer,
    size :: Maybe CExpr
  } deriving (Show, Eq, Typeable, Data, Generic)

  instance Default CDeclInfo where def = CDeclInfo Nothing Nothing Nothing
  
  -- | C declarations (C99 6.7).
  -- This method of structuring declarations was innovated by Benedikt Huber.
  data CDeclaration = CDeclaration {
      declrSpecifiers :: [CSpecifier],
      declrInfos :: [CDeclInfo]
  } deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | Represents C type names. The list of specifiers will not be empty.
  data CTypeName = CTypeName [CSpecifier] (Maybe CDeclarator) deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | Represents C parameters. There will be at least one 'Specifier', and only 
  -- one 'CDeclInfo', which will contain a possibly-named declarator
  -- and no initializer or size.
  data CParameter = CParameter [CSpecifier] (Maybe CDeclarator) deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | Represents fields of structs or unions. There will be at least one specifier,
  -- at least one 'CDeclInfo', all of which will not have an initVal (but may be 
  -- named, sized, named and sized, or unnamed and sized.)
  newtype CField = CField CDeclaration deriving (Show, Eq, Typeable, Data, Generic)
  
  -- As a GNU extension, the user can specify the assembly name for a C function 
  -- or variable.
  type CAsmName = Maybe String
  
  -- | C declarators, both abstract and concrete (C99 6.7.5 and 6.7.6).
  data CDeclarator 
   = CDeclarator {
      declName :: Maybe String,
      derived :: [CDerivedDeclarator],
      asmName :: CAsmName,
      declAttributes :: [CAttribute]
   } deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | C designators, i.e. that which can appear inside compound initialization statements.
  data CDesignator 
    = ArrayDesignator CExpr
    | MemberDesignator String
    deriving (Show, Eq, Typeable, Data, Generic)
  
  -- | C initializers (C99 6.7.8). Initialization types can contain one 
  -- expression or a bracketed list of initializers.
  data CInitializer 
    = CInitExpression CExpr
    | CInitList [CInitializerSubfield]
    deriving (Eq, Show, Typeable, Data, Generic)
  
  -- | Represents the deconstructed initializers.
  data CInitializerSubfield = CInitializerSubfield [CDesignator] CInitializer
    deriving (Show, Eq, Typeable, Data, Generic)
  
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  -- In the future, Apple's extension for blocks (declared with @^@) may be added.
  data CDerivedDeclarator
   = Pointer [CTypeQualifier]
   | Array [CTypeQualifier] (Maybe CExpr)
   | DerivedFunction [CParameter] Bool
   deriving (Eq, Show, Typeable, Data, Generic)
