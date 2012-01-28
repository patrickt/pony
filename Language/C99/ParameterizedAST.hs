{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, ExistentialQuantification #-}

module Language.C99.ParameterizedAST
  
  where
  
  import Data.Coproduct
  import Data.Generics hiding ((:+:))
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Get some consistent naming structure up in here.
  -- TODO: CTranslationUnit and BlockItem need to become datatypes rather than type synonyms.
  -- TODO: Rename CAsmOperand and CAsmArgument to something more descriptive.
  -- TODO: Rename the CInitList constructor of CInitializer, as I find it really hard to read code 
  -- where constructors and types have the same name (save for types with one constructor).
  
  -- | A translation unit is a nonempty list of external declarations (C99 9.6).
  data CTranslationUnit a
    = CTranslationUnit [CExternal a]
    deriving (Show, Eq, Functor, Typeable, Data)
  
  -- | A block item is either a declaration or statement (C99 6.8.3).
  -- Block items make up the bodies of compound statements ('CompoundStmt').
  data CBlockItem a
    = BlockDeclaration (CDeclaration a) 
    | BlockStatement (CStatement a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | A statement specifies an action to be performed in sequence (C99 6.8.3).
  -- Unless specified otherwise, the semantics of statements (e.g. that 
  -- @break@ and @continue@ may only appear inside @for@/@while@ loops) are 
  -- not enforced by the parser, but will fail to compile in any modern C compiler.
  data CStatement a
    -- | The GCC syntax for inline assembly.
    = AsmStmt (Maybe CTypeQualifier) (CAsmOperand a) 
    -- | The @break@ statement. Should only appear inside loop constructs.
    | BreakStmt 
    -- | The @case@ statement, taking the form of @case expr: statement@.
    -- Should only appear inside the bodies of @switch@ statements.
    | CaseStmt (CExpr a) (CStatement a)
    -- | Compound statements are blocks of code (C99 6.8.2). They are composed
    -- of either 'CDeclaration' or 'CStatement' types, wrapped by the 
    -- 'BlockItem' type synonym.
    | CompoundStmt [CBlockItem a]
    -- | The @continue@ statement. Should only appear inside loop constructs.
    | ContinueStmt 
    -- | The @default@ statement, taking the form of @default: statement@.
    -- Should only appear inside of loop constructs.
    | DefaultStmt (CStatement a)
    -- | The do-while loop, taking the form of @do statement while expr@.
    | DoWhileStmt (CStatement a) (CExpr a)
    -- | The empty statement (i.e. just a semicolon.) May disappear in the future;
    -- if so, 'ExpressionStmt' will have a 'Maybe' 'CExpr' as its body.
    | EmptyStmt
    -- | A simple expression statement, i.e. that which evaluates its body
    -- and discards the result.
    | ExpressionStmt (CExpr a)
    -- | Old-style @for@ loops (C99 6.8.5.3). 
    | ForStmt (Maybe (CExpr a)) (Maybe (CExpr a)) (Maybe (CExpr a)) (CStatement a)
    -- | New-style @for@-loops (C99 6.8.5.3.1).
    | ForDeclStmt (CDeclaration a) (Maybe (CExpr a)) (Maybe (CExpr a)) (CStatement a)
    -- | The @goto@ statement.
    | GotoStmt (CExpr a)
    -- | The @if@ statement, taking the form of @if (cond) statement else? statement?@.
    | IfStmt (CExpr a) (CStatement a) (Maybe (CStatement a))
    -- | Labeled statements. As a GNU extension, labels may have the @__attribute__((unused))@ 
    -- attribute added to them to avoid warnings when compiling with @-Wall@.
    | LabeledStmt String [CAttribute a] (CStatement a)
    -- | The @return@ statement, of the form @return expr?@
    | ReturnStmt (Maybe (CExpr a))
    -- | The @switch@ statement, which should take the form of @switch (constant) compound-stmt@.
    | SwitchStmt (CExpr a) (CStatement a)
    -- | The @while@ statement, of the form @while expr statement@.
    | WhileStmt (CExpr a) (CStatement a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | There are two types of inline assembly: the first is the standard call to @asm()@, 
  -- which is identical to a function call in syntax (except for the fact that it can only 
  -- take a string as its parameter). The second is GCC assembly syntax, which takes the form of
  -- @asm( instructions : output-operands* : input-operands : clobbered-registers* );@
  data CAsmOperand a
    = Simple (CStringLiteral a)
    | GCCAsm (CStringLiteral a) [CAsmArgument a] [CAsmArgument a] [CStringLiteral a]
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Represents an output or input value in GCC assembly syntax. Takes the form of 
  -- | @string (variable)?@.
  data CAsmArgument a
    = CAsmArgument (CStringLiteral a) (Maybe (CExpr a))
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | A C function (C99 6.9.1).
  -- Invariant: The final 'CStatement' will always be a 'CompoundStmt', and the 
  -- provided 'CDeclarator' will always be named.
  data CFunction a = CFunction [CSpecifier a] (CDeclarator a) (CStatement a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | External declarations (C99 6.9). Wraps either a 'CFunction' or 'CDeclaration'.
  data CExternal a 
    = FunctionDecl (CFunction a)
    | ExternDecl (CDeclaration a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | C expressions (C99 6.5).
  -- Please note that the comma operator is currently unimplemented.
  data CExpr a
    = Constant (CLiteral a)
    | Comma [(CExpr a)]
    | Identifier String
    | Index (CExpr a) (CExpr a)
    | Call (CExpr a) [(CExpr a)]
    | CCast (CTypeName a) (CExpr a)
    | UnaryOp String (CExpr a)
    | BinaryOp String (CExpr a) (CExpr a)
    | TernaryOp (CExpr a) (CExpr a) (CExpr a)
    -- | Whereas sizeof(variable) parses as a function call, sizeof(type) needs its own node.
    | SizeOfType (CTypeName a)
    | CBuiltin (CBuiltinExpr a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | A string literal newtype to provide a modicum of type safety in the AST.
  newtype CStringLiteral a = CStringLiteral {
    getExpr :: (CExpr a)
  } deriving (Eq, Show, Functor, Typeable, Data)
  
  -- TODO: Expand this to include __builtin_offsetof and __builtin_types_compatible_p
  -- | GNU/clang built-in functions that are exposed after preprocessing.
  data CBuiltinExpr a
    -- | Corresponds to @__builtin_va_arg(id, type)@.
    = BuiltinVaArg (CExpr a) (CTypeName a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Storage class specifiers (C99 6.7.1).
  -- As an extension, @__attribute__(())@ is considered a storage specifier.
  data CStorageSpecifier a
    = CAuto
    | CRegister
    | CStatic
    | CExtern
    | CTypedef
    | CAttr (CAttribute a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Type qualifiers (C99 6.7.3) and function specifiers (C99 6.7.4).
  -- Please note that the 'FInline' qualifier must only be applied to functions.
  data CTypeQualifier
    = CConst
    | CRestrict
    | CVolatile
    | CInline
    deriving (Eq, Show, Typeable, Data)
  
  -- | C qualifiers and specifiers.
  data CSpecifier a 
    = TSpec (CTypeSpecifier a)
    | TQual CTypeQualifier
    | SSpec (CStorageSpecifier a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | C type specifiers (6.7.2).
  -- As a GNU extension, @typeof(expr)@ is supported.
  data CTypeSpecifier a
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
     -- | TBuiltin String
     -- | TStructOrUnion (Maybe String) Bool [CField a] [CAttribute a]
     -- | TEnumeration (Maybe String) [CEnumerator a] [CAttribute a]
     -- | TTypedef String (CTypeName a)
     | TTypeOfExpr (CExpr a)
     deriving (Eq, Show, Functor, Typeable, Data)
     
  type ASTTypeSpecifier = Co CTypeSpecifier

  -- | C enumeration specifiers (C99 6.7.2.2).
  data CEnumerator a
    = EnumIdent String
    | EnumAssign String (CExpr a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute a = CAttribute [(CExpr a)]
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Record type that wraps the various fields a declaration may have.
  data CDeclInfo a = CDeclInfo {
    contents :: Maybe (CDeclarator a),
    initVal :: Maybe (CInitializer a),
    size :: Maybe (CExpr a)
  } deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | C declarations (C99 6.7).
  -- This method of structuring declarations was innovated by Benedikt Huber.
  data CDeclaration a
    = CDeclaration [CSpecifier a] [CDeclInfo a]
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Represents C type names. These have a number of invariants: there will 
  -- be at least one 'CSpecifier', at most one 'CDeclInfo', which may contain a 
  -- declarator (if it is not 'Nothing', it will be unnamed) and will not have an 'Initializer' or be sized..
  newtype CTypeName a = CTypeName (CDeclaration a) deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Represents C parameters. There will be at least one 'Specifier', and only 
  -- one 'CDeclInfo', which will contain a possibly-named declarator
  -- and no initializer or size.
  newtype CParameter a = CParameter (CDeclaration a) deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Represents fields of structs or unions. There will be at least one specifier,
  -- at least one 'CDeclInfo', all of which will not have an initVal (but may be 
  -- named, sized, named and sized, or unnamed and sized.)
  newtype CField a = CField (CDeclaration a) deriving (Eq, Show, Functor, Typeable, Data)
  
  -- As a GNU extension, the user can specify the assembly name for a C function 
  -- or variable.
  type CAsmName = Maybe String
  
  -- | C declarators, both abstract and concrete (C99 6.7.5 and 6.7.6).
  data CDeclarator a
   = CDeclarator {
      declName :: Maybe String,
      derived :: [CDerivedDeclarator a],
      asmName :: CAsmName,
      declAttributes :: [CAttribute a]
   } deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | C designators, i.e. that which can appear inside compound initialization statements.
  data CDesignator a
    = ArrayDesignator (CExpr a)
    | MemberDesignator String
    deriving (Show, Eq, Typeable, Data)
  
  -- | C initializers (C99 6.7.8). Initialization types can contain one 
  -- expression or a bracketed list of initializers.
  data CInitializer a
    = CInitExpression (CExpr a)
    | CInitList a
    deriving (Eq, Show, Functor, Typeable, Data)
  
  -- | Represents the deconstructed initializers.
  type CInitList a = [([(CDesignator a)], (CInitializer a))]
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  -- In the future, Apple's extension for blocks (declared with @^@) may be added.
  data CDerivedDeclarator a
   = Pointer [CTypeQualifier]
   | Array [CTypeQualifier] (Maybe (CExpr a))
   | DerivedFunction [CParameter a] Bool
   deriving (Eq, Show, Functor, Typeable, Data)
 
  data ReifiedType a
    = OtherVoid
    | OtherChar
    | OtherShort
    | OtherInt
    | OtherLong
    | OtherInt128
    | OtherUInt128
    | OtherFloat
    | OtherDouble
    | OtherSigned
    | OtherUnsigned
    | OtherBool
    | Other (CExpr a)
    deriving (Eq, Show, Functor, Typeable, Data)
  
  type ASGReifiedType = Co ReifiedType
  
  -- | C literals.
  data CLiteral a
    = CInteger Int
    | CChar Char
    | CFloat String -- | Strings don't lose precision, unlike Doubles.
    | CString String
    deriving (Eq, Show, Functor, Typeable, Data)
  
  type ASTLiteral = Co CLiteral
  
  -- instance Show ASGReifiedType where
  --   show (In OtherVoid) = "void"
  --   show _ = undefined
  -- 
  -- foldExpr :: Functor f => (f a -> a) -> Co f -> a 
  -- foldExpr f (In t) = f (fmap (foldExpr f) t)
  -- 
  -- class (Functor f) => Eval f g | f -> g, g -> f where
  --   evalAlgebra :: (f g) -> g
  -- 
  -- 
  -- instance Eval CExpr (Co CExpr) where
  --   evalAlgebra = In
  --   
  -- instance Eval CTypeSpecifier ASGReifiedType where
  --   evalAlgebra TVoid = In OtherVoid
  --   evalAlgebra TChar = In OtherChar
  --   evalAlgebra TShort = In OtherShort
  --   evalAlgebra TInt = In OtherInt
  --   evalAlgebra TLong = In OtherLong
  --   evalAlgebra TInt128 = In OtherInt128
  --   evalAlgebra TUInt128 = In OtherUInt128
  --   evalAlgebra TFloat = In OtherFloat
  --   evalAlgebra TDouble = In OtherDouble
  --   evalAlgebra TSigned = In OtherSigned
  --   evalAlgebra TUnsigned = In OtherUnsigned
  --   evalAlgebra TBool = In OtherBool
  --   evalAlgebra (TTypeOfExpr a) = In $ Other $ (foldExpr evalAlgebra a)
    
