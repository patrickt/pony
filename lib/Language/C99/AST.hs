module Language.C99.AST
  
  where
  
  import GHC.Generics
  import Data.Default
  import Data.Generics hiding (Generic, GT)
  import Language.C99.Literals
  import Language.C99.Syntax
  import Language.Pony.Overture
  import Data.List (sort)
  
  -- TODO: Add position information to all of the types, etc.
  -- TODO: Rename CAsmOperand and CAsmArgument to something more descriptive.
  
  -- | There are two types of inline assembly: the first is the standard call to @asm()@, 
  -- which is identical to a function call in syntax (except for the fact that it can only 
  -- take a string as its parameter). The second is GCC assembly syntax, which takes the form of
  -- @asm( instructions : output-operands* : input-operands : clobbered-registers* );@
  data CAsmOperand 
    = Simple CStringLiteral
    | GCCAsm CStringLiteral [CAsmArgument] [CAsmArgument] [CStringLiteral]
    deriving (Show, Eq)
  
  -- | Represents an output or input value in GCC assembly syntax. Takes the form of 
  -- | @string (variable)?@.
  data CAsmArgument 
    = CAsmArgument CStringLiteral (Maybe CSyn)
    deriving (Show, Eq)
  
  -- | A C function (C99 6.9.1).
  -- Invariant: The final 'CStatement' will always be a 'CompoundStmt', and the 
  -- provided 'CDeclarator' will always be named.
  data CFunction = CFunction [CSpecifier] CDeclarator CSyn
    deriving (Eq, Show)
  
  
  -- | A string literal newtype to provide a modicum of type safety in the AST.
  newtype CStringLiteral = CStringLiteral {
    getExpr :: CSyn
  } deriving (Eq, Show)
  
  -- TODO: Expand this to include __builtin_offsetof and __builtin_types_compatible_p
  -- | GNU/clang built-in functions that are exposed after preprocessing.
  data CBuiltinExpr
    -- | Corresponds to @__builtin_va_arg(id, type)@.
    = BuiltinVaArg CSyn CSyn
    deriving (Eq, Show)
  
  -- | Storage class specifiers (C99 6.7.1).
  -- As an extension, @__attribute__(())@ is considered a storage specifier.
  data CStorageSpecifier
    = CTypedef
    | CAuto
    | CRegister
    | CStatic
    | CExtern
    | CAttr CAttribute
    deriving (Eq, Show, Ord)
  
  -- | Type qualifiers (C99 6.7.3) and function specifiers (C99 6.7.4).
  -- Please note that the 'FInline' qualifier must only be applied to functions.
  data CTypeQualifier
    = CConst
    | CRestrict
    | CVolatile
    | CInline
    deriving (Eq, Show)
  
  instance Ord CTypeQualifier where
    compare _ _ = EQ
  
  -- | C qualifiers and specifiers.
  data CSpecifier 
    = SSpec CStorageSpecifier
    | TQual CTypeQualifier
    | TSpec CTypeSpecifier
    deriving (Eq, Show, Ord)
  
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
     | TTypedef String CSyn
     | TTypeOfExpr CSyn
     deriving (Eq, Show)
  
  -- Signedness comes first, then modifiers, then base types
  instance Ord CTypeSpecifier where
    compare a b 
      | isSignedness a = LT
      | isSignedness b = GT
      | isModifier a = LT
      | isModifier b = GT
      | otherwise = EQ
      where
        isSignedness a = (a == TSigned) || (a == TUnsigned)
        isModifier a = (a == TShort) || (a == TLong)
      
  -- | C enumeration specifiers (C99 6.7.2.2).
  data CEnumerator 
    = EnumIdent String
    | EnumAssign String CSyn
    deriving (Eq, Show)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute = CAttribute [CSyn]
    deriving (Eq, Show)
    
  instance Ord CAttribute
  
  -- | Record type that wraps the various fields a declaration may have.
  data CDeclInfo = CDeclInfo {
    contents :: CDeclarator,
    initVal :: Maybe CInitializer,
    size :: Maybe CSyn
  } deriving (Show, Eq)
  
  infoName :: CDeclInfo -> Maybe String
  infoName = declName . contents
  
  -- TODO: these Default things are stupid
  instance Default CDeclInfo where def = CDeclInfo def Nothing Nothing
  
  -- | C declarations (C99 6.7).
  -- This method of structuring declarations was innovated by Benedikt Huber.
  data CDeclaration = CDeclaration {
      declrSpecifiers :: [CSpecifier],
      declrInfos :: [CDeclInfo]
  } deriving (Eq, Show)
  
  -- | Represents fields of structs or unions. There will be at least one specifier,
  -- at least one 'CDeclInfo', all of which will not have an initVal (but may be 
  -- named, sized, named and sized, or unnamed and sized.)
  newtype CField = CField { unCField :: CDeclaration } deriving (Show, Eq)
  
  -- As a GNU extension, the user can specify the assembly name for a C function 
  -- or variable.
  type CAsmName = Maybe String
  
  data CDeclaratorBody 
    = CIdentBody String
    | CParenBody CDeclarator
    | CEmptyBody
    deriving (Show, Eq)
    
  declName :: CDeclarator -> Maybe String
  declName d = case (declBody d) of
    (CIdentBody s) -> Just s
    (CParenBody d) -> declName d
    CEmptyBody -> Nothing
    
  derived :: CDeclarator -> [CDerivedDeclarator]
  derived (CDeclarator {pointers, modifiers, ..}) = pointers ++ modifiers
  
  -- | C declarators, both abstract and concrete (C99 6.7.5 and 6.7.6).
  data CDeclarator 
   = CDeclarator 
     { pointers :: [CDerivedDeclarator]
     , declBody :: CDeclaratorBody
     , modifiers :: [CDerivedDeclarator]
     , asmName :: CAsmName
     , declAttributes :: [CAttribute]
   } deriving (Eq, Show)
   
  instance Default CDeclarator where def = CDeclarator [] CEmptyBody [] Nothing []
  
  -- | C designators, i.e. that which can appear inside compound initialization statements.
  data CDesignator 
    = ArrayDesignator CSyn
    | MemberDesignator String
    deriving (Show, Eq)
  
  -- | C initializers (C99 6.7.8). Initialization types can contain one 
  -- expression or a bracketed list of initializers.
  data CInitializer 
    = CInitExpression CSyn
    | CInitList [CInitializerSubfield]
    deriving (Eq, Show)
  
  -- | Represents the deconstructed initializers.
  data CInitializerSubfield = CInitializerSubfield [CDesignator] CInitializer
    deriving (Show, Eq)
  
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  -- In the future, Apple's extension for blocks (declared with @^@) may be added.
  data CDerivedDeclarator
   = Pointer [CTypeQualifier]
   | Array [CTypeQualifier] CSyn
   | DerivedFunction [CSyn] Bool
   deriving (Eq, Show)
