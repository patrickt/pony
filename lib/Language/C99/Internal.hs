module Language.C99.Internal
  
  where
  
  import Language.C99.Syntax
  
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
     | TStructOrUnion CSyn CSyn [CSyn] [CAttribute]
     | TEnumeration CSyn [CSyn] [CAttribute]
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
        isSignedness t = t `elem` [TSigned, TUnsigned]
        isModifier t = t `elem` [TLong, TShort]
      
  -- | C enumeration specifiers (C99 6.7.2.2).
  data CEnumerator 
    = EnumIdent String
    | EnumAssign String CSyn
    deriving (Eq, Show)
  
  -- | C @__attribute__(())@ specifications. 
  data CAttribute = CAttribute [CSyn]
    deriving (Eq, Show)
    
  instance Ord CAttribute
  
  
