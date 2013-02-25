module Language.C99.Specifiers
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  , specifier
  , attribute
  )

where
  
  import qualified Data.Map as M
  import Language.C99.Parser
  import Language.C99.Lexer as L
  import Language.C99.AST
  import Language.C99.Literals
  import {-# SOURCE #-} Language.C99.Declarations
  import Language.C99.Expressions (expression, constantExpression)
  
  as name qual = qual <$ L.reserved name
  
  -- You can put reserved words like "const" inside __attribute__ declarations, so we try parsing an expression then give up and just read letters
  customAttribute :: Parser CExpr
  customAttribute = try expression <|> (Constant <$> CString <$> some letter)
  
  attribute :: Parser CAttribute
  attribute = CAttribute <$> (L.reserved "__attribute__" *> L.parens (L.parens (L.commaSep1 customAttribute)))
  
  typeQualifier :: Parser CTypeQualifier
  typeQualifier = choice 
    [ "const" `as` CConst
    , "restrict" `as` CRestrict
    , "volatile" `as` CVolatile 
    , "inline" `as` CInline
    , "__inline" `as` CInline
    , "__inline__" `as` CInline
    , "__const" `as` CConst
    , "__restrict" `as` CRestrict
    ] <?> "type qualifier"
    
  enumerator :: Parser CEnumerator
  enumerator = do
    ident <- L.identifier
    value <- optional (L.reservedOp "=" *> constantExpression)
    return $ maybe (EnumIdent ident) (EnumAssign ident) value
  
  typeSpecifier :: Parser CTypeSpecifier
  typeSpecifier = choice 
    [ "void" `as` TVoid 
    , "char" `as` TChar
    , "short" `as` TShort
    , "int" `as` TInt
    , "long" `as` TLong
    , "__int128_t" `as` TInt128
    , "__uint128_t" `as` TUInt128
    , "float" `as` TFloat
    , "double" `as` TDouble
    , "signed" `as` TSigned
    , "unsigned" `as` TUnsigned
    , "_Bool" `as` TBool
    , typeof
    , enum
    , struct
    , union
    , try builtin
    , try lookupTypedef
    ] <?> "type specifier"
    where
      builtin = TBuiltin <$> L.symbol "__builtin_va_list"
      typeof = TTypeOfExpr <$> ((L.reserved "typeof" <|> L.reserved "__typeof__") *> expression)
      -- TODO: if name is Nothing and idents is empty, fail
      enum = TEnumeration <$> (L.reserved "enum" *> optional identifier)
                          <*> option [] (L.braces (enumerator `sepEndBy1` L.symbol ","))
                          <*> many attribute
      struct = TStructOrUnion <$> (L.reserved "struct" *> optional identifier)
                              <*> pure True
                              <*> option [] (L.braces (some sizedDeclaration))
                              <*> many attribute
      union = TStructOrUnion <$> (L.reserved "union" *> optional identifier)
                             <*> pure False
                             <*> option [] (L.braces (some sizedDeclaration))
                             <*> many attribute
      lookupTypedef = do
        defs <- getState
        ident <- identifier
        case M.lookup ident (typedefs defs) of
          (Just specs) -> return (TTypedef ident specs)
          Nothing -> fail "could not find typedef"

  storageSpecifier :: Parser CStorageSpecifier
  storageSpecifier = choice
   [ "typedef" `as` CTypedef
   , "extern" `as` CExtern
   , "static" `as` CStatic
   , "auto" `as` CAuto
   , "register" `as` CRegister
   , CAttr <$> attribute 
   ] <?> "storage specifier"

  specifier :: Parser CSpecifier
  specifier = choice 
    [ TQual <$> typeQualifier
    , TSpec <$> typeSpecifier
    , SSpec <$> storageSpecifier
    ]
  
