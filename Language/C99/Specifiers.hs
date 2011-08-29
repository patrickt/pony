module Language.C99.Specifiers
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  , specifier
  , attribute
  )

where
  
  import Language.C99.Parser
  import Language.C99.Lexer as L
  import Language.C99.AST
  import {-# SOURCE #-} Language.C99.Declarations
  import Language.C99.Expressions (expression, constantExpression)
  
  as name qual = L.reserved name >> return qual
  
  attribute :: Parser CAttribute
  attribute = pure CAttribute <*> (L.reserved "__attribute__" *> L.parens (L.parens $ L.commaSep1 expression))
  
  typeQualifier :: Parser CTypeQualifier
  typeQualifier = choice 
    [ "const" `as` CConst
    , "restrict" `as` CRestrict
    , "volatile" `as` CVolatile 
    , "inline" `as` CInline -- TODO: ensure that this only goes on functions.
    , "__inline" `as` CInline
    , "__inline__" `as` CInline
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
      builtin = pure TBuiltin <*> L.symbol "__builtin_va_list"
      typeof = pure TTypeOfExpr <*> ((L.reserved "typeof" <|> L.reserved "__typeof__") *> expression)
      -- TODO: if name is Nothing and idents is empty, fail
      enum = pure TEnumeration <*> (L.reserved "enum" *> optional identifier)
                               <*> option [] (L.braces (enumerator `sepEndBy1` L.symbol ","))
                               <*> many attribute
      struct = pure TStructOrUnion <*> (L.reserved "struct" *> optional identifier)
                                   <*> pure True
                                   <*> option [] (L.braces (some sizedDeclaration))
                                   <*> many attribute
      union = pure TStructOrUnion <*> (L.reserved "union" *> optional identifier)
                                  <*> pure False
                                  <*> option [] (L.braces (some sizedDeclaration))
                                  <*> many attribute
      lookupTypedef = do
        defs <- getState
        ident <- identifier
        case lookup ident (typedefs defs) of
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
  
