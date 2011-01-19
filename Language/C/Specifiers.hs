module Language.C.Specifiers
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  , specifier
  , attribute
  )

where
  
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import {-# SOURCE #-} Language.C.Declarations
  import Language.C.Expressions (expression, constantExpression)
  
  as name qual = L.reserved name >> return qual
  
  attribute :: Parser CAttribute
  attribute = pure CAttribute <*> (L.reserved "__attribute__" *> L.parens (L.parens $ L.commaSep1 expression))
  
  typeQualifier :: Parser TypeQualifier
  typeQualifier = choice 
    [ "const" `as` QConst
    , "restrict" `as` QRestrict
    , "volatile" `as` QVolatile 
    , "inline" `as` FInline -- TODO: ensure that this only goes on functions.
    , "__inline" `as` FInline
    , "__inline__" `as` FInline
    ] <?> "type qualifier"
    
  enumerator :: Parser Enumerator
  enumerator = do
    ident <- L.identifier
    value <- optional (L.reservedOp "=" *> constantExpression)
    return $ maybe (EnumIdent ident) (EnumAssign ident) value
  
  typeSpecifier :: Parser TypeSpecifier
  typeSpecifier = choice 
    [ "void" `as` TVoid 
    , "char" `as` TChar
    , "short" `as` TShort
    , "int" `as` TInt
    , "long" `as` TLong
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
        case (lookup ident (typedefs defs)) of
          (Just specs) -> return (TTypedef ident specs)
          Nothing -> fail "could not find typedef"

  storageSpecifier :: Parser StorageSpecifier
  storageSpecifier = choice
   [ "typedef" `as` STypedef
   , "extern" `as` SExtern
   , "static" `as` SStatic
   , "auto" `as` SAuto
   , "register" `as` SRegister
   , SAttribute <$> attribute 
   ] <?> "storage specifier"
   where
    

  specifier :: Parser Specifier
  specifier = choice 
    [ TQual <$> typeQualifier
    , TSpec <$> typeSpecifier
    , SSpec <$> storageSpecifier
    ]
  
