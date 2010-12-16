module Language.C.Specifiers
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  , specifier
  )

where
  
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import {-# SOURCE #-} Language.C.Declarations
  
  as name qual = L.reserved name >> return qual
  
  typeQualifier :: Parser TypeQualifier
  typeQualifier = choice 
    [ "const" `as` QConst
    , "restrict" `as` QRestrict
    , "volatile" `as` QVolatile 
    , "inline" `as` FInline -- TODO: ensure that this only goes on functions.
    , "__inline" `as` FInline
    ] <?> "type qualifier"
  
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
    , enum
    , struct
    , union
    , try lookupTypedef
    ] <?> "type specifier"
    where
      -- TODO: if name is Nothing and idents is empty, fail
      enum = pure TEnumeration <*> (L.reserved "enum" *> optional identifier)
                               <*> option [] (L.braces (L.commaSep1 identifier))
      struct = pure TStructOrUnion <*> (L.reserved "struct" *> optional identifier)
                                   <*> pure True
                                   <*> option [] (L.braces (some sizedDeclaration))
      union = pure TStructOrUnion <*> (L.reserved "union" *> optional identifier)
                                  <*> pure False
                                  <*> option [] (L.braces (some sizedDeclaration))
      lookupTypedef = do
        defs <- getState
        ident <- identifier
        case (lookup ident (typedefs defs)) of
          (Just spec) -> return (TTypedef ident spec)
          Nothing -> fail "could not find typedef"
  
  storageSpecifier :: Parser StorageSpecifier
  storageSpecifier = choice
    [ "typedef" `as` STypedef
    , "extern" `as` SExtern
    , "static" `as` SStatic
    , "auto" `as` SAuto
    , "register" `as` SRegister 
    ] <?> "storage specifier"
  
  specifier :: Parser Specifier
  specifier = choice 
    [ TQual <$> typeQualifier
    , TSpec <$> typeSpecifier
    , SSpec <$> storageSpecifier
    ]
  
