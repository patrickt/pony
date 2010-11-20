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
  
  as name qual = L.reserved name >> return qual
  into = flip fmap
  
  typeQualifier :: Parser TypeQualifier
  typeQualifier = choice 
    [ "const" `as` QConst
    , "restrict" `as` QRestrict
    , "volatile" `as` QVolatile 
    , "inline" `as` FInline -- TODO: ensure that this only goes on functions.
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
    , try lookupTypedef
    ] <?> "type specifier"
    where
      lookupTypedef = do
        defs <- getState
        ident <- identifier
        case (lookup ident (typedefs defs)) of
          (Just spec) -> return (TTypedef ident spec)
          Nothing -> fail "typedef not found"
  
  storageSpecifier :: Parser StorageSpecifier
  storageSpecifier = choice
    [ "typedef" `as` STypedef
    , "extern" `as` SExtern
    , "static" `as` SStatic
    , "auto" `as` SAuto
    , "register" `as` SRegister 
    ]
  
  specifier :: Parser Specifier
  specifier = choice 
    [ typeQualifier `into` TQual
    , typeSpecifier `into` TSpec
    , storageSpecifier `into` SSpec
    ]
  
