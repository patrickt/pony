module Language.C.Declarators
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  )

where
  
  import Text.Parsec
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  
  into name qual = L.reserved name >> return qual
  
  typeQualifier :: Parser TypeQualifier
  typeQualifier = choice 
    [ "const" `into` QConst
    , "restrict" `into` QRestrict
    , "volatile" `into` QVolatile 
    ] <?> "type qualifier"
  
  -- I need to put something into the parser state so that it can handle typedefs
  typeSpecifier :: Parser TypeSpecifier
  typeSpecifier = choice 
    [ "void" `into` TVoid 
    , "char" `into` TChar
    , "short" `into` TShort
    , "int" `into` TInt
    , "long" `into` TLong
    , "float" `into` TFloat
    , "double" `into` TDouble
    , "signed" `into` TSigned
    , "unsigned" `into` TUnsigned
    ] <?> "type specifier"
  
  storageSpecifier :: Parser StorageSpecifier
  storageSpecifier = choice
    [ "typedef" `into` STypedef
    , "extern" `into` SExtern
    , "static" `into` SStatic
    , "auto" `into` SAuto
    , "register" `into` SRegister
    ]
  
  functionSpecifier :: Parser FunctionSpecifier
  functionSpecifier = "inline" `into` FInline
  