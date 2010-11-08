module Language.C.Declarators where
  
  import Text.Parsec
  import Text.Parsec.String
  import Language.C.Lexer as L
  import Language.C.AST
  
  p qual name = L.reserved name >> return qual
  
  typeQualifier :: Parser TypeQualifier
  typeQualifier = choice 
    [ p "const" QConst
    , p "restrict" QRestrict
    , p "volatile" QVolatile 
    ] <?> "type qualifier"
  
  -- I need to put something into the parser state so that it can handle typedefs
  typeSpecifier :: Parser TypeSpecifier
  typeSpecifier = choice 
    [ p "void" TVoid 
    , p "char" TChar
    , p "short" TShort
    , p "int" TInt
    , p "long" TLong
    , p "float" TFloat
    , p "double" TDouble
    , p "signed" TSigned
    , p "unsigned" TUnsigned
    ] <?> "type specifier"
  
  storageSpecifier :: Parser StorageSpecifier
  storageSpecifier = choice
    [ p "typedef" STypedef
    , p "extern" SExtern
    , p "static" SStatic
    , p "auto" SAuto
    , p "register" SRegister
    ]
  
  functionSpecifier :: Parser StorageSpecifier
  functionSpecifier = p "inline" FInline
  