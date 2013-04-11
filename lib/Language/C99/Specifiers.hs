module Language.C99.Specifiers
  ( typeQualifier
  , typeSpecifier
  , storageSpecifier
  , specifier
  , specifiers
  , attribute
  )

where
  
  import Data.Functor.Fix
  import Language.C99.Expressions (expression, constantExpression, identifier, opt')
  import Language.C99.Internal
  import Data.List (sort)
  import Language.C99.Parser
  import Language.C99.Syntax
  import Language.Pony.Overture
  import qualified Data.Map as M
  import qualified Language.C99.Lexer as L
  import {-# SOURCE #-} Language.C99.Declarations

  
  as name qual = qual <$ L.reserved name
  
  -- -- You can put reserved words like "const" inside __attribute__ declarations, so we try parsing an expression then give up and just read letters
  customAttribute :: Parser CSyn
  customAttribute = try expression <|> (Fix <$> Name <$> some letter)
  
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
    ] <?> "type qualifier"
  
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
    , TBuiltin <$> (try $ L.symbol "__builtin_va_list")
    -- , typeof
    , enum
    , composite "struct" struct'
    , composite "union" union'
    -- , try builtin
    , try lookupTypedef
    ] <?> "type specifier"
    where
      -- builtin = TBuiltin <$> L.symbol "__builtin_va_list"
      -- typeof = TTypeOfExpr <$> ((L.reserved "typeof" <|> L.reserved "__typeof__") *> expression)
      -- -- TODO: if name is Nothing and idents is empty, fail
      enum = TEnumeration <$> (L.reserved "enum" *> opt' identifier)
                          <*> option [] (L.braces (enumerator `sepEndBy1` L.symbol ","))
                          <*> many attribute
      composite s t = TStructOrUnion <$> (L.reserved s *> opt' identifier)
                                     <*> pure t
                                     <*> option [] (L.braces (concat <$> (some sizedDeclarations)))
                                     <*> many attribute
      lookupTypedef = do
        defs <- getState
        ident <- L.identifier
        case M.lookup ident (typedefs defs) of
          (Just specs) -> return (TTypedef ident specs)
          Nothing -> fail "could not find typedef"
  enumerator :: Parser CSyn
  enumerator = do
    i <- identifier
    val <- optional (L.reservedOp "=" *> constantExpression)
    return $ maybe i (binary' i (name' "=")) val

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
  
  specifiers :: Parser [CSpecifier]
  specifiers = sort <$> some specifier
  
