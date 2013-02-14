module Language.C99.Declarations
  ( declaration
  , sizedDeclaration
  , typeName
  , declarator 
  )
where
  
  -- Lasciate ogne speranza, voi ch'intrate.
  
  import Control.Monad
  import Data.Either
  import Language.Pony.Overture
  import Language.C99.AST hiding (asmName)
  import Language.C99.Expressions
  import qualified Language.C99.Lexer as L
  import Language.C99.Parser
  import Language.C99.Specifiers
  
  -- | C99 6.7 - abstract and concrete declarations.
  declaration :: Parser CDeclaration
  declaration = declaration' >>= checkTypedefs
    where declaration' = CDeclaration <$> some specifier <*> L.commaSep initDeclarator <* L.semi
  
  checkTypedefs :: CDeclaration -> Parser CDeclaration
  checkTypedefs d@(CDeclaration (SSpec CTypedef : rest) ((CDeclInfo { contents, .. }) : _)) = do
    let (Just name) = declName contents
    updateState $ addTypeDef name (CTypeName rest contents)
    return d
  checkTypedefs x = return x
  
  -- | Sized declarations can only appear in the bodies of composite types.
  sizedDeclaration :: Parser CField
  sizedDeclaration = CField <$> (CDeclaration <$> some specifier
                                              <*> L.commaSep sizedDeclarator 
                                              <*  L.semi)
  
  -- | Parameter declarations. Must have types, but may be anonymous if they appear
  -- as a forward declaration.
  parameter :: Parser CParameter
  parameter = CParameter <$> some specifier <*> optional declarator
  
  -- | Type names, used in cast operations and typedefs.
  typeName :: Parser CTypeName
  typeName = CTypeName <$> some specifier <*> declarator
  
  func :: Parser CDerivedDeclarator
  func = L.parens $ do
    first <- optional parameter
    if isNothing first
      then return $ DerivedFunction [] False
      else do
        rests <- many $ try (L.comma *> parameter)
        ellip <- optional (L.comma *> L.reservedOp "...")
        return $ DerivedFunction (fromJust first : rests) (isJust ellip)
  
  -- This doesn't handle a lot of the stupid cases introduced by C99's variable-length arrays, e.g.
  -- [ static type-qualifier-list? assignment-expression ]
  -- [type-qualifier-list static assignment-expression]
  -- [type-qualifier-list? *]
  array :: Parser CDerivedDeclarator
  array = L.brackets (Array <$> many typeQualifier <*> optional expression)

  -- ISO C99 standard, section 6.7.5.
  pointer :: Parser CDerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser CDeclInfo
  initDeclarator = CDeclInfo <$> declarator 
                             <*> optional assignment 
                             <*> pure Nothing
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser CDeclInfo
  sizedDeclarator = CDeclInfo <$> declarator
                              <*> pure Nothing
                              <*> optional (L.colon *> expression)
                                  
  designator :: Parser CDesignator
  designator =  (ArrayDesignator  <$> L.brackets constant     <?> "array declaration")
            <|> (MemberDesignator <$> (L.dot *> L.identifier) <?> "dotted declaration")
  
  initList :: Parser [CInitializerSubfield]
  initList = L.braces (L.commaSep1 initSubfield)
  
  initSubfield :: Parser CInitializerSubfield
  initSubfield = do 
    -- first we look for the designators: .x for membership. don't think an array designator could get in here.
    desigs <- many designator
    -- if there are designators, e.g. .x, we require an explicit = statement to make an assignment.
    when (desigs /= []) (L.reservedOp "=")
    CInitializerSubfield <$> pure desigs <*> initializer
  
  initializer :: Parser CInitializer
  initializer = (CInitExpression <$> expression) <|> (CInitList <$> initList) 

  -- hack hack hack
  data DirectDeclarator 
    = Parenthesized CDeclarator
    | Single String 
  
  direct :: Parser DirectDeclarator
  direct = parens <|> ident where
    parens = Parenthesized <$> L.parens declarator
    ident = Single <$> L.identifier
    
  asmName :: Parser String
  asmName = L.reserved "__asm" *> L.parens (some $ noneOf ")")
  
  declaratorBody :: Parser CDeclaratorBody
  declaratorBody = choice
    [ CIdentBody <$> getIdent <$> identifier
    , CParenBody <$> L.parens declarator
    , pure CEmptyBody
    ]
  
  -- CDeclarator 
  
  -- Declarators (C99 6.7.5). May be named or unnamed.
  declarator :: Parser CDeclarator
  declarator = CDeclarator
    <$> many pointer
    <*> declaratorBody
    <*> many (try array <|> try func)
    <*> optional (try asmName)
    <*> many attribute
