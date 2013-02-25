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
    let (Just name) = contents >>= declName
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
  typeName = CTypeName <$> some specifier <*> optional declarator
  
  func :: Parser CDerivedDeclarator
  func = L.parens $ do
    -- Since I can't figure out an elegant way of ensuring that only the last 
    -- parameter is (optionally) an ellipsis, we parse instances of 
    -- `Either CDeclaration ()` and do some compile-time sanity checking 
    -- to ensure that nobody puts ellipses in the wrong place.
    given <- L.commaSep ((Left <$> parameter) <|> (Right <$> L.reservedOp "..."))
    let params = lefts given
    let dots = rights given
    let notNull = not . null
    let singleton x = length x == 1
    -- there must be only one ..., and it must be the last element of the function
    when (notNull dots && (not (singleton dots) || null params || last given /= Right ())) (unexpected "ellipsis")
    return $ DerivedFunction params $ notNull dots
  
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
  initDeclarator = CDeclInfo <$> Just <$> declarator 
                             <*> optional assignment 
                             <*> pure Nothing
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser CDeclInfo
  sizedDeclarator = pure CDeclInfo <*> Just <$> declarator
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
    
  asmString :: Parser ()
  asmString = choice [
                       L.reserved "__asm"
                     , L.reserved "__asm__"
                     ]

  asmName :: Parser String
  asmName = asmString *> L.parens (some $ noneOf ")")
  
  
  -- CDeclarator 
  
  -- Declarators (C99 6.7.5). May be named or unnamed.
  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    direct' <- optional direct
    arrayOrFunction <- many (try array <|> func)
    asm <- optional (try asmName)
    attrs <- many attribute
    let derived = ptrs ++ arrayOrFunction
    case direct' of
      (Just (Single s)) -> return $ CDeclarator (Just s) derived asm attrs
      -- discarding the result of __attributes__ here; could this be a bug?
      (Just (Parenthesized (CDeclarator n _ _ _))) -> return $ CDeclarator n derived asm attrs
      Nothing -> do
        guard $ not $ null derived
        return $ CDeclarator Nothing derived asm attrs
