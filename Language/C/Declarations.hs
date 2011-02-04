module Language.C.Declarations
  ( declaration
  , abstractDeclarator
  , sizedDeclaration
  , typeName
  , declarator
  , parameter )
where
  
  -- Lasciate ogne speranza, voi ch'intrate.
  
  import Control.Monad (when)
  import Data.Either
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Expressions
  import Language.C.Lexer as L
  import Language.C.Parser
  import Language.C.Specifiers
  import Language.C.Miscellany
  
  -- | C99 6.7
  declaration :: Parser CDeclaration
  declaration = do
    specs <- some specifier
    decls <- (L.commaSep initDeclarator <* L.semi)
    when (head specs == SSpec STypedef) $
      case (fst3 (head decls)) of
        (Just (Named name _ _ _)) -> updateState $ addTypeDef name (CDeclaration (tail specs) decls)
        x -> fail "what?"
    return $ CDeclaration specs decls
  
  -- | Sized declarations can only appear in structure bodies.
  sizedDeclaration :: Parser CDeclaration
  sizedDeclaration = pure CDeclaration <*> some specifier
                                       <*> L.commaSep sizedDeclarator <* L.semi
  
  parameter :: Parser CDeclaration
  parameter = pure declaration <*> some specifier <*> declarator where
    declaration s d = CDeclaration s [(Just d, Nothing, Nothing)]
  
  typeName :: Parser CDeclaration
  typeName = pure declaration <*> some specifier <*> optional declarator where
    declaration s d = CDeclaration s [(d, Nothing, Nothing)]
  
  func :: Parser DerivedDeclarator
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
    return $ Function params $ notNull dots
  
  array :: Parser DerivedDeclarator
  array = do
    (quals, expr) <- L.brackets lunacy
    return $ Array quals expr
    where lunacy = pure (,) <*> many typeQualifier <*> optional expression

  -- ISO C99 standard, section 6.7.5.
  pointer :: Parser DerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser (Maybe CDeclarator, Maybe Initializer, Maybe CExpr)
  initDeclarator = pure (,,) <*> Just <$> declarator 
                             <*> optional assignment 
                             <*> pure Nothing
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser (Maybe CDeclarator, Maybe Initializer, Maybe CExpr)
  sizedDeclarator = pure (,,) <*> Just <$> declarator
                              <*> pure Nothing
                              <*> optional (L.colon *> expression)
  
  initializer :: Parser Initializer
  initializer = (InitList <$> L.braces (L.commaSep1 initializer)) <|> (InitExpression <$> expression)

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
  
  abstractDeclarator :: Parser CDeclarator
  abstractDeclarator = do
    ptrs <- many pointer
    arrayOrFunction <- many (try array <|> func)
    let derived = ptrs ++ arrayOrFunction
    return $ Abstract derived []
  
  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    direct' <- optional direct
    arrayOrFunction <- many (try array <|> func)
    asm <- optional (try asmName)
    attrs <- many attribute
    let derived = ptrs ++ arrayOrFunction
    case direct' of
      (Just (Single s)) -> return $ Named s derived asm attrs
      -- discarding the result of __attributes__ here; could this be a bug?
      (Just (Parenthesized (Named s decls _ _))) -> return $ Named s (derived ++ decls) asm attrs
      -- is this even possible?
      (Just (Parenthesized (Abstract decls _))) -> return $ Abstract (derived ++ decls) attrs
      Nothing -> return $ Abstract derived attrs