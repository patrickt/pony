module Language.C.Declarations
  ( declaration
  , abstractDeclarator
  , sizedDeclaration
  , typeName
  , declarator
  , parameter )
where
  
  import Control.Monad (when)
  import Data.Either
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Expressions
  import Language.C.Lexer as L
  import Language.C.Parser
  import Language.C.Specifiers
  
  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a
  
  -- | C99 6.7
  declaration :: Parser CDeclaration
  declaration = do
    specs <- some specifier
    decls <- (L.commaSep initDeclarator <* L.semi)
    let ret = TopLevel specs decls
    when (head specs == SSpec STypedef) $ do
      case (fst3 (head decls)) of
        (Named name _ _) -> updateState $ addTypeDef name (TopLevel (tail specs) decls)
        x -> fail "what?"
    return $ ret
  
  sizedDeclaration :: Parser CDeclaration
  sizedDeclaration = pure TopLevel <*> some specifier
                                   <*> (L.commaSep sizedDeclarator) <* L.semi
  
  parameter :: Parser CDeclaration
  parameter = pure Parameter <*> some specifier 
                             <*> declarator
  
  typeName :: Parser CDeclaration
  typeName = pure TypeName <*> some specifier 
                           <*> optional declarator
  
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
    when (notNull dots
     && (not (singleton dots) 
         || (null params && notNull dots)
         || (singleton dots && last given /= Right ())))
     (unexpected "ellipsis")
    return $ Function params $ notNull dots
  
  array :: Parser DerivedDeclarator
  array = do
    (quals, expr) <- L.brackets lunacy
    return $ Array quals expr
    where lunacy = pure (,) <*> many typeQualifier <*> optional expression

  -- ISO C99 standard, section 6.7.5.
  pointer :: Parser DerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser (CDeclarator, Initializer, CSize)
  initDeclarator = pure (,,) <*> declarator 
                             <*> option Uninitialized assignment 
                             <*> pure Unsized
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser (CDeclarator, Initializer, CSize)
  sizedDeclarator = pure (,,) <*> declarator
                              <*> pure Uninitialized
                              <*> option Unsized (Sized <$> (L.colon *> expression))
  
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
    
  attributes :: Parser [CExpr]
  attributes = L.reserved "__attribute__" *> L.parens (L.parens $ L.commaSep1 expression)
  
  abstractDeclarator :: Parser CDeclarator
  abstractDeclarator = do
    ptrs <- many pointer
    arrayOrFunction <- many (try array <|> func)
    let derived = ptrs ++ arrayOrFunction
    return $ Abstract derived
  
  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    direct' <- optionMaybe direct
    arrayOrFunction <- many (try array <|> func)
    asm <- optional (try asmName)
    attrs <- many attributes
    let derived = ptrs ++ arrayOrFunction
    case direct' of
      (Just (Single s)) -> return $ Named s derived asm
      (Just (Parenthesized (Named s decls _))) -> return $ Named s (decls ++ derived) asm
      -- is this even possible?
      (Just (Parenthesized (Abstract decls))) -> return $ Abstract (decls ++ derived)
      Nothing -> return $ Abstract derived