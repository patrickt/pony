module Language.C.Declarations
  ( declaration
  , typeName )
where
  
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Expressions
  import Language.C.Lexer as L
  import Language.C.Parser
  import Language.C.Specifiers
  
  -- 6.7
  declaration :: Parser CDeclaration
  declaration = do
    -- declaration-specifiers
    specs <- many1 specifier
    -- init-declarator-list
    decls <- L.commaSep initDeclarator
    L.semi
    case decls of
      [(decl, maybeExpr)] -> return $ TopLevel specs decl maybeExpr
      more -> return $ Multiple specs more
      
  parameter :: Parser CDeclaration
  parameter = do
    -- declaration-specifiers
    specs <- many1 specifier
    -- init-declarator-list
    decls <- L.commaSep initDeclarator
    case decls of
      [(decl, maybeExpr)] -> return $ TopLevel specs decl maybeExpr
      more -> return $ Multiple specs more
  
  
  typeName :: Parser CDeclaration
  typeName = do
    specs <- many1 specifier
    decl <- optionMaybe declarator
    return $ TypeName specs decl
  
  func :: Parser DerivedDeclarator
  func = L.parens $ do
    params <- L.commaSep parameter
    isVariadic <- option False (L.reserved "..." >> return True)
    return $ Function params isVariadic

  array :: Parser DerivedDeclarator
  array = do
    (quals, expr) <- L.brackets lunacy
    return $ Array quals expr
    where
      lunacy = do
        q <- many typeQualifier
        e <- optionMaybe expression
        return (q, e)

  -- 6.7.5.
  -- this is gorgeous.
  pointer :: Parser DerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser (CDeclarator, Initializer)
  initDeclarator = do
    decl <- declarator
    init <- option Uninitialized assignment
    return (decl, init)
    where assignment = L.reservedOp "=" >> initializer
  
  initializer :: Parser Initializer
  initializer = InitExpression <$> expression

  -- hack hack hack
  data DirectDeclarator 
    = Parenthesized CDeclarator
    | Single String
  
  direct :: Parser DirectDeclarator
  direct = parens <|> ident where
    parens = Parenthesized <$> L.parens declarator
    ident = Single <$> L.identifier

  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    direct' <- optionMaybe direct
    arrayOrFunction <- many (try array <|> func)
    let derived = ptrs ++ arrayOrFunction
    case direct' of
      (Just (Single s)) -> return $ Named s derived
      (Just (Parenthesized (Named s decls))) -> return $ Named s (decls ++ derived)
      -- is this even possible?
      (Just (Parenthesized (Abstract decls))) -> return $ Abstract (decls ++ derived)
      Nothing -> return $ Abstract derived