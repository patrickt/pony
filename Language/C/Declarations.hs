module Language.C.Declarations
  ( declaration
  , typeName )
where
  
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Expressions
  import Language.C.Lexer as L hiding (identifier)
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
  pointer = char '*' >> L.whiteSpace >> many typeQualifier >>= return . Pointer

  initDeclarator :: Parser (CDeclarator, Initializer)
  initDeclarator = do
    decl <- declarator
    init <- option Uninitialized assignment
    return (decl, init)
    where assignment = L.reservedOp "=" >> initializer
  
  initializer :: Parser Initializer
  initializer = expression >>= return . InitExpression

  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    name <- optionMaybe identifier
    arrayOrFunction <- many (try array <|> func)
    let derived = ptrs ++ arrayOrFunction
    case name of
      (Just (Identifier s)) -> return $ Named s derived
      Nothing -> return $ Abstract derived