module Language.C.Declarations
  ( declaration
  , typeName
  , declarator
  , parameter )
where
  
  import Control.Monad (when)
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
    specs <- some specifier
    -- init-declarator-list
    decls <- L.commaSep initDeclarator
    L.semi
    when (head specs == SSpec STypedef) $ do
      state <- getState
      let td = typedefs state
      case (fst (head decls)) of
        Named name _ -> putState $ addTypeDef name (specs !! 1) state
    case decls of
      [(decl, maybeExpr)] -> return $ TopLevel specs decl maybeExpr
      more -> return $ Multiple specs more
      
  parameter :: Parser CDeclaration
  parameter = pure TopLevel <*> some specifier <*> declarator <*> pure Uninitialized
  
  typeName :: Parser CDeclaration
  typeName = pure TypeName <*> some specifier <*> optional declarator
  
  func :: Parser DerivedDeclarator
  func = L.parens $ pure Function <*> L.commaSep parameter <*> option False (L.reserved "..." >> return True)

  array :: Parser DerivedDeclarator
  array = do
    (quals, expr) <- L.brackets lunacy
    return $ Array quals expr
    where lunacy = pure (,) <*> many typeQualifier <*> optional expression

  -- ISO C99 standard, section 6.7.5.
  pointer :: Parser DerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser (CDeclarator, Initializer)
  initDeclarator = pure (,) <*> declarator <*> option Uninitialized assignment
    where assignment = L.reservedOp "=" >> initializer
  
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