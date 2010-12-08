module Language.C.Declarations
  ( declaration
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
        Named name _ _ -> putState $ addTypeDef name (specs !! 1) state
        _ -> return ()
    case decls of
      [(decl, maybeExpr)] -> return $ TopLevel specs decl maybeExpr
      more -> return $ Multiple specs more
      
  parameter :: Parser CDeclaration
  parameter = pure TopLevel <*> some specifier <*> declarator <*> pure Uninitialized
  
  typeName :: Parser CDeclaration
  typeName = pure TypeName <*> some specifier <*> optional declarator
  
  -- BUGGY HACK: this could allow ... anywhere in the parameters.
  func :: Parser DerivedDeclarator
  func = L.parens $ do
    items <- L.commaSep (Left <$> parameter <|> Right <$> L.reservedOp "...")
    let isVariadic = either (const False) (const True) (last items)
    return $ Function (lefts items) isVariadic
  
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
    
  asmName :: Parser String
  asmName = (L.symbol "__asm") >> (L.parens $ some $ noneOf ")")
    
  attributes :: Parser [CExpr]
  attributes = (L.symbol "__attribute__") *> (L.parens $ L.parens $ many expression)

  declarator :: Parser CDeclarator
  declarator = do
    ptrs <- many pointer
    direct' <- optionMaybe direct
    arrayOrFunction <- many (try array <|> func)
    asm <- optional (try asmName)
    attrs <- optional attributes
    let derived = ptrs ++ arrayOrFunction
    case direct' of
      (Just (Single s)) -> return $ Named s derived asm
      (Just (Parenthesized (Named s decls _))) -> return $ Named s (decls ++ derived) asm
      -- is this even possible?
      (Just (Parenthesized (Abstract decls))) -> return $ Abstract (decls ++ derived)
      Nothing -> return $ Abstract derived