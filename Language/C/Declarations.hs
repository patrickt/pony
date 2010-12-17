module Language.C.Declarations
  ( declaration
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
  
  -- | C99 6.7
  declaration :: Parser CDeclaration
  declaration = pure TopLevel <*> some specifier
                              <*> L.commaSep1 initDeclarator
  
  sizedDeclaration :: Parser CDeclaration
  sizedDeclaration = undefined
  
  parameter :: Parser CDeclaration
  parameter = pure Parameter <*> some specifier 
                             <*> declarator
  
  typeName :: Parser CDeclaration
  typeName = pure TypeName <*> some specifier 
                           <*> optional declarator
  
  -- BUGGY HACK: this could allow ... anywhere in the parameters.
  func :: Parser DerivedDeclarator
  func = L.parens $ do
    items <- L.commaSep (Left <$> parameter <|> Right <$> L.reservedOp "...")
    let isVariadic = if (null items) then False else either (const False) (const True) (last items);
    
    return $ Function (lefts items) isVariadic
  
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
  attributes = L.reserved "__attribute__" *> L.parens (L.parens $ many expression)

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