module Language.C.Declarations
  ( declaration
  , sizedDeclaration
  , typeName
  , declarator 
  )
where
  
  -- Lasciate ogne speranza, voi ch'intrate.
  
  import Control.Monad (when)
  import Data.Either
  import Language.C.AST hiding (asmName)
  import Language.C.Expressions
  import qualified Language.C.Lexer as L
  import Language.C.Parser
  import Language.C.Specifiers
  
  -- | C99 6.7 - abstract and concrete declarations.
  declaration :: Parser CDeclaration
  declaration = declaration' >>= checkTypedefs
    where declaration' = pure CDeclaration <*> some specifier <*> initDecls
          initDecls = L.commaSep initDeclarator <* L.semi
  
  
  checkTypedefs :: CDeclaration -> Parser CDeclaration
  checkTypedefs d@(CDeclaration (SSpec STypedef : rest) infos) = do
    let (Just declr) = contents $ head infos
    let (Just name) = declName declr
    updateState $ addTypeDef name (CTypeName (CDeclaration rest infos))
    return d
  checkTypedefs x = return x
  
  -- | Sized declarations can only appear in the bodies of composite types.
  sizedDeclaration :: Parser CField
  sizedDeclaration = CField <$> (pure CDeclaration <*> some specifier
                                       <*> L.commaSep sizedDeclarator <* L.semi)
  
  -- | Parameter declarations. Must have types, but may be anonymous if they appear
  -- as a forward declaration.
  parameter :: Parser CParameter
  parameter = pure decl <*> some specifier <*> declarator where
    decl s d = CParameter $ CDeclaration s [CDeclInfo {contents = Just d, initVal = Nothing, size = Nothing}]
  
  -- | Type names, used in cast operations and typedefs.
  typeName :: Parser CTypeName
  typeName = pure decl <*> some specifier <*> optional declarator where
    decl s d = CTypeName $ CDeclaration s [CDeclInfo {contents = d, initVal = Nothing, size = Nothing}]
  
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
    return $ Function params $ notNull dots
  
  -- This doesn't handle a lot of the stupid cases introduced by C99's variable-length arrays, e.g.
  -- [ static type-qualifier-list? assignment-expression ]
  -- [type-qualifier-list static assignment-expression]
  -- [type-qualifier-list? *]
  array :: Parser CDerivedDeclarator
  array = do
    (quals, expr) <- L.brackets lunacy
    return $ Array quals expr
    where lunacy = pure (,) <*> many typeQualifier <*> optional expression

  -- ISO C99 standard, section 6.7.5.
  pointer :: Parser CDerivedDeclarator
  pointer = Pointer <$> (char '*' >> L.whiteSpace >> many typeQualifier)

  initDeclarator :: Parser CDeclInfo
  initDeclarator = pure CDeclInfo <*> Just <$> declarator 
                                  <*> optional assignment 
                                  <*> pure Nothing
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser CDeclInfo
  sizedDeclarator = pure CDeclInfo <*> Just <$> declarator
                                   <*> pure Nothing
                                   <*> optional (L.colon *> expression)
                                  
  designator :: Parser CDesignator
  designator =  pure ArrayDesignator <*> L.brackets constant
            <|> pure MemberDesignator <*> (L.dot *> L.identifier)
  
  initlist :: Parser IList
  initlist = L.braces (L.commaSep1 initList') <?> "initializer list" where
    initList' = do
      desigs <- many designator
      initValue <- (L.reservedOp "=" *> initializer)
      return (desigs, initValue)
  
  initializer :: Parser CInitializer
  initializer = (CInitList <$> initlist) <|> (CInitExpression <$> expression)

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
      (Just (Parenthesized (CDeclarator n decls _ _))) -> return $ CDeclarator n (derived ++ decls) asm attrs
      Nothing -> return $ CDeclarator Nothing derived Nothing attrs