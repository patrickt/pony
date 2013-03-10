module Language.C99.Declarations
  ( declarations
  , sizedDeclaration
  , typeName
  , parameter
  , declarator 
  , functionSignature
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
  import Language.C99.Syntax
  import Language.C99.Specifiers
  import Data.Functor.Fix hiding (attribute)
  import Data.List (sort, partition)
  
  
  specifierBelongsToFunction :: CSpecifier -> Bool
  specifierBelongsToFunction (SSpec CStatic) = True
  specifierBelongsToFunction (SSpec CExtern) = True
  specifierBelongsToFunction (TQual CInline) = True
  specifierBelongsToFunction _ = False
  
  type SynBuilder = CSyn -> CSyn
  
  functionSignature :: Parser (SynBuilder, CSyn)
  functionSignature = do
    (funcSpecs, returnTypeSpecs) <- partition specifierBelongsToFunction <$> sort <$> some specifier
    let returnType = typeFromSpecifiers returnTypeSpecs
    let functionModifiers = builderFromSpecifier <$> funcSpecs
    let functionBuilder = foldl (.) id functionModifiers
    return (functionBuilder, returnType)
  
  builderFromDerived :: CDerivedDeclarator -> SynBuilder -> SynBuilder
  builderFromDerived (Pointer qs) base = foldl (flip makeModifiersFromSpec) base (TQual <$> qs) . pointer_to'
  builderFromDerived (Array qs len) base = foldl (flip makeModifiersFromSpec) base (TQual <$> qs) . array' len
  -- builderFromDerived (DerivedFunction params variadic) base = fpointer' (arguments' (convert <$> params) variadic) . base
  
  makeModifiersFromDeclarator :: CDeclarator -> SynBuilder
  makeModifiersFromDeclarator (CDeclarator pointers body modifiers asmName declAttributes) = foldBody . foldModifiers . foldPointers where
    foldPointers = foldr builderFromDerived id pointers
    foldModifiers = foldl (flip builderFromDerived) id modifiers
    foldBody = case body of
      (CParenBody d) -> makeModifiersFromDeclarator d
      _ -> id
  
  makeType :: [CSpecifier] -> CDeclarator -> CSyn
  makeType specs decl = (makeModifiersFromDeclarator decl) (typeFromSpecifiers specs)
  
  typeName :: Parser CSyn
  typeName = do
    specs <- sort <$> some specifier
    decl  <- declarator
    return $ (makeModifiersFromDeclarator decl) (typeFromSpecifiers specs)
  
  parameter :: Parser CSyn
  parameter = do
    specs <- sort <$> some specifier
    decl  <- declarator
    let type' = (makeModifiersFromDeclarator decl) (typeFromSpecifiers specs)
    let name = name' <$> declName decl
    return $ if (isJust name) then Fix (Variable {vname = (fromJust name), vtype = type', vvalue = nil'}) else type'
  
  -- read specifiers, sort them
  -- if there are typedefs, drop all of them, assert only names are declared, and record it in the symbol table
  -- if there was a typedef declaring one name, we return a Typedef
  -- if there was a typedef declaring one name, we return a Typedef with a List as its name record
  -- if there was no typedef and it declared one name, we return a Variable
  -- otherwise we return a MultiDeclaration
    
  declarations :: Parser [CSyn]
  declarations = do
    specs <- sort <$> some specifier
    decl <- L.commaSep1 initDeclarator
    
    let wrapper = if ((SSpec CTypedef) `elem` specs) then wrapTypedef else wrapDecl
    
    mapM (wrapper specs) decl
  
  wrapTypedef :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapTypedef specs (CDeclInfo { contents, initVal, ..}) = do
    when (isJust initVal) (fail "expected uninitialized declaration in typedef")
    when (isNothing (declName contents)) (fail "expected named declaration in typedef")
    let (Just name) = name' <$> declName contents
    let typ = makeType [s | s <- specs, s ≠ (SSpec CTypedef)] contents
    updateState $ addTypeDef ((liftFix getName) name) typ
    return $ Fix $ Typedef { tname = name, ttype = typ }
  
  wrapDecl :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapDecl specs (CDeclInfo { contents, initVal, ..}) = do
    let name = name' <$> declName contents
    when (isNothing name) $ fail "expected variable name"
    return $ Fix $ Variable { vname = fromJust name, vtype = makeType specs contents, vvalue = nil' }
    
  
  -- -- | C99 6.7 - abstract and concrete declarations.
  -- declaration :: Parser CDeclaration
  -- declaration = declaration' >>= checkTypedefs
  --   where declaration' = CDeclaration <$> some specifier <*> L.commaSep initDeclarator <* L.semi
  
  -- checkTypedefs :: CDeclaration -> Parser CDeclaration
  -- checkTypedefs d@(CDeclaration (SSpec CTypedef : rest) infos) = do
  --   forM infos $ \info -> do 
  --     let name = infoName info
  --     when (isNothing name) (fail "unnamed declaration passed to typedef")
  --     let name' = fromJust name
  --     updateState $ addTypeDef name' (CTypeName rest (contents info))
  --   return d
  -- checkTypedefs x = return x
  -- 
  -- | Sized declarations can only appear in the bodies of composite types.
  sizedDeclaration :: Parser CField
  sizedDeclaration = CField <$> (CDeclaration <$> some specifier
                                              <*> L.commaSep sizedDeclarator 
                                              <*  L.semi)
  
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
  array = L.brackets (Array <$> many typeQualifier <*> opt' expression)

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
    [ CIdentBody <$> (liftFix getName) <$> identifier
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
