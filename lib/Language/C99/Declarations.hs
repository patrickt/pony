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
  import Control.Arrow
  import Data.Either
  import Language.Pony.Overture
  import Language.C99.AST hiding (asmName)
  import Language.C99.Expressions
  import qualified Language.C99.Lexer as L
  import Language.C99.Parser
  import Language.C99.Syntax
  import Language.C99.Specifiers
  import Data.Functor.Fix
  import Data.List (sort, partition)
  
  type SynBuilder = CSyn -> CSyn
  
  specifierBelongsToFunction :: CSpecifier -> Bool
  specifierBelongsToFunction (SSpec CStatic) = True
  specifierBelongsToFunction (SSpec CExtern) = True
  specifierBelongsToFunction (TQual CInline) = True
  specifierBelongsToFunction _ = False
  
  functionSignature :: Parser (SynBuilder, CSyn)
  functionSignature = do
    (funcSpecs, returnTypeSpecs) <- partition specifierBelongsToFunction <$> sort <$> some specifier
    let returnType = typeFromSpecifiers returnTypeSpecs
    let functionModifiers = builderFromSpecifier <$> funcSpecs
    let functionBuilder = foldl (.) id functionModifiers
    return (functionBuilder, returnType)
  
  builderFromDerived :: CDerivedDeclarator -> SynBuilder -> SynBuilder
  builderFromDerived (Pointer qs) base   = pointer_to' >>> foldr makeModifiersFromSpec base (TQual <$> qs)
  builderFromDerived (Array qs len) base = array' len  >>> foldr makeModifiersFromSpec base (TQual <$> qs)
  builderFromDerived (DerivedFunction params variadic) base = functionpointer' (arguments' params variadic) >>> base
  
  makeModifiersFromDeclarator :: CDeclarator -> SynBuilder
  makeModifiersFromDeclarator (CDeclarator pointers body modifiers asmName declAttributes) 
    = foldBody . foldModifiers . foldPointers where
      -- this is the right-left rule. 
      -- first we find the identifier, going as deep into parenthesized declarations as possible.
      foldBody (CParenBody d) = makeModifiersFromDeclarator d
      foldBody _ = id
      -- then we look right to find ()s (function pointers) or [] (arrays)
      foldModifiers = foldDerived modifiers
      -- then we look left (with the reverse) to find any pointers
      foldPointers = foldDerived $ reverse pointers
      -- then we jump outward as needed as the stack unwinds.
      foldDerived = foldr builderFromDerived id
  
  makeType :: [CSpecifier] -> CDeclarator -> CSyn
  makeType specs decl = makeModifiersFromDeclarator decl $ typeFromSpecifiers specs
  
  typeName :: Parser CSyn
  typeName = makeType <$> (sort <$> some specifier) <*> declarator
  
  parameter :: Parser CSyn
  parameter = Fix <$> do
    specs <- sort <$> some specifier
    decl  <- declarator
    let type' = (makeModifiersFromDeclarator decl) (typeFromSpecifiers specs)
    let name = name' <$> declName decl
    return $ 
      if (isJust name) 
        then Variable {name = fromJust name, typ = type', value = nil'} 
        else unFix type'
  
  -- read specifiers, sort them
  -- if there are typedefs, drop all of them, assert only names are declared, and record it in the symbol table
  -- if there was a typedef declaring one name, we return a Typedef
  -- if there was a typedef declaring one name, we return a Typedef with a List as its name record
  -- if there was no typedef and it declared one name, we return a Variable
  -- otherwise we return a MultiDeclaration
    
  declarations :: Parser [CSyn]
  declarations = do
    specs <- sort <$> some specifier
    decls <- L.commaSep1 initDeclarator <* L.semi
    
    let wrapper = if ((SSpec CTypedef) `elem` specs) then wrapTypedef else wrapDecl
    mapM (wrapper specs) decls
  
  wrapTypedef :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapTypedef specs (CDeclInfo { contents, initVal, ..}) = Fix <$> do
    when (isJust initVal) (fail "expected uninitialized declaration in typedef")
    when (isNothing (declName contents)) (fail "expected named declaration in typedef")
    
    let (Just name) = name' <$> declName contents
    let typ = makeType [s | s <- specs, s ≠ (SSpec CTypedef)] contents
    
    updateState $ addTypeDef ((liftFix getName) name) typ
    
    return $ Typedef { name = name, typ = typ }
  
  wrapDecl :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapDecl specs (CDeclInfo { contents, initVal, ..}) = Fix <$> do
    let name = name' <$> declName contents
    when (isNothing name) $ fail "expected variable name"
    return $ Variable { name = fromJust name, typ = makeType specs contents, value = nil' }
  
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
