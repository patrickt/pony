module Language.C99.Declarations
  ( declarations
  , sizedDeclarations
  , typeName
  , parameter
  , declarator 
  , designator
  , functionSignature
  , builderFromSpecifier
  )
where
  
  -- Lasciate ogne speranza, voi ch'intrate.
  
  import Control.Arrow
  import Language.Pony.Overture
  import Language.C99.AST 
  import Language.C99.Expressions
  import qualified Language.C99.Lexer as L
  import Language.C99.Parser
  import Language.C99.Syntax
  import Language.C99.Specifiers
  import qualified Data.Foldable as F
  import Data.Functor.Fix
  import Data.List (sort, partition)
  
  -- | Parses the four relevant parts of a function signature:
  -- |  a) the attributes that apply to the function (@extern@, @static@, @inline@ and their permutations)
  -- |  b) the return type of the function
  -- |  c) the name
  -- |  d) the arguments
  functionSignature :: Parser (SynBuilder, CSyn, CSyn, CSyn)
  functionSignature = do
    (funcSpecs, returnTypeSpecs) <- partition specifierBelongsToFunction <$> sort <$> some specifier
    decl <- declarator 
    name <- maybe (fail "expected function name") (pure . name') (declName decl)
    
    args <- case (modifiers decl) of
      [DerivedFunction args variadic] -> pure $ arguments' args variadic
      _ -> fail $ "unexpected declarator body" ++ show decl
    
    let functionBuilder = foldSpecifiers funcSpecs
    let returnType = makeType funcSpecs decl
    
    return (functionBuilder, returnType, name, args)
  
  -- | Parses a type name. Used in builtin expressions and in casts.
  typeName :: Parser CSyn
  typeName = makeType <$> (sort <$> some specifier) <*> declarator
  
  -- | Parses a parameter in function declarations. If the parameter is unnamed a 
  -- | type node will be returned, otherwise a Variable will be returned.
  parameter :: Parser CSyn
  parameter = Fix <$> do
    specs <- sort <$> some specifier
    decl  <- declarator
    let type' = makeType specs decl
    let name = name' <$> declName decl
    return $ 
      if (isJust name) 
        then Variable {name = fromJust name, typ = type', value = nil'} 
        else unFix type'
  
  -- | Parses a semicolon-terminated series of declarations.
  declarations :: Parser [CSyn]
  declarations = declarations' False
  
  -- | Parses a semicolon-terminated series of possibly-sized declarations
  sizedDeclarations :: Parser [CSyn]
  sizedDeclarations = declarations' True

  declarations' :: Bool -> Parser [CSyn]
  declarations' allowsSize = do
    specs <- sort <$> some specifier
    let declParser = if allowsSize then sizedDeclarator else initDeclarator
    decls <- L.commaSep1 declParser <* L.semi
    
    let wrapper = if ((SSpec CTypedef) `elem` specs) then wrapTypedef else wrapDecl
    mapM (wrapper specs) decls
  
  
  -- Record type that wraps the various fields a declaration may have.
  data CDeclInfo = CDeclInfo {
    contents :: CDeclarator,
    initVal :: CSyn,
    size :: CSyn
  } deriving (Show, Eq)
  
  -- As a GNU extension, the user can specify the assembly name for a C function 
  -- or variable.
  type CAsmName = Maybe String
    
  declName :: CDeclarator -> Maybe String
  declName d = case (declBody d) of
    (CIdentBody s) -> Just s
    (CParenBody d) -> declName d
    CEmptyBody -> Nothing
  
  -- | C declarators, both abstract and concrete (C99 6.7.5 and 6.7.6).
  data CDeclarator 
   = CDeclarator 
     { pointers :: [CDerivedDeclarator]
     , declBody :: CDeclaratorBody
     , modifiers :: [CDerivedDeclarator]
     , asmName :: CAsmName
     , declAttributes :: [CAttribute]
   } deriving (Eq, Show)
  
  -- | Indirectly derived declarators used inside the 'CDeclarator' type.
  -- In the future, Apple's extension for blocks (declared with @^@) may be added.
  data CDerivedDeclarator
   = Pointer [CTypeQualifier]
   | Array [CTypeQualifier] CSyn
   | DerivedFunction [CSyn] Bool
   deriving (Eq, Show)
  
  type SynBuilder = CSyn -> CSyn
  
  
  convertType :: CTypeSpecifier -> CSyn
  convertType TVoid     = void'
  convertType TChar     = char'
  convertType TShort    = short' int'
  convertType TInt      = int'
  convertType TLong     = long' int'
  convertType TInt128   = verylong'
  convertType TUInt128  = unsigned' verylong'
  convertType TFloat    = float'
  convertType TDouble   = double'
  convertType TSigned   = signed' int'
  convertType TUnsigned = unsigned' int'
  convertType TBool     = bool'
  convertType (TEnumeration n members attrs) = enumeration' n (Fix (CommaGroup members))
  convertType (TStructOrUnion n kind members attrs) = composite' kind n (group' members)
  
  
  typeFromSpecifiers :: [CSpecifier] -> CSyn
  typeFromSpecifiers specs = foldSpecifiers (init specs') $ (convertType typeSpec)
    where 
      specs' = sort specs
      (TSpec typeSpec) = last specs'
  
  specifierBelongsToFunction :: CSpecifier -> Bool
  specifierBelongsToFunction (SSpec CStatic) = True
  specifierBelongsToFunction (SSpec CExtern) = True
  specifierBelongsToFunction (TQual CInline) = True
  specifierBelongsToFunction _ = False
    
    
  builderFromSpecifier :: CSpecifier -> SynBuilder
  builderFromSpecifier (TSpec TShort) = short'
  builderFromSpecifier (TSpec TLong) = long'
  builderFromSpecifier (TSpec TSigned) = signed'
  builderFromSpecifier (TSpec TUnsigned) = unsigned'
  builderFromSpecifier (TQual CConst) = const'
  builderFromSpecifier (TQual CRestrict) = restrict'
  builderFromSpecifier (TQual CVolatile) = volatile'
  builderFromSpecifier (TQual CInline) = inline'
  builderFromSpecifier (SSpec CAuto) = auto'
  builderFromSpecifier (SSpec CStatic) = static'
  builderFromSpecifier (SSpec CExtern) = extern'
  -- builderFromSpecifier (SSpec (CAttr (CAttribute es))) = attribute' (convert <$> es)
  builderFromSpecifier x = error $ show x
    
  foldSpecifiers :: [CSpecifier] -> SynBuilder
  foldSpecifiers s = concatEndo (builderFromSpecifier <$> s)
    where concatEndo m = appEndo $ mconcat (Endo <$> m)
  
  foldTypeQualifiers :: [CTypeQualifier] -> SynBuilder
  foldTypeQualifiers qs = foldSpecifiers (TQual <$> qs)
  
  builderFromDerived :: CDerivedDeclarator -> SynBuilder -> SynBuilder
  builderFromDerived (Pointer qs) base                      = pointer_to' >>> foldTypeQualifiers qs >>> base
  builderFromDerived (Array qs len) base                    = array' len  >>> foldTypeQualifiers qs >>> base
  builderFromDerived (DerivedFunction params variadic) base = functionpointer' (arguments' params variadic) >>> base
  
  builderFromDeclarator :: CDeclarator -> SynBuilder
  builderFromDeclarator (CDeclarator pointers body modifiers asmName declAttributes) 
    = foldBody . foldModifiers . foldPointers where
      -- this is the right-left rule. 
      -- first we recurse as deeply as we can into parenthesized subdeclarations.
      foldBody = case body of
        (CParenBody d) -> builderFromDeclarator d
        _ -> id
      -- then we look right to find array and function pointer modifiers.
      foldModifiers = foldDerived $ reverse modifiers
      -- then we look left to find pointers.
      foldPointers = foldDerived pointers
      -- then as the stack unwinds we jump out.
      foldDerived = foldr builderFromDerived id
  
  makeType :: [CSpecifier] -> CDeclarator -> CSyn
  makeType specs decl = builderFromDeclarator decl $ typeFromSpecifiers specs
  
  wrapTypedef :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapTypedef specs (CDeclInfo { contents, initVal, size}) = Fix <$> do
    when (isNil initVal) (fail "expected uninitialized declaration in typedef")
    when (isNothing (declName contents)) (fail "expected named declaration in typedef")
    when (isNil size) (fail "unexpected size in typedef")
    
    let (Just name) = name' <$> declName contents
    let typ = makeType [s | s <- specs, s ≠ (SSpec CTypedef)] contents
    
    updateState $ addTypeDef ((liftFix getName) name) typ
    
    return $ Typedef { name = name, typ = typ }
    
  wrapDecl :: [CSpecifier] -> CDeclInfo -> Parser CSyn
  wrapDecl specs (CDeclInfo { contents, initVal, size}) = Fix <$> do
    let sizeWrapper = if isNil size then id else sized' size
    let name = name' <$> declName contents
    let typ = makeType specs contents
    if isJust name
      then return $ Variable { name = fromJust name, typ = sizeWrapper typ, value = initVal }
      else case unFix typ of
        (Enumeration _ _) -> return $ ForwardDeclaration typ
        (Composite _ _ _) -> return $ ForwardDeclaration typ
        _ -> fail ("unexpected unnamed variable of type " ++ show typ)
        
    
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
                             <*> opt' assignment 
                             <*> pure nil'
    where assignment = L.reservedOp "=" >> initializer
    
  sizedDeclarator :: Parser CDeclInfo
  sizedDeclarator = CDeclInfo <$> declarator
                              <*> pure nil'
                              <*> opt'(L.colon *> constantExpression)
                                  
  designator :: Parser CSyn
  designator = choice 
    [ dotAccess
    , indexAccess
    ]
    
  dotAccess :: Parser CSyn
  dotAccess = do
    let access'' b c a = access' a b c
    dots <- some $ access'' <$> (name' <$> L.dot) <*> identifier
    let folded = appEndo $ F.foldMap Endo $ reverse dots
    return $ folded nil'
    
  indexAccess :: Parser CSyn
  indexAccess = do 
    idxs <- some $ (index' <$$> L.brackets constantExpression)
    let folded = appEndo $ F.foldMap Endo $ reverse idxs
    return $ folded nil'
    
  initList :: Parser CSyn
  initList = Fix <$> CommaGroup <$> L.braces (L.commaSep1 initSubfield)
  
  initSubfield :: Parser CSyn
  initSubfield = do 
    -- first we look for the designators: .x for membership. don't think an array designator could get in here.
    desigs <- optional (designator <* L.reservedOp "=")
    -- if there are designators, e.g. .x, we require an explicit = statement to make an assignment.
    exp <- initializer
    return $ 
      if (isJust desigs)
        then binary' (fromJust desigs) "=" exp
        else exp
  
  initializer :: Parser CSyn
  initializer = expression <|> initList

  asm :: Parser String
  asm = L.reserved "__asm" *> L.parens (some $ noneOf ")")
  
  data CDeclaratorBody 
    = CIdentBody String
    | CParenBody CDeclarator
    | CEmptyBody
    deriving (Show, Eq)
  
  declaratorBody :: Parser CDeclaratorBody
  declaratorBody = choice
    [ CIdentBody <$> (liftFix getName) <$> identifier
    , CParenBody <$> L.parens declarator
    , pure CEmptyBody
    ]
  
  -- Declarators (C99 6.7.5). May be named or unnamed.
  declarator :: Parser CDeclarator
  declarator = CDeclarator
    <$> many pointer
    <*> declaratorBody
    <*> many (try array <|> try func)
    <*> optional (try asm)
    <*> many attribute
