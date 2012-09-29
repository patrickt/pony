{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Reifiable.Instances 
  ( Reifiable(..) )

  where
    
  -- TODO: CaseStmt is broken, needs to have a list of locals? I think?
  -- and switchstmt
  
  import Semantics.C.ASG as ASG
  import Semantics.C.Reifiable
  import Language.C99 hiding (char, Empty)
  import qualified Language.C99.Literals as Lit
  import Language.Pony.MachineSizes
  import Data.List (find)
  
  -- CTranslationUnit -> Program.
  -- hits: list of CExternals
  instance Reifiable CTranslationUnit where
    convert ts = tie $ Program $ convert <$> ts
  
  -- CExternal -> Function | Typedef | FunctionProto? | Variable | Declarations
  -- hits: CFunction -> Function
  instance Reifiable CExternal where
    convert (FunctionDecl f) = convert f
    convert (ExternDecl d) 
      | declarationIsTypedef d                                    = convert (TD d)
      | declarationIsComposite d && not (declarationHasPointer d) = convert (CD d)
      | declarationIsFunctionPrototype d                          = convert (FPD d)
      | otherwise                                                 = convert d
  
  -- here we get very clever and define newtypes for the different parts of a function
  -- so that we don't have to define a bunch of helper functions
  newtype TypedefDeclaration           = TD  { unTD  :: CDeclaration }
  newtype CompositeDeclaration         = CD  { unCD  :: CDeclaration }
  newtype FunctionPrototypeDeclaration = FPD { unFPD :: CDeclaration }
  newtype MultiVariableDeclaration     = MVD { unMVD :: CDeclaration }
  newtype DerivedTypeDeclaration       = DTD { unDT  :: ([CSpecifier], CDeclarator)}
  
  instance Reifiable TypedefDeclaration
  instance Reifiable CompositeDeclaration
  instance Reifiable FunctionPrototypeDeclaration
  instance Reifiable DerivedTypeDeclaration
  instance Reifiable CDeclaration
  
  -- TODO: deal with variadicity
  
  instance Reifiable CFunction where
    convert f@(CFunction _ decl (CompoundStmt body)) = tie $ Function ftype fname fargs fbody
      where ftype        = convert  $  FT f
            (Just fname) = name'   <$> declName decl 
            fargs        = convert  $  FA decl
            fbody        = convert <$> body
  
  -- In the old code we dropped the first derived declarator. Why did we do that?
  newtype FunctionType = FT { unFT :: CFunction }
  instance Reifiable FunctionType where
    convert (FT (CFunction specs decl _)) = convert $ DTD (relevantSpecs, decl) where
      relevantSpecs = filter (not . specifierBelongsToFunction) specs

  -- we're going to find one and only one (assuming the parser is right) 
  -- DerivedFunction derived declarator inside here, and it's going to have a list of CParameters. we convert those into Variables.
  newtype FunctionArgs = FA { unFA :: CDeclarator }
  instance Reifiable FunctionArgs where
    convert (FA (CDeclarator _ derived _ _)) = tie $ Declarations $ convert <$> args
      where (Just (DerivedFunction args _)) = find isFunction derived
            isFunction (DerivedFunction _ _) = True
            isFunction _ = False
  
  instance Reifiable CParameter where
    convert (CParameter (CDeclaration specs [CDeclInfo { contents = (Just contents), .. }])) = 
      tie $ Variable n (convert (DTD (specs, contents))) Nothing where (Just n) = name' <$> declName contents
    convert (CParameter (CDeclaration specs _)) = tie $ Variable (tie Empty) (convert specs) Nothing
  
  instance Reifiable CBlockItem where
    convert (Left decl)  = convert decl
    convert (Right stmt) = convert stmt
  
  instance Reifiable CStatement where
    -- convert (AsmStmt tq (Simple s)) = Asm (isJust tq) (convert s) [] [] []
    -- convert (AsmStmt tq (GCCAsm s inR outR clobber)) 
      --                         = Asm (isJust tq) (convert s) (convert <$> inR) (convert <$> outR) (convert <$> clobber) 
    convert BreakStmt            = In Break
    convert (CaseStmt ex st)     = In $ Case (convert ex) [convert st]
    convert (CompoundStmt bis)   = In $ Compound (convert <$> bis)
    convert ContinueStmt         = In Continue
    convert (DefaultStmt st)     = In $ Default (convert st)
    convert (DoWhileStmt st e)   = In $ DoWhile (convert st) (convert e)
    convert EmptyStmt            = In Empty
    convert (ExpressionStmt ex)  = convert ex
    convert (ForStmt e1 e2 e3 s) = In $ For 
      (convert <$> e1)
      (convert <$> e2)
      (convert <$> e3)
      (convert s)
    convert (ForDeclStmt d e2 e3 s) = In $ For
      (Just $ convert d)
      (convert <$> e2)
      (convert <$> e3)
      (convert s)
    convert (GotoStmt s)            = In $ Goto (convert s)
    convert (IfStmt e s Nothing)    = In $ IfThen (convert e) (convert s)
    convert (IfStmt e s (Just s2))  = In $ IfThenElse (convert e) (convert s) (convert s2)
    convert (LabeledStmt l [] s)    = tie $ Labeled (name' l) (convert s)
    convert (LabeledStmt l attrs s) = tie $ Attributed (convert <$> attrs) $ convert (LabeledStmt l [] s)
    convert (ReturnStmt mE)         = In $ Return (convert <$> mE)
    convert (SwitchStmt ex st)      = In $ Switch (convert ex) [convert st]
    convert (WhileStmt ex st)       = In $ While (convert ex) (convert st)
  
  
  instance Reifiable CAttribute where
    convert (CAttribute a) = tie $ Custom (convert <$> a)
  
  instance Reifiable CTypeQualifier where
    convert CConst    = In Const
    convert CRestrict = In Restrict
    convert CVolatile = In Volatile
    convert CInline   = In Inline
  
  instance Reifiable CStorageSpecifier where
    convert CAuto     = In Auto
    convert CRegister = In Register
    convert CStatic   = In Static
    convert CExtern   = In Extern
    convert (CAttr c) = convert c
    convert CTypedef  = error "stray CTypedef passed to `convert`"
  
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  instance Reifiable [CTypeSpecifier] where
    convert [TVoid]                         = void'
    convert [TChar]                         = signed' CharT
    convert [TSigned, TChar]                = signed' CharT
    convert [TUnsigned, TChar]              = unsigned' CharT
    convert [TShort]                        = int' sizeOfShort Signed
    convert [TSigned, TShort]               = int' sizeOfShort Signed
    convert [TShort, TInt]                  = int' sizeOfShort Signed
    convert [TSigned, TShort, TInt]         = int' sizeOfShort Signed
    convert [TBool]                         = int' sizeOfShort Signed -- this is probably wrong
    convert [TUnsigned, TShort]             = int' sizeOfShort Unsigned
    convert [TUnsigned, TShort, TInt]       = int' sizeOfShort Unsigned
    convert [TInt]                          = int' sizeOfInt Signed
    convert [TSigned]                       = int' sizeOfInt Signed
    convert [TSigned, TInt]                 = int' sizeOfInt Signed
    convert [TUnsigned]                     = int' sizeOfInt Unsigned
    convert [TUnsigned, TInt]               = int' sizeOfInt Unsigned
    convert [TLong]                         = int' sizeOfLong Signed
    convert [TSigned, TLong]                = int' sizeOfLong Signed
    convert [TLong, TInt]                   = int' sizeOfLong Signed
    convert [TSigned, TLong, TInt]          = int' sizeOfLong Signed
    convert [TUnsigned, TLong]              = int' sizeOfLong Unsigned
    convert [TLong, TUnsigned, TInt]        = int' sizeOfLong Unsigned
    convert [TUnsigned, TLong, TInt]        = int' sizeOfLong Unsigned
    convert [TLong, TLong]                  = int' sizeOfLongLong Signed
    convert [TSigned, TLong, TLong]         = int' sizeOfLongLong Signed
    convert [TLong, TLong, TInt]            = int' sizeOfLongLong Signed
    convert [TSigned, TLong, TLong, TInt]   = int' sizeOfLongLong Unsigned
    convert [TUnsigned, TLong, TLong]       = int' sizeOfLongLong Unsigned
    convert [TUnsigned, TLong, TLong, TInt] = int' sizeOfLongLong Unsigned
    convert [TInt128]                       = int' sizeOfInt128 Signed
    convert [TUInt128]                      = int' sizeOfInt128 Unsigned
    convert [TFloat]                        = tie FloatT
    convert [TDouble]                       = tie DoubleT
    convert [TLong, TDouble]                = tie LongDoubleT
    -- convert [t@(TStructOrUnion _ _ _ as)]   = SComposite (convertComposite t) (convert <$> as)
    -- convert [TEnumeration n a attrs]        = SEnum (EnumerationInfo n (convert <$> a)) (convert <$> attrs)
    convert [TTypedef n d]                  = tie $ Typedef (name' n) (convert d)
    -- convert [TBuiltin s]                    = SBuiltinType s []
    convert other                           = error ("unknown type " ++ show other)
  

  instance Reifiable CExpr where
    convert (Comma _)            = error "BUG: COMMA NOT DEFINED YET"
    convert (Constant l)         = convert l
    convert (Identifier i)       = name' i
    convert (Index l r)          = tie $ Brackets (convert l) (convert r)
    convert (Call f args)        = tie $ FunCall (convert f) (convert <$> args)
    convert (CCast tn arg)       = tie $ Cast (convert tn) (convert arg)
    convert (UnaryOp n arg)      = tie $ Unary (name' n) (convert arg)
    convert (BinaryOp n lhs rhs) = tie $ Binary (convert lhs) (name' n) (convert rhs)
    convert (TernaryOp a b c)    = tie $ Ternary (convert a) (convert b) (convert c)
    convert (SizeOfType decl)    = tie $ Unary (name' "sizeof") (convert decl)
    convert (CBuiltin t)         = convert t
  
  instance Reifiable CStringLiteral where
    convert = convert . getExpr
    
  instance Reifiable CLiteral where
    convert (Lit.CInteger i) = tie $ ASG.CInt i
    convert (Lit.CChar c)    = tie $ ASG.CChar c
    convert (Lit.CFloat f)   = tie $ ASG.CFloat f
    convert (Lit.CString s)  = tie $ ASG.CStr s
  
  instance Reifiable [CSpecifier] where
    convert them = case (specs, quals) of
      ([], [])  -> baseT
      otherwise -> tie $ Attributed (specs ++ quals) baseT
      where 
        baseT    = convert a
        specs    = convert <$> b
        quals    = convert <$> c
        (a, b, c) = partitionSpecifiers them
  
  instance Reifiable CTypeName where
    convert (CTypeName (CDeclaration specs [CDeclInfo { contents = Just decl, ..}])) = convert $ DTD (specs, decl)
    convert (CTypeName (CDeclaration specs _)) = convert specs
  
  
  instance Reifiable CBuiltinExpr where
    convert (BuiltinVaArg ex ty) = tie $ VaArg (convert ex) (convert ty)
  
  {-
  
  instance Reifiable CAsmArgument AsmOp where
    convert (CAsmArgument x y) = AsmOp (convert x) (convert <$> y)
  
  instance Reifiable CField [Field] where
    convert (CField (CDeclaration specs infos)) = map convert' infos where
      convert' :: CDeclInfo -> Field
      convert' (CDeclInfo {contents = (Just contents), size, ..}) = Field (declName contents) (extractTypeFromComponents specs contents) (convert <$> size)
      convert' _ = error "unexpected pattern passed to CField -> Field conversion" 
    
  instance Reifiable CEnumerator Enumeration where
    convert (EnumIdent s) = Enumeration s Nothing
    convert (EnumAssign s expr) = Enumeration s (Just (convert expr))
  
  -- TODO: Handle initializer lists here.
  convertDeclarationToVariable :: CDeclaration -> Maybe Variable
  convertDeclarationToVariable (CDeclaration specs [CDeclInfo { contents = Just decl
                                                              , initVal = Just (CInitExpression e)
                                                              , size = Nothing}]) = 
                                                                let (Just name) = declName decl
                                                                in Just (Variable name (extractTypeFromComponents specs decl) (Just (convert e)))
  convertDeclarationToVariable (CDeclaration specs [CDeclInfo { contents = Just decl
                                                              , initVal = Nothing
                                                              , size = Nothing }]) = 
                                                                let (Just name) = declName decl
                                                                in Just (Variable name (extractTypeFromComponents specs decl) Nothing)
  convertDeclarationToVariable _ = Nothing
  
  -- | A declaration can refer to multiple variables, for example:
  -- @int foo, bar, baz;@
  convertDeclarationToVariables :: CDeclaration -> [Variable]
  convertDeclarationToVariables (CDeclaration specs infos) = map convert' infos where
    convert' (CDeclInfo {contents = Just decl, initVal = Nothing, size }) 
      = let (Just name) = declName decl 
        in Variable name (extractTypeFromComponents specs decl) (convert <$> size)
    convert' (CDeclInfo {contents = Just decl, initVal = Just (CInitExpression ie), size = Nothing}) 
      = let (Just name) = declName decl 
        in Variable name (extractTypeFromComponents specs decl) (Just (convert ie))
    convert' _ = error "BUG: unexpected pattern passed to convertDeclarationToVariables."
  
  functionLevelSpecifiers :: [CSpecifier] -> [Attribute]
  functionLevelSpecifiers specs = (convert <$> sspecs) ++ (convert <$> tquals) where
    relevant = filter specifierBelongsToFunction specs
    (_, tquals, sspecs) = partitionSpecifiers relevant 
  
  convertDeclarationToCompositeInfo :: CDeclaration -> CompositeInfo
  convertDeclarationToCompositeInfo (CDeclaration [TSpec (TStructOrUnion mN isStruct fields _)] _) =
    CompositeInfo (boolToCompositeType isStruct) mN (concatMap convert fields) where
      boolToCompositeType True = Struct
      boolToCompositeType False = Union
  convertDeclarationToCompositeInfo _ = error "BUG: unexpected pattern passed to convertDeclarationToCompositeInfo"
  
  -- FIXME: ignoring attributes here
  convertComposite :: CTypeSpecifier -> CompositeInfo
  convertComposite (TStructOrUnion n b decls _) = CompositeInfo (boolToCompositeType b) n (concatMap convert decls) where
    boolToCompositeType True = Struct
    boolToCompositeType False = Union
  convertComposite _ = error "BUG: non-composite type specifier passed to convertComposite"
  
  augmentType :: SType -> CDerivedDeclarator -> SType
  augmentType t (Pointer qs) = SPointerTo t (convert <$> qs)
  augmentType t (Array qs size) = SArray t (convert <$> size) (convert <$> qs)
  augmentType t (DerivedFunction args variadic) = SFunctionPointer t (convert <$> args) []

  removeSpuriousPointers :: [CDerivedDeclarator] -> [CDerivedDeclarator]
  removeSpuriousPointers p = go p [] where
      go [] acc = acc
      go (f@(DerivedFunction _ _) : (Pointer _) : xs) acc = go xs (acc ++ [f])
      go (x:xs) acc = go xs (acc ++ [x]) 
  
  functionPrototypeFromDeclaration :: CDeclaration -> SGlobal
  functionPrototypeFromDeclaration (CDeclaration specs [CDeclInfo { contents = (Just contents), ..}]) 
    = GFunctionPrototype rtype name params isVariadic where
        (Just name) = declName contents
        params = extractFunctionArguments contents
        rtype = returnTypeOfFunction (CFunction specs contents undefined)
        isVariadic = doesDeclaratorContainVariadicSpecifier contents
  functionPrototypeFromDeclaration _ = error "BUG: invalid declaration passed to functionPrototypeFromDeclaration"
  
  convertDeclarationToLocal :: CDeclaration -> Maybe Local
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
  
  -}
