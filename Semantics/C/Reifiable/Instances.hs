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
    
  instance Reifiable CTypeSpecifier where
    convert x = convert [x]
  
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
    -- convert [TFloat]                        = float
    -- convert [TDouble]                       = double
    -- convert [TLong, TDouble]                = longDouble
    -- convert [t@(TStructOrUnion _ _ _ as)]   = SComposite (convertComposite t) (convert <$> as)
    -- convert [TEnumeration n a attrs]        = SEnum (EnumerationInfo n (convert <$> a)) (convert <$> attrs)
    -- convert [TTypedef n d]                  = STypedef n (convert d) []
    -- convert [TBuiltin s]                    = SBuiltinType s []
    convert other                           = error ("unknown type " ++ show other)
  
  instance Reifiable CStatement where
    -- convert (AsmStmt tq (Simple s)) = Asm (isJust tq) (convert s) [] [] []
    -- convert (AsmStmt tq (GCCAsm s inR outR clobber)) 
      -- = Asm (isJust tq) (convert s) (convert <$> inR) (convert <$> outR) (convert <$> clobber) 
    convert BreakStmt = In Break
    convert (CaseStmt ex st) = In $ Case (convert ex) [convert st]
    convert (CompoundStmt bis) = In $ Compound (convert <$> bis)
    convert ContinueStmt = In Continue
    convert (DefaultStmt st) = In $ Default (convert st)
    convert (DoWhileStmt st e) = In $ DoWhile (convert st) (convert e)
    convert EmptyStmt = In Empty
    convert (ExpressionStmt ex) = convert ex
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
    convert (GotoStmt s) = In $ Goto (convert s)
    convert (IfStmt e s Nothing) = In $ IfThen (convert e) (convert s)
    convert (IfStmt e s (Just s2)) = In $ IfThenElse (convert e) (convert s) (convert s2)
    -- convert (LabeledStmt l attrs s) = Labeled l (convert <$> attrs) (convert s)
    convert (ReturnStmt mE) = In $ Return (convert <$> mE)
    convert (SwitchStmt ex st) = In $ Switch (convert ex) [convert st]
    convert (WhileStmt ex st) = In $ While (convert ex) (convert st)
  
  instance Reifiable CExpr where
    convert (Comma _)            = error "BUG: COMMA NOT DEFINED YET"
    convert (Constant l)         = convert l
    convert (Identifier i)       = name' i
    convert (Index l r)          = In $ Brackets (convert l) (convert r)
    convert (Call f args)        = In $ FunCall (convert f) (convert <$> args)
    convert (CCast tn arg)       = In $ Cast (convert tn) (convert arg)
    convert (UnaryOp n arg)      = In $ Unary (name' n) (convert arg)
    convert (BinaryOp n lhs rhs) = In $ Binary (convert lhs) (name' n) (convert rhs)
    convert (TernaryOp a b c)    = In $ Ternary (convert a) (convert b) (convert c)
    convert (SizeOfType decl)    = In $ Unary (name' "sizeof") (convert decl)
    convert (CBuiltin t)         = convert t
  
  instance Reifiable CStringLiteral where
    convert = convert . getExpr
    
  instance Reifiable CLiteral where
    convert (Lit.CInteger i) = tie $ ASG.CInt i
    convert (Lit.CChar c)    = tie $ ASG.CChar c
    convert (Lit.CFloat f)   = tie $ ASG.CFloat f
    convert (Lit.CString s)  = tie $ ASG.CStr s
    
  instance Reifiable CDeclaration
  
  instance Reifiable CBlockItem where
    convert (Left decl)  = convert decl
    convert (Right stmt) = convert stmt
  
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
    convert (CTypeName (CDeclaration specs [CDeclInfo { contents = Just decl, ..}])) = undefined -- TODO: FIGURE THIS OUT
    convert (CTypeName (CDeclaration specs _)) = convert specs
  
  
  instance Reifiable CBuiltinExpr where
    convert (BuiltinVaArg ex ty) = tie $ VaArg (convert ex) (convert ty)
  
  -- here we get very clever and define newtypes for the different parts of a function
  -- so that we don't have to define a bunch of helper functions
  newtype FunctionType = FunctionType { unFT :: CFunction }
  newtype FunctionArgs = FunctionArgs { unFA :: CDeclarator }
  
  instance Reifiable FunctionType
  instance Reifiable FunctionArgs
  
  instance Reifiable CTranslationUnit where
    convert ts = tie $ Program $ convert <$> ts
  
  instance Reifiable CExternal where
    convert (FunctionDecl f) = convert f
    convert (ExternDecl d) = undefined -- TODO: FIGURE ME OUT!
  
  -- TODO: deal with variadicity
  -- TODO: most of this is wrong
  instance Reifiable CFunction where
    convert f@(CFunction specs decl (CompoundStmt body)) = tie $ Function ftype fname fargs fbody
      where ftype        = convert (FunctionType f)
            (Just fname) = name' <$> declName decl 
            fargs        = convert (FunctionArgs decl)
            fbody        = convert <$> body
  
  {-
  
  import Language.C99 hiding (char)
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Data.Maybe
  import Data.List (find, foldl')
  
  instance Reifiable CTranslationUnit Program where
    convert = concatMap convert' where
      convert' :: CExternal -> [SGlobal]
      convert' (FunctionDecl f) = GFunction <$> [convert f]
      -- TODO: For the love of god, turn this into pattern matching
      convert' (ExternDecl d) 
        | declarationIsTypedef d = 
            let 
            (Just name) = nameOfDeclaration d
            (Just typ) = convertDeclarationToType $ dropTypedef d 
             in [GTypedef name typ]
        | declarationIsComposite d && not (declarationHasPointer d) = [GComposite $ convertDeclarationToCompositeInfo d]
        | declarationIsFunctionPrototype d = [ functionPrototypeFromDeclaration d ]
        | otherwise = GVariable <$> convertDeclarationToVariables d
  
  instance Reifiable CFunction Function where
    convert f@(CFunction specs decl (CompoundStmt body)) =
      let ftype = returnTypeOfFunction f
          fmods = functionLevelSpecifiers specs
          (Just name) = declName decl
          args = extractFunctionArguments decl
          newBody = body >>= convert
          in Function fmods ftype name args newBody (isFunctionVariadic f)
    convert _ = error "malformed function passed to `convert`"
  
  instance Reifiable CBlockItem [Local] where
    convert (Left decl) = LDeclaration <$> convertDeclarationToVariables decl
    convert (Right stmt) = LStatement <$> [convert stmt]
  
  instance Reifiable CBuiltinExpr SBuiltin where
    convert (BuiltinVaArg ex ty) = SVaArg (convert ex) (convert ty)
  
  instance Reifiable CAsmArgument AsmOp where
    convert (CAsmArgument x y) = AsmOp (convert x) (convert <$> y)
  
  instance Reifiable CTypeName SType where
    convert (CTypeName (CDeclaration specs [CDeclInfo { contents = Just decl, ..}])) = extractTypeFromComponents specs decl
    convert (CTypeName (CDeclaration specs _)) = convert specs
  
  instance Reifiable CField [Field] where
    convert (CField (CDeclaration specs infos)) = map convert' infos where
      convert' :: CDeclInfo -> Field
      convert' (CDeclInfo {contents = (Just contents), size, ..}) = Field (declName contents) (extractTypeFromComponents specs contents) (convert <$> size)
      convert' _ = error "unexpected pattern passed to CField -> Field conversion" 
  
  instance Reifiable CParameter Parameter where
    convert (CParameter (CDeclaration specs [CDeclInfo { contents = (Just contents), .. }])) = Parameter (declName contents) (extractTypeFromComponents specs contents)
    convert (CParameter (CDeclaration specs _)) = Parameter Nothing (convert specs)
    
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
  
  extractFunctionArguments :: CDeclarator -> [Parameter]
  extractFunctionArguments (CDeclarator _ derived _ _) = map convert args
    where (Just (DerivedFunction args _)) = funcArgs
          funcArgs = find isFunction derived
          isFunction (DerivedFunction _ _) = True
          isFunction _ = False
  
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
  
  extractTypeFromComponents :: [CSpecifier] -> CDeclarator -> SType
  extractTypeFromComponents specs decl = foldl' augmentType (convert specs) (removeSpuriousPointers $ derived decl)
    
  -- This is an easy conversion; all that is necessary is to drop the last
  -- item in the list of derived declarators.
  returnTypeOfFunction :: CFunction -> SType
  returnTypeOfFunction (CFunction specs (CDeclarator n derived asm attrs) _) = 
    extractTypeFromComponents relevantSpecs (CDeclarator n (init derived) asm attrs) where
      relevantSpecs = filter (not . specifierBelongsToFunction) specs
  
  functionPrototypeFromDeclaration :: CDeclaration -> SGlobal
  functionPrototypeFromDeclaration (CDeclaration specs [CDeclInfo { contents = (Just contents), ..}]) 
    = GFunctionPrototype rtype name params isVariadic where
        (Just name) = declName contents
        params = extractFunctionArguments contents
        rtype = returnTypeOfFunction (CFunction specs contents undefined)
        isVariadic = doesDeclaratorContainVariadicSpecifier contents
  functionPrototypeFromDeclaration _ = error "BUG: invalid declaration passed to functionPrototypeFromDeclaration"
  
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (CDeclaration specs [info]) = let (Just contents') = contents info 
                                                         in Just (extractTypeFromComponents specs contents')
  convertDeclarationToType _ = Nothing
  
  convertDeclarationToLocal :: CDeclaration -> Maybe Local
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
  
  -}
