{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Reifiable.Instances 
  ( Reifiable(..) )

  where
  
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
  
  instance Reifiable CStatement Statement where
    convert (AsmStmt tq (Simple s)) 
      = Asm (isJust tq) (convert s) [] [] []
    convert (AsmStmt tq (GCCAsm s inR outR clobber)) 
      = Asm (isJust tq) (convert s) (convert <$> inR) (convert <$> outR) (convert <$> clobber) 
    convert BreakStmt = Break
    convert (CaseStmt ex st) = Case (convert ex) (convert st)
    convert (CompoundStmt bis) = Compound (bis >>= convert)
    convert ContinueStmt = Continue
    convert (DefaultStmt st) = Default (convert st)
    convert (DoWhileStmt st e) = DoWhile (convert st) (convert e)
    convert EmptyStmt = EmptyS
    convert (ExpressionStmt ex) = ExpressionS (convert ex)
    convert (ForStmt e1 e2 e3 s) = For 
      (convertExpressionToLocal <$> e1)
      (convert <$> e2)
      (convert <$> e3)
      (convert s)
    convert (ForDeclStmt d e2 e3 s) = For
      (convertDeclarationToLocal d)
      (convert <$> e2)
      (convert <$> e3)
      (convert s)
    convert (GotoStmt s) = GoTo (convert s)
    convert (IfStmt e s mS) = case mS of
      (Just s') -> IfThenElse (convert e) (convert s) (convert s')
      Nothing -> IfThen (convert e) (convert s)
    convert (LabeledStmt l attrs s) = Labeled l (convert <$> attrs) (convert s)
    convert (ReturnStmt mE) = Return (convert <$> mE)
    convert (SwitchStmt ex st) = Switch (convert ex) (convert st)
    convert (WhileStmt ex st) = While (convert ex) (convert st)
  
  instance Reifiable CExpr Expression where
    convert (Comma _) = error "BUG: COMMA NOT DEFINED YET"
    convert (Constant l) = Literal l
    convert (Identifier i) = Ident i
    convert (Index l r) = Brackets (convert l) (convert r)
    convert (Call f args) = FunctionCall (convert f) (convert <$> args)
    convert (CCast tn arg) = Cast (convert tn) (convert arg)
    convert (UnaryOp n arg) = Unary n (convert arg)
    convert (BinaryOp n lhs rhs) = Binary (convert lhs) n (convert rhs)
    convert (TernaryOp a b c) = Ternary (convert a) (convert b) (convert c)
    convert (SizeOfType decl) = SizeOfSType (convert decl)
    convert (CBuiltin t) = Builtin t
  
  instance Reifiable CStringLiteral Expression where
    convert lit = CStr s where
      (Constant (CString s)) = getExpr lit
  
  instance Reifiable CAsmArgument AsmOp where
    convert (CAsmArgument x y) = AsmOp (convert x) (convert <$> y)
  
  instance Reifiable CAttribute Attribute where
    convert (CAttribute e) = Custom (convert <$> e)
  
  instance Reifiable CStorageSpecifier Attribute where
    convert CAuto     = Auto
    convert CRegister = Register
    convert CStatic   = Static
    convert CExtern   = Extern
    convert (CAttr c) = convert c
    convert CTypedef  = error "stray CTypedef passed to `convert`"
    
  instance Reifiable CTypeQualifier Attribute where
    convert CConst    = Const
    convert CRestrict = Restrict
    convert CVolatile = Volatile
    convert CInline   = Inline
  
  instance Reifiable CTypeSpecifier SType where
    convert x = convert [x]
    
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  instance Reifiable [CTypeSpecifier] SType where
    convert [TVoid]                         = void
    convert [TChar]                         = char
    convert [TSigned, TChar]                = signedChar
    convert [TUnsigned, TChar]              = unsignedChar
    convert [TShort]                        = shortSignedInt
    convert [TSigned, TShort]               = shortSignedInt
    convert [TShort, TInt]                  = shortSignedInt
    convert [TSigned, TShort, TInt]         = shortSignedInt
    convert [TBool]                         = shortSignedInt
    convert [TUnsigned, TShort]             = shortUnsignedInt
    convert [TUnsigned, TShort, TInt]       = shortUnsignedInt
    convert [TInt]                          = signedInt
    convert [TSigned]                       = signedInt
    convert [TSigned, TInt]                 = signedInt
    convert [TUnsigned]                     = unsignedInt
    convert [TUnsigned, TInt]               = unsignedInt
    convert [TLong]                         = longSignedInt
    convert [TSigned, TLong]                = longSignedInt
    convert [TLong, TInt]                   = longSignedInt
    convert [TSigned, TLong, TInt]          = longSignedInt
    convert [TUnsigned, TLong]              = longUnsignedInt
    convert [TLong, TUnsigned, TInt]        = longUnsignedInt
    convert [TUnsigned, TLong, TInt]        = longUnsignedInt
    convert [TLong, TLong]                  = longLongSignedInt
    convert [TSigned, TLong, TLong]         = longLongSignedInt
    convert [TLong, TLong, TInt]            = longLongSignedInt
    convert [TSigned, TLong, TLong, TInt]   = longLongSignedInt
    convert [TUnsigned, TLong, TLong]       = longLongSignedInt
    convert [TUnsigned, TLong, TLong, TInt] = longLongSignedInt
    convert [TInt128]                       = int128
    convert [TUInt128]                      = uint128
    convert [TFloat]                        = float
    convert [TDouble]                       = double
    convert [TLong, TDouble]                = longDouble
    convert [t@(TStructOrUnion _ _ _ as)]   = SComposite (convertComposite t) (convert <$> as)
    convert [TEnumeration n a attrs]        = SEnum (EnumerationInfo n (convert <$> a)) (convert <$> attrs)
    convert [TTypedef n d]                  = STypedef n (convert d) []
    convert [TBuiltin s]                    = SBuiltinType s []
    convert other                           = error ("unknown type " ++ show other)
  
  instance Reifiable [CSpecifier] SType where
    convert specs = setAttributes typ quals where
      (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
      typ = convert typeSpecs
      quals = (convert <$> typeQuals) ++ (convert <$> storageSpecs)
  
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
  
  convertExpressionToLocal :: CExpr -> Local
  convertExpressionToLocal e = LStatement $ ExpressionS $ convert e

  convertDeclarationToLocal :: CDeclaration -> Maybe Local
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
