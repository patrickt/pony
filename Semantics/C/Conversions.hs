{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Conversions where
  
  import Language.C hiding (char)
  import Semantics.C.Nodes
  import Semantics.C.Reifiable
  import Data.Maybe
  import Data.List (find)
  
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
  
  instance Reifiable CFunction SFunction where
    convert f@(CFunction spec decl (CompoundStmt body)) =
      let ftype = returnTypeOfFunction f
          (Just name) = nameOfDeclarator decl
          args = extractFunctionArguments decl
          newBody = body >>= convert
          in SFunction [] ftype name args newBody (isFunctionVariadic f)
  
  instance Reifiable BlockItem FunctionBody where
    convert (Left declaration) = LDeclaration <$> convertDeclarationToVariables declaration
    convert (Right statement) = LStatement <$> [convert statement]
  
  instance Reifiable CStatement Statement where
    convert (AsmStmt tq (Simple s)) 
      = Asm (isJust tq) (convert s) Nothing Nothing Nothing
    convert (AsmStmt tq (GCCAsm s inR outR clobber)) 
      = Asm (isJust tq) (convert s) (convert <$> inR) (convert <$> outR) (convert <$> clobber) 
    convert BreakStmt = Break
    convert (CaseStmt ex st) = Case (convert ex) (convert st)
    convert (CompoundStmt bis) = Compound (bis >>= convert)
    convert ContinueStmt = Continue
    convert (DefaultStmt st) = Default (convert st)
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
    convert (GotoStmt s) = GoTo s
    convert (IfStmt e s mS) = case mS of
      (Just s') -> IfThenElse (convert e) (convert s) (convert s')
      Nothing -> IfThen (convert e) (convert s)
    convert (LabeledStmt l attrs s) = Labeled l (convert <$> attrs) (convert s)
    convert (ReturnStmt mE) = Return (convert <$> mE)
    convert (SwitchStmt ex st) = Switch (convert ex) (convert st)
    convert (WhileStmt ex st) = While (convert ex) (convert st)
  
  instance Reifiable CExpr Expression where
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
  
  
  instance Reifiable CAttribute Attribute where
    convert (CAttribute e) = Custom (convert <$> e)
  
  instance Reifiable StorageSpecifier Attribute where
    convert SAuto = Auto
    convert SRegister = Register
    convert SStatic   = Static
    convert SExtern   = Extern
    convert (SAttribute c) = convert c
    
  instance Reifiable TypeQualifier Attribute where
    convert QConst    = Const
    convert QRestrict = Restrict
    convert QVolatile = Volatile
    convert FInline = Inline
  
  instance Reifiable TypeSpecifier SType where
    convert x = convert [x]
    
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  instance Reifiable [TypeSpecifier] SType where
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
    convert [TTypedef n d]                  = Typedef n (convert d) []
    convert [TBuiltin s]                    = SBuiltinType s []
    convert other                           = error ("unknown type " ++ show other)
  
  instance Reifiable [Specifier] SType where
    convert specs = setAttributes typ quals where
      (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
      typ = convert typeSpecs
      quals = (convert <$> typeQuals) ++ (convert <$> storageSpecs)
  
  -- TODO: remember to put in the array size when I have a handle on expressions
  convertDerivedDeclarators :: DerivedDeclarator -> SType -> SType
  -- the qualifiers are getting lost here, this needs a fix
  convertDerivedDeclarators (Pointer qs) (SFunctionPointer t args attrs) = SFunctionPointer t args attrs
  convertDerivedDeclarators (Pointer qs) t = SPointerTo t (map convert qs)
  convertDerivedDeclarators (Array qs size) t = SArray t (convert <$> size) (map convert qs)
  convertDerivedDeclarators (Function args variadic) t = SFunctionPointer t (convert <$> args) []
  
  instance Reifiable CTypeName SType where
    convert (CTypeName (CDeclaration specs [DeclInfo { contents = Just decl, ..}])) = convertComponents specs decl
    convert (CTypeName (CDeclaration specs _)) = convert specs
  
  instance Reifiable CField [SField] where
    convert (CField (CDeclaration specs infos)) = map convert' infos where
      convert' :: DeclInfo -> SField
      convert' (DeclInfo {contents = (Just contents), size, ..}) = SField (nameOfDeclarator contents) (convertComponents specs contents) (convert <$> size) 
  
  instance Reifiable CParameter SParameter where
    convert (CParameter (CDeclaration specs [DeclInfo { contents = (Just contents), .. }])) = SParameter (nameOfDeclarator contents) (convertComponents specs contents)
    convert (CParameter (CDeclaration specs _)) = SParameter Nothing (convert specs)
    
  instance Reifiable Enumerator Enumeration where
    convert (EnumIdent s) = Enumeration s Nothing
    convert (EnumAssign s expr) = Enumeration s (Just (convert expr))
  
  -- TODO: Handle initializer lists here.
  convertDeclarationToVariable :: CDeclaration -> Maybe SVariable
  convertDeclarationToVariable (CDeclaration specs [DeclInfo { contents = Just decl
                                                             , initVal = Just (InitExpression e)
                                                             , size = Nothing}]) = 
                                                               let (Just name) = nameOfDeclarator decl
                                                               in Just (Variable name (convertComponents specs decl) (Just (convert e)))
  convertDeclarationToVariable (CDeclaration specs [DeclInfo { contents = Just decl
                                                             , initVal = Nothing
                                                             , size = Nothing }]) = 
                                                               let (Just name) = nameOfDeclarator decl
                                                               in Just (Variable name (convertComponents specs decl) Nothing)
  convertDeclarationToVariable _ = Nothing
  
  convertDeclarationToVariables :: CDeclaration -> [SVariable]
  convertDeclarationToVariables (CDeclaration specs infos) = map convert' infos where
    convert' (DeclInfo {contents = Just decl, initVal, size = (Just size)}) = let (Just name) = nameOfDeclarator decl 
                                                                             in Variable name (convertComponents specs decl) (Just (convert size))
    convert' (DeclInfo {contents = Just decl, initVal = Nothing, size = Nothing}) = let (Just name) = nameOfDeclarator decl 
                                                                             in Variable name (convertComponents specs decl) Nothing
    convert' (DeclInfo {contents = Just decl, initVal = Just (InitExpression init), size = Nothing}) = let (Just name) = nameOfDeclarator decl 
                                                                                                       in Variable name (convertComponents specs decl) (Just (convert init))
                                                                                                       
  
  convertDeclarationToCompositeInfo :: CDeclaration -> CompositeInfo
  convertDeclarationToCompositeInfo (CDeclaration [TSpec (TStructOrUnion mN isStruct fields _)] _) =
    CompositeInfo (boolToCompositeType isStruct) mN (concatMap convert fields) where
      boolToCompositeType True = Struct
      boolToCompositeType False = Union
      
  
  extractFunctionArguments :: CDeclarator -> [SParameter]
  extractFunctionArguments (CDeclarator n derived asm attributes) = map convert args
    where (Just (Function args _)) = funcArgs
          funcArgs = find isFunction derived
          isFunction (Function _ _) = True
          isFunction _ = False
  
  -- FIXME: ignoring attributes here
  convertComposite :: TypeSpecifier -> CompositeInfo
  convertComposite (TStructOrUnion n b decls _) = CompositeInfo (boolToCompositeType b) n (concatMap convert decls) where
    boolToCompositeType True = Struct
    boolToCompositeType False = Union
  
  -- FIXME: this won't work if there's more than one declarator per declaration
  convertDeclarationToField :: CDeclaration -> SField
  convertDeclarationToField d@(CDeclaration _ [DeclInfo {contents=(Just decl), initVal, size}]) = let (Just typ) = convertDeclarationToType d 
                                                                                                  in SField (nameOfDeclarator decl) typ (convert <$> size)
  -- TODO: We're leaving storage specifiers out here, those should be included too.
  convertComponents :: [Specifier] -> CDeclarator -> SType
  convertComponents specs decl = foldr convertDerivedDeclarators (setAttributes (convert typeSpecs) (storageAttrs ++ qualAttrs)) (reverse $ derivedPartsOfDeclarator decl) where
    storageAttrs = convert <$> storageSpecs
    qualAttrs = convert <$> typeQuals
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
    
  -- This is an easy conversion; all that is necessary is to drop the last
  -- item in the list of derived declarators.
  returnTypeOfFunction :: CFunction -> SType
  returnTypeOfFunction (CFunction specs (CDeclarator n derived asm attrs) _) = convertComponents specs (CDeclarator n (init derived) asm attrs)
  
  functionPrototypeFromDeclaration :: CDeclaration -> SGlobal
  functionPrototypeFromDeclaration (CDeclaration specs [DeclInfo { contents = (Just contents), ..}]) 
    = GFunctionPrototype rtype name params isVariadic where
        (Just name) = nameOfDeclarator contents
        params = extractFunctionArguments contents
        rtype = returnTypeOfFunction (CFunction specs contents undefined)
        isVariadic = doesDeclaratorContainVariadicSpecifier contents
  
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (CDeclaration specs [info]) = let (Just contents') = contents info 
                                                         in Just (convertComponents specs contents')
  convertDeclarationToType _ = Nothing
  
  convertExpressionToLocal :: CExpr -> SLocal
  convertExpressionToLocal e = LStatement $ ExpressionS $ convert e

  convertDeclarationToLocal :: CDeclaration -> Maybe SLocal
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
