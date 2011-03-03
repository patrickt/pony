{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
  TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Conversions where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Miscellany
  import Semantics.C.Nodes
  import Data.Maybe
  import Data.List (find)
  
  class Syntax abstract semantic | abstract -> semantic where
    convert :: abstract -> semantic
    
  instance Syntax CTranslationUnit Program where
    convert = concatMap convert' where
      convert' :: CExternal -> [SGlobal]
      convert' (FunctionDecl f) = GFunction <$> [convert f]
      -- TODO: For the love of god, turn this into pattern matching
      convert' (ExternDecl d) 
        | declarationIsTypedef d = [GTypedef (fromJust $ nameOfDeclaration d) (fromJust $ convertDeclarationToType $ dropTypedef d)]
        | declarationIsComposite d = [GComposite $ convertDeclarationToCompositeInfo d]
        | otherwise = GVariable <$> convertDeclarationToVariables d
  
  instance Syntax CFunction SFunction where
    convert f@(CFunction spec decl (CompoundStmt body)) =
      let ftype = returnTypeOfFunction f
          name = fromJust $ nameOfDeclarator decl
          args = convertFunctionArguments decl
          newBody = body >>= convert
          in SFunction ftype name args newBody (isFunctionVariadic f)
  
  instance Syntax BlockItem FunctionBody where
    convert (Left declaration) = LDeclaration <$> convertDeclarationToVariables declaration
    convert (Right statement) = LStatement <$> [convert statement]
  
  instance Syntax CStatement Statement where
    convert BreakStmt = Break
    convert (CaseStmt ex st) = Case (convertExpression ex) (convert st)
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
      Nothing -> IfThen (convertExpression e) (convert s)
    convert (LabeledStmt l attrs s) = Labeled l (convertAttribute <$> attrs) (convert s)
    convert (ReturnStmt mE) = Return (convert <$> mE)
    convert (SwitchStmt ex st) = Switch (convert ex) (convert st)
    convert (WhileStmt ex st) = While (convert ex) (convert st)
  
  convertExpressionToLocal :: CExpr -> SLocal
  convertExpressionToLocal e = LStatement $ ExpressionS $ convert e
  
  convertDeclarationToLocal :: CDeclaration -> Maybe SLocal
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
  
  instance Syntax CExpr Expression where
    convert (Constant l) = Literal l
    convert (Identifier i) = Ident i
    convert (Index l r) = Brackets (convert l) (convert r)
    convert (Call f args) = FunctionCall (convert f) (convert <$> args)
    convert (Cast tn arg) = SCast (convert tn) (convert arg)
    convert (UnaryOp n arg) = Unary n (convert arg)
    convert (BinaryOp n lhs rhs) = Binary (convert lhs) n (convert rhs)
    convert (TernaryOp a b c) = Ternary (convert a) (convert b) (convert c)
    convert (SizeOfType decl) = SizeOfSType (convert decl)
    convert (CBuiltin t) = Builtin t
  
  convertExpression = convert
  
  convertAttribute :: CAttribute -> Attribute
  convertAttribute = error "convertAttribute = undefined"
  
  instance Syntax StorageSpecifier Attribute where
    convert SAuto = Auto
    convert SRegister = Register
    convert SStatic   = Static
    convert SExtern   = Extern
    
  instance Syntax TypeQualifier Attribute where
    convert QConst    = Const
    convert QRestrict = Restrict
    convert QVolatile = Volatile
  
  
  -- TODO: remember to put in the array size when I have a handle on expressions
  convertDerivedDeclarators :: DerivedDeclarator -> SType -> SType
  convertDerivedDeclarators (Pointer qs) t = SPointerTo t (map convert qs)
  convertDerivedDeclarators (Array qs size) t = SArray t Nothing (map convert qs)
  convertDerivedDeclarators (Function args variadic) t = SFunctionPointer t [] []
    
  instance Syntax CTypeName SType where
    convert (CTypeName (CDeclaration specs [DeclInfo { contents = Just decl, ..}])) = convertComponents specs decl
  
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (CDeclaration specs [info]) = Just (convertComponents specs (fromJust $ contents info))
  convertDeclarationToType _ = Nothing
  
  -- TODO: Handle initializer lists here.
  convertDeclarationToVariable :: CDeclaration -> Maybe SVariable
  convertDeclarationToVariable (CDeclaration specs [DeclInfo { contents = Just decl
                                                             , initVal = Just (InitExpression e)
                                                             , size = Nothing}]) = Just (Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) (Just (convertExpression e)))
  convertDeclarationToVariable (CDeclaration specs [DeclInfo { contents = Just decl
                                                             , initVal = Nothing
                                                             , size = Nothing }]) = Just (Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) Nothing)
  convertDeclarationToVariable _ = Nothing
  
  convertDeclarationToVariables :: CDeclaration -> [SVariable]
  convertDeclarationToVariables (CDeclaration specs infos) = map convert infos where
    convert (DeclInfo {contents = Just decl, initVal, size = Just size}) = Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) (Just (convertExpression size))
    convert (DeclInfo {contents = Just decl, initVal = Nothing, size = Nothing}) = Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) Nothing
  
  convertDeclarationToCompositeInfo :: CDeclaration -> CompositeInfo
  convertDeclarationToCompositeInfo (CDeclaration [TSpec (TStructOrUnion mN isStruct fields _)] _) =
    CompositeInfo isStruct mN (convertDeclarationToField <$> fields)
  
  convertFunctionArguments :: CDeclarator -> [SVariable]
  convertFunctionArguments (CDeclarator n derived asm attributes) = mapMaybe convertDeclarationToVariable args
    where (Just (Function args _)) = funcArgs
          funcArgs = find isFunction derived
          isFunction (Function _ _) = True
          isFunction _ = False
  
  -- TODO: Finish composite types, enumerations, and typedefs
  -- TOOD: Fill in the attributes for structs and enums
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  instance Syntax [TypeSpecifier] SType where
    convert [TVoid]                         = void
    convert [TChar]                         = signedChar
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
    convert [TFloat]                        = float
    convert [TDouble]                       = double
    convert [TLong, TDouble]                = longDouble
    convert [t@(TStructOrUnion _ _ _ _)]    = SComposite (convertComposite t) []
    convert [TEnumeration Nothing a _]      = SEnum (EnumerationInfo "unnamed" (convertEnumeration a)) []
    convert [TEnumeration (Just n) a _]     = SEnum (EnumerationInfo n (convertEnumeration a)) []
    convert [TTypedef n d]                  = Typedef n (fromJust $ convertDeclarationToType d) []
    convert [TBuiltin s]                    = SBuiltinType s []
    convert other                           = error ("unknown type " ++ show other)
  
  -- FIXME: ignoring attributes here
  convertComposite :: TypeSpecifier -> CompositeInfo
  convertComposite (TStructOrUnion n b decls _) = CompositeInfo b n (convertDeclarationToField <$> decls)
  
  -- FIXME: this won't work if there's more than one declarator per declaration
  convertDeclarationToField :: CDeclaration -> SField
  convertDeclarationToField d@(CDeclaration _ [DeclInfo {contents=(Just decl), initVal, size}]) = SField (fromJust $ nameOfDeclarator decl) (fromJust $ convertDeclarationToType d) (convert <$> size)
  
  -- FIXME: increasing doesn't work in the case of {FOO, BAR=5, BAZ} (baz should == 6)
  convertEnumeration :: [Enumerator] -> [(Name, Expression)]
  convertEnumeration e = convert' 0 e [] where
    convert' _ [] accum = accum
    convert' i (e:es) accum = case e of
      (EnumIdent n) -> convert' (i+1) es (accum ++ [(n, intToLiteral i)])
      (EnumAssign n e) -> convert' (i+1) es (accum ++ [(n, convertExpression e)])
  
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
  
