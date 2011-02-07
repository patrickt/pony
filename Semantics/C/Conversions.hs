module Semantics.C.Conversions where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Miscellany
  import Semantics.C.Nodes
  import Data.Maybe
  import Data.List (find)
  
  convertTranslationUnit :: CTranslationUnit -> Program
  convertTranslationUnit = concatMap convert where
    convert :: CExternal -> [SGlobal]
    convert (FunctionDecl f) = GFunction <$> [convertFunction f]
    convert (ExternDecl d) = GVariable <$> convertDeclarationToVariables d
  
  convertFunction :: CFunction -> SFunction
  convertFunction f@(CFunction spec decl (CompoundStmt body)) = 
    let ftype = returnTypeOfFunction f
        name = fromJust $ nameOfDeclarator decl
        args = convertFunctionArguments decl
        newBody = concatMap convertBlockItem body
        in SFunction ftype name args newBody (isFunctionVariadic f)
  
  convertBlockItem :: BlockItem -> [SLocal]
  convertBlockItem (Left  declaration) = LDeclaration <$> convertDeclarationToVariables declaration 
  convertBlockItem (Right statement) = LStatement <$> [convertStatement statement]
  
  convertStatement :: CStatement -> Statement
  convertStatement BreakStmt            = Break
  convertStatement (CaseStmt ex st)     = Case (convertExpression ex) (convertStatement st)
  convertStatement (CompoundStmt bis)   = Compound (convertBlockItem `concatMap` bis)
  convertStatement ContinueStmt         = Continue
  convertStatement (DefaultStmt st)     = Default (convertStatement st)
  convertStatement (DoWhileStmt st ex)  = DoWhile (convertStatement st) (convertExpression ex)
  convertStatement EmptyStmt            = EmptyS
  convertStatement (ExpressionStmt ex)  = ExpressionS (convertExpression ex)
  convertStatement (ForStmt e1 e2 e3 s) = For 
    (convertExpressionToLocal <$> e1)
    (convertExpression <$> e2)
    (convertExpression <$> e3)
    (convertStatement s) 
  convertStatement (ForDeclStmt d e2 e3 s) = For
    (convertDeclarationToLocal d)
    (convertExpression <$> e2)
    (convertExpression <$> e3)
    (convertStatement s)
  convertStatement (GotoStmt s)            = GoTo s
  convertStatement (IfStmt e s mS)         = case mS of
    (Just s') -> IfThenElse (convertExpression e) (convertStatement s) (convertStatement s')
    Nothing -> IfThen (convertExpression e) (convertStatement s)
  convertStatement (LabeledStmt l attrs s) = Labeled l (convertAttribute <$> attrs) (convertStatement s)
  convertStatement (ReturnStmt mE)         = Return (convertExpression <$> mE)
  convertStatement (SwitchStmt ex st)      = Switch (convertExpression ex) (convertStatement st)
  convertStatement (WhileStmt ex st)       = While (convertExpression ex) (convertStatement st)
  
  convertExpressionToLocal :: CExpr -> SLocal
  convertExpressionToLocal e = LStatement $ ExpressionS $ convertExpression e
  
  convertDeclarationToLocal :: CDeclaration -> Maybe SLocal
  convertDeclarationToLocal d = LDeclaration <$> convertDeclarationToVariable d
  
  convertExpression :: CExpr -> Expression
  convertExpression (Constant l) = Literal l
  convertExpression (Identifier i) = Ident i
  convertExpression (Index l r) = Brackets (convertExpression l) (convertExpression r)
  convertExpression (Call f args) = FunctionCall (convertExpression f) (convertExpression <$> args)
  convertExpression (Language.C.AST.Cast decl arg) = Semantics.C.Nodes.Cast (fromJust $ convertDeclarationToType decl) (convertExpression arg)
  convertExpression (UnaryOp n arg) = Unary n (convertExpression arg)
  convertExpression (BinaryOp n lhs rhs) = Binary (convertExpression lhs) n (convertExpression rhs)
  convertExpression (TernaryOp a b c) = Ternary (convertExpression a) (convertExpression b) (convertExpression c)
  convertExpression (SizeOfType decl) = SizeOfSType (fromJust $ convertDeclarationToType decl)
  convertExpression (CBuiltin t) = Builtin t
  
  convertAttribute :: CAttribute -> Attribute
  convertAttribute = error "convertAttribute = undefined"
  
  -- TODO: deal with STypedef and SAttribute [CExpr] here
  convertStorageSpecifiers :: StorageSpecifier -> Attribute
  convertStorageSpecifiers SAuto     = Auto
  convertStorageSpecifiers SRegister = Register
  convertStorageSpecifiers SStatic   = Static
  convertStorageSpecifiers SExtern   = Extern
  
  convertTypeQualifiers :: TypeQualifier -> Attribute
  convertTypeQualifiers QConst    = Const
  convertTypeQualifiers QRestrict = Restrict
  convertTypeQualifiers QVolatile = Volatile
  
  -- TODO: remember to put in the array size when I have a handle on expressions
  convertDerivedDeclarators :: DerivedDeclarator -> SType -> SType
  convertDerivedDeclarators (Pointer qs) t = SPointerTo t (map convertTypeQualifiers qs)
  convertDerivedDeclarators (Array qs size) t = SArray t Nothing (map convertTypeQualifiers qs)
  convertDerivedDeclarators (Function args variadic) t = SFunctionPointer t (concatMap convertDeclarationToVariables args) []
  
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (CDeclaration specs [(Just declr, Nothing, Nothing)]) = Just (convertComponents specs declr)
  convertDeclarationToType _ = Nothing
  
  -- TODO: Handle initializer lists here.
  convertDeclarationToVariable :: CDeclaration -> Maybe SVariable
  convertDeclarationToVariable (CDeclaration specs [(Just decl, Just (InitExpression e), Nothing)]) = Just (Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) (Just (convertExpression e)))
  convertDeclarationToVariable (CDeclaration specs [(Just decl, Nothing, Nothing)]) = Just (Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) Nothing)
  convertDeclarationToVariable _ = Nothing
  
  convertDeclarationToVariables :: CDeclaration -> [SVariable]
  convertDeclarationToVariables (CDeclaration specs tuples) = map convert tuples where
    convert (Just decl, Just (InitExpression e), Nothing) = Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) (Just (convertExpression e))
    convert (Just decl, Nothing, Nothing) = Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl) Nothing
  
  convertFunctionArguments :: CDeclarator -> [SVariable]
  convertFunctionArguments (CDeclarator n derived asm attributes) = mapMaybe convertDeclarationToVariable args
    where (Just (Function args _)) = funcArgs
          funcArgs = find isFunction derived
          isFunction (Function _ _) = True
          isFunction _ = False
  
  -- TODO: Finish composite types, enumerations, and typedefs
  -- TOOD: Fill in the attributes for structs and enums
  -- This is where type aliases go, as defined in C99, 6.7.2.2
  convertTypeSpecifiers :: [TypeSpecifier] -> SType
  convertTypeSpecifiers [TVoid]                         = void
  convertTypeSpecifiers [TChar]                         = signedChar
  convertTypeSpecifiers [TSigned, TChar]                = signedChar
  convertTypeSpecifiers [TUnsigned, TChar]              = unsignedChar
  convertTypeSpecifiers [TShort]                        = shortSignedInt
  convertTypeSpecifiers [TSigned, TShort]               = shortSignedInt
  convertTypeSpecifiers [TShort, TInt]                  = shortSignedInt
  convertTypeSpecifiers [TSigned, TShort, TInt]         = shortSignedInt
  convertTypeSpecifiers [TBool]                         = shortSignedInt
  convertTypeSpecifiers [TUnsigned, TShort]             = shortUnsignedInt
  convertTypeSpecifiers [TUnsigned, TShort, TInt]       = shortUnsignedInt
  convertTypeSpecifiers [TInt]                          = signedInt
  convertTypeSpecifiers [TSigned]                       = signedInt
  convertTypeSpecifiers [TSigned, TInt]                 = signedInt
  convertTypeSpecifiers [TUnsigned]                     = unsignedInt
  convertTypeSpecifiers [TUnsigned, TInt]               = unsignedInt
  convertTypeSpecifiers [TLong]                         = longSignedInt
  convertTypeSpecifiers [TSigned, TLong]                = longSignedInt
  convertTypeSpecifiers [TLong, TInt]                   = longSignedInt
  convertTypeSpecifiers [TSigned, TLong, TInt]          = longSignedInt
  convertTypeSpecifiers [TUnsigned, TLong]              = longUnsignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TInt]        = longUnsignedInt
  convertTypeSpecifiers [TLong, TLong]                  = longLongSignedInt
  convertTypeSpecifiers [TSigned, TLong, TLong]         = longLongSignedInt
  convertTypeSpecifiers [TLong, TLong, TInt]            = longLongSignedInt
  convertTypeSpecifiers [TSigned, TLong, TLong, TInt]   = longLongSignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TLong]       = longLongSignedInt
  convertTypeSpecifiers [TUnsigned, TLong, TLong, TInt] = longLongSignedInt
  convertTypeSpecifiers [TFloat]                        = float
  convertTypeSpecifiers [TDouble]                       = double
  convertTypeSpecifiers [TLong, TDouble]                = longDouble
  convertTypeSpecifiers [t@(TStructOrUnion _ _ _ _)]    = SComposite (convertComposite t) []
  convertTypeSpecifiers [TEnumeration Nothing a _]      = SEnum (EnumerationInfo "unnamed" (convertEnumeration a)) []
  convertTypeSpecifiers [TEnumeration (Just n) a _]     = SEnum (EnumerationInfo n (convertEnumeration a)) []
  convertTypeSpecifiers [TTypedef _ _]                  = undefined
  convertTypeSpecifiers other                           = error ("unknown type " ++ show other)
  
  -- FIXME: ignoring attributes here
  convertComposite :: TypeSpecifier -> CompositeInfo
  convertComposite (TStructOrUnion n b decls _) = CompositeInfo b n (convertDeclarationToField <$> decls)
  
  -- FIXME: this won't work if there's more than one declarator per declaration
  convertDeclarationToField :: CDeclaration -> SField
  convertDeclarationToField = undefined
  
  -- FIXME: increasing doesn't work in the case of {FOO, BAR=5, BAZ} (baz should == 6)
  convertEnumeration :: [Enumerator] -> [(Name, Expression)]
  convertEnumeration e = convert' 0 e [] where
    convert' _ [] accum = accum
    convert' i (e:es) accum = case e of
      (EnumIdent n) -> convert' (i+1) es (accum ++ [(n, intToLiteral i)])
      (EnumAssign n e) -> convert' (i+1) es (accum ++ [(n, convertExpression e)])
  
  -- TODO: We're leaving storage specifiers out here, those should be included too.
  convertComponents :: [Specifier] -> CDeclarator -> SType
  convertComponents specs decl = foldr convertDerivedDeclarators (setAttributes (convertTypeSpecifiers typeSpecs) (storageAttrs ++ qualAttrs)) (derivedPartsOfDeclarator decl) where
    storageAttrs = convertStorageSpecifiers <$> storageSpecs
    qualAttrs = convertTypeQualifiers <$> typeQuals
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
    
  -- This is an easy conversion; all that is necessary is to drop the last
  -- item in the list of derived declarators.
  returnTypeOfFunction :: CFunction -> SType
  returnTypeOfFunction (CFunction specs (CDeclarator n derived asm attrs) _) = convertComponents specs (CDeclarator n (init derived) asm attrs)
  
