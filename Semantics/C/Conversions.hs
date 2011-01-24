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
        in SFunction ftype name args newBody
  
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
    undefined
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
  
  convertExpression :: CExpr -> Expression
  convertExpression = id
  
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
  
  convertDeclarationToType :: CDeclaration -> Maybe SType
  convertDeclarationToType (CDeclaration specs [(Just declr, Nothing, Nothing)]) = Just (convertComponents specs declr)
  convertDeclarationToType _ = Nothing
  
  convertDeclarationToVariable :: CDeclaration -> Maybe SVariable
  convertDeclarationToVariable (CDeclaration specs [(Just decl, Nothing, Nothing)]) = Just (Variable (fromJust $ nameOfDeclarator decl) (convertComponents specs decl))
  convertDeclarationToVariable _ = Nothing
  
  convertDeclarationToVariables :: CDeclaration -> [SVariable]
  convertDeclarationToVariables = error "convertDeclarationToVariables = undefined"
  
  convertFunctionArguments :: CDeclarator -> [SVariable]
  convertFunctionArguments (Named n derived asm attributes) = catMaybes $ map convertDeclarationToVariable args
    where (Just (Function args _)) = funcArgs
          funcArgs = find isFunction derived
          isFunction (Function _ _) = True
          isFunction _ = False
  
  -- TODO: Finish composite types, enumerations, and typedefs
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
  convertTypeSpecifiers [(TStructOrUnion _ _ _ _)]      = SComposite undefined []
  convertTypeSpecifiers [(TEnumeration _ _ _)]          = SEnum undefined []
  convertTypeSpecifiers [(TTypedef _ _)]                = undefined
  convertTypeSpecifiers other                           = error ("unknown type " ++ (show other))
  
  -- TODO: We're leaving storage specifiers out here, those should be included too.
  convertComponents :: [Specifier] -> CDeclarator -> SType
  convertComponents specs decl = foldr convertDerivedDeclarators (setAttributes (convertTypeSpecifiers typeSpecs) (storageAttrs ++ qualAttrs)) (derivedPartsOfDeclarator decl) where
    storageAttrs = convertStorageSpecifiers <$> storageSpecs
    qualAttrs = convertTypeQualifiers <$> typeQuals
    (typeSpecs, typeQuals, storageSpecs) = partitionSpecifiers specs
    
  -- This is an easy conversion; all that is necessary is to drop the last
  -- item in the list of derived declarators.
  returnTypeOfFunction :: CFunction -> SType
  returnTypeOfFunction (CFunction specs (Named n derived asm attrs) _) = convertComponents specs (Named n (init derived) asm attrs)
  
