{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Reifiable.Instances 
  where
  
  import Data.Functor.Fix
  import Data.List (find, foldl', partition)
  import Data.Maybe (fromMaybe)
  import Language.C99 hiding (char, Empty)
  import Language.Pony.MachineSizes
  import qualified Language.C99.Literals as Lit
  import Semantics.C.ASG as ASG
  import Semantics.C.Reifiable
  import Data.Maybe
  
  -- CTranslationUnit -> Program.
  -- hits: CExternal -> global
  instance Reifiable CTranslationUnit where
    convert (CTranslationUnit ts) = program $ convert <$> ts
  
  -- CExternal -> Function | Typedef | FunctionProto? | Variable | Declarations
  -- hits: CFunction -> Function
  instance Reifiable CExternal where
    convert (FunctionDecl f) = convert f
    convert (ExternDecl d) 
      | declarationIsTypedef d             = convert (TdD d)
      | declarationIsComposite d
          && declarationHasFields d
          && declarationIsUnnamed d        = convert (CID d)
      | declarationIsComposite d 
          && not (declarationHasFields d)
          && declarationIsUnnamed d        = convert (FCD d)
      | declarationIsFunctionPrototype d   = convert (FPD d)
      | otherwise                          = convert (VD d)
      
  -- CBlockItem -> Variable | Expression
  -- Because in open recursion style we can mix statements and declarations freely, the BlockItem class is not useful.
  instance Reifiable CBlockItem where
    convert (BlockDeclaration decl) = convert (VD decl)
    convert (BlockStatement stmt)   = convert stmt
  
  -- CExternal -> Typedef
  -- hits: TypeDeclaration -> Type
  -- SOMEDAY: `typedef foo;` on its own is legal, declaring foo as a type for int (curse you, implicit int)
  newtype TypedefDeclaration = TdD { unTdD  :: CDeclaration }
  instance Reifiable TypedefDeclaration where
      convert (TdD decl@(CDeclaration _ [info])) = tie $ Typedef alias aliasedType
        where 
          -- first we drop the leading Typedef specifier from the declaration specifiers
          specs = declrSpecifiers $ dropTypedef decl
          -- now we extract the information about the type over which we're aliasing
          (Just declar) = contents info
          -- then convert that type along with its other specifiers
          aliasedType = convert $ DTD (specs, declar)
          -- lastly we find the name of the new typedef.
          (Just alias) = name' <$> nameOfDeclaration decl
      convert _ = error "invariant violated, please report this as a bug"

  
  -- (Specifiers x Declarator) -> Type
  newtype DerivedTypeDeclaration       = DTD { unDT  :: ([CSpecifier], CDeclarator)}
  instance Reifiable DerivedTypeDeclaration where
    convert (DTD (specs, decl)) = foldl' buildDerivedType (convert specs) (derived decl) where
      -- there are two steps in building a real type out of a declarator. 
      -- first we convert the provided specifiers to a type, then we fold the derived declarators
      -- (pointers, arrays, function declarations) around that type. 
      -- however during the fold we need to convert any specifiers that those derived declarators 
      -- may have (e.g. const or volatile pointers).
      -- FIXME: ignoring variadicity in DerivedFunction
      -- Possible bug: cdecl(1) describes "static int* foo" as "static pointer to int", but 
      -- this parses it as "pointer to static int", which makes sense from a pretty-printing POV
      -- but possibly not from a semantic point of view
      buildDerivedType :: Mu Sem -> CDerivedDeclarator -> Mu Sem
      buildDerivedType t (Pointer qs)                    = wrapQualifiers qs $ tie $ PointerToT t
      buildDerivedType t (Array qs Nothing)              = wrapQualifiers qs $ tie $ ArrayT t (tie Empty)
      buildDerivedType t (Array qs (Just size))          = wrapQualifiers qs $ tie $ ArrayT t (convert size)
      buildDerivedType t (DerivedFunction args variadic) = tie $ FunctionPointerT t (tie $ Arguments $ convert <$> args)
      wrapQualifiers :: [CTypeQualifier] -> Mu Sem -> Mu Sem
      wrapQualifiers [] t = t
      wrapQualifiers qs t = tie $ Attributed (convert <$> qs) t
  
  -- Declaration -> Variable | Group of Variables
  -- hits: TypeDeclaration -> Type, CInitializer -> List
  newtype VariableDeclaration = VD  { unVD  :: CDeclaration }
  instance Reifiable VariableDeclaration where
    -- if there's just one DeclInfo -- i.e. a declaration like `int foo;` we return a Variable
    convert (VD (CDeclaration specs [CDeclInfo { contents = Just decl, initVal, .. }])) 
      = tie $ Variable vartype varname initial where 
          (Just varname) = name' <$> declName decl
          vartype        = convert (DTD (specs, decl))
          initial        = convert initVal
    -- if there are more infos, e.g. statements of the form `int foo, bar, *baz;`
    -- then we loop around and convert each of them to Variables and stick them in a Group
    convert (VD (CDeclaration specs infos)) = tie $ Group $ [ convert (VD (CDeclaration specs [i])) | i <- infos ]  
  
  -- FIXME: ignoring variadicity here too
  -- CFunction -> Function
  -- hits: CFunction -> Type, FunctionArgs -> Declarations, BlockItem -> Group
  instance Reifiable CFunction where
    convert f@(CFunction v decl (CompoundStmt body)) = tie $ Function ftype fname fargs fbody
      where ftype        = convert  $  FT f
            (Just fname) = name'   <$> declName decl 
            fargs        = convert  $  FA decl
            fbody        = tie      $ Group $ convert <$> body
    convert other = error $ "converting function " ++ show other ++ " failed: invariants not respected"
    
  -- CFunction -> Type
  -- hits: derived type declaration -> type
  newtype FunctionType = FT { unFT :: CFunction }
  instance Reifiable FunctionType where
    -- We have to drop the first derived declarator from `decl` here because function declarators
    -- are parsed as function pointers (makes sense, really) but we don't want to consider them as such here.
    convert (FT (CFunction specs decl _)) = convert $ DTD (relevantSpecs, prunedDecl) where
      relevantSpecs = filter (not . specifierBelongsToFunction) specs
      prunedDecl = decl { derived = tail $ derived decl }
      
  -- CFunction -> Arguments
  -- hits: CParameter -> Variable
  newtype FunctionArgs = FA { unFA :: CDeclarator }
  instance Reifiable FunctionArgs where
    -- we're going to find one and only one (assuming the parser is right) 
    -- DerivedFunction derived declarator inside here, and it's going to have a list of CParameters. we convert those into Variables.
    convert (FA (CDeclarator _ derived _ _)) = tie $ Arguments fromArgs
      where (Just (DerivedFunction args variad)) = find isFunction derived
            isFunction (DerivedFunction _ _) = True
            isFunction _ = False
            params = convert <$> args
            fromArgs = if variad then params ++ [Fix Variadic] else params
  
  -- CTypeName -> Type
  -- hits: derived type declaration -> type
  -- CTypeNames are only hit in casts and sizeof(type) and the like. 
  instance Reifiable CTypeName where
    convert (CTypeName (CDeclaration specs [CDeclInfo { contents = Just decl, ..}])) = convert $ DTD (specs, decl)
    convert (CTypeName (CDeclaration specs _)) = convert specs
  
    -- this is pretty sus but cool also
  instance (Reifiable a) => Reifiable (Maybe a) where
    convert (Just a) = convert a
    convert _ = nil
  
  newtype FunctionPrototypeDeclaration = FPD { unFPD :: CDeclaration }
  instance Reifiable FunctionPrototypeDeclaration where
    convert (FPD d@(CDeclaration specs info)) = let 
      -- TODO: WE ARE DROPPING THINGS OFF LIKE IT'S AFTER-SCHOOL ACTIVITIES, YO 
      (functionSpecs, returnTypeSpecs) = partition specifierBelongsToFunction specs
      (Just functionName) = name' <$> nameOfDeclaration d
      (Just declarator) = contents $ head info
      params = declaratorParameters declarator
      in 
      variable (fpointerto (convert returnTypeSpecs) (tie $ Arguments $ convert <$> params)) functionName nil
  
  -- SUPER SUS
  instance Reifiable String where convert = name'
  
  instance Reifiable CInitializer where
    convert (CInitExpression e) = convert e
    convert (CInitList l) = list $ convert <$> l
  
  -- BUG: WE still need something to indicate .a=5 and whatnot
  instance Reifiable CInitializerSubfield where
    convert (CInitializerSubfield desigs initial) = convert initial
  
  -- CParameter -> Variable
  -- hits: declaration+specifiers -> type
  instance Reifiable CParameter where
    -- we know thanks to the Parameter axioms that theres only going to be one info, with 
    -- contents, but that it might not have a name. 
    convert (CParameter (CDeclaration specs [CDeclInfo { contents = (Just contents), .. }])) = 
      variable (convert (DTD (specs, contents))) n nil where n = maybe nil name' $ declName contents
    -- sometimes parameter names are just given type specifiers. spooky!
    convert (CParameter (CDeclaration specs _)) = variable nil (convert specs) nil
    

  -- Composite info declarations are of the form:
  -- (struct|union) { fields+ };
  -- i.e. they are not predeclarations of future composite types,
  -- nor do they declare any variables.
  newtype CompositeInfoDeclaration = CID  { unCID  :: CDeclaration }
  instance Reifiable CompositeInfoDeclaration where
    convert (CID (CDeclaration [TSpec (TStructOrUnion name isStruct fields _)] [])) = 
      tie $ CompositeInfo { 
          cname = (convert name)
        , ckind = (kind isStruct)
        , cfields = (group (convert <$> fields)) } 
      where 
        kind True = tie Struct
        kind False = tie Union
    convert _ = error "error in conversion, invariants not respected"
    
  
  -- We just parse it as a regular declaration than 
  instance Reifiable CField where
    convert (CField d@(CDeclaration _ infos)) 
      | isNothing $ size $ head infos = convert (VD d)
      | otherwise = tie $ Sized (convert (VD d)) (convert $ size $ head infos)
        
  newtype ForwardCompositeDeclaration = FCD { unFCD :: CDeclaration }
  instance Reifiable ForwardCompositeDeclaration where
    convert (FCD (CDeclaration [TSpec (TStructOrUnion name isStruct [] _)] [])) = 
      tie $ CompositeInfo { cname = convert name 
                          , ckind = kind isStruct
                          , cfields = nil }
      where
        kind True = tie Struct
        kind False = tie Union
  
  
  
  
  -- THE LINE OF BULLSHIT. FROM HERE ON EVERYTHING SUCKS.
  
  -- CExpr -> expression
  -- hits: CLiteral -> constant, CBuiltInExpr -> expression
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
    convert (CParen s)           = tie $ Paren (convert s)
    

    
  instance Reifiable CDeclaration where
    convert = dieInBreakpoint "BUG: C declaration has gone unclassified"
  
  instance Reifiable CStatement where
    -- convert (AsmStmt tq (Simple s)) = Asm (isJust tq) (convert s) [] [] []
    -- convert (AsmStmt tq (GCCAsm s inR outR clobber)) 
      --                         = Asm (isJust tq) (convert s) (convert <$> inR) (convert <$> outR) (convert <$> clobber) 
    convert BreakStmt            = tie Break
    convert (CaseStmt ex st)     = tie $ Case (convert ex) [convert st]
    convert (CompoundStmt bis)   = tie $ Compound (convert <$> bis)
    convert ContinueStmt         = tie Continue
    convert (DefaultStmt st)     = tie $ Default (convert st)
    convert (DoWhileStmt st e)   = tie $ DoWhile (convert st) (convert e)
    convert EmptyStmt            = tie Empty
    convert (ExpressionStmt ex)  = convert ex
    convert (ForStmt e1 e2 e3 s) = tie $ For 
      (convert e1)
      (convert e2)
      (convert e3)
      (convert s)
    convert (ForDeclStmt d e2 e3 s) = tie $ For
      (convert d)
      (convert e2)
      (convert e3)
      (convert s)
    convert (GotoStmt s)            = tie $ Goto (convert s)
    convert (IfStmt e s Nothing)    = tie $ IfThen (convert e) (convert s)
    convert (IfStmt e s (Just s2))  = tie $ IfThenElse (convert e) (convert s) (convert s2)
    convert (LabeledStmt l [] s)    = tie $ Labeled (name' l) (convert s)
    convert (LabeledStmt l attrs s) = tie $ Attributed (convert <$> attrs) $ convert (LabeledStmt l [] s)
    convert (ReturnStmt mE)         = tie $ Return (convert mE)
    convert (SwitchStmt ex st)      = tie $ Switch (convert ex) [convert st]
    convert (WhileStmt ex st)       = tie $ While (convert ex) (convert st)
  
  
  instance Reifiable CAttribute where
    convert (CAttribute a) = tie $ Custom (convert <$> a)
  
  instance Reifiable CTypeQualifier where
    convert CConst    = tie Const
    convert CRestrict = tie Restrict
    convert CVolatile = tie Volatile
    convert CInline   = tie Inline
  
  instance Reifiable CStorageSpecifier where
    convert CAuto     = tie Auto
    convert CRegister = tie Register
    convert CStatic   = tie Static
    convert CExtern   = tie Extern
    convert (CAttr c) = convert c
    convert CTypedef  = error "stray CTypedef passed to `convert`"
    
  dieInBreakpoint x = error $ "dying: " ++ x
  
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
    -- FIXME: this is so wrong
    convert [TStructOrUnion mName sou fields attrs] = tie $ CompositeInfo (tie comp) (fromMaybe nil (name' <$> mName)) (group $ convert <$> fields) where comp = if sou then Struct else Union
    convert [TEnumeration n a attrs]        = tie $ Enumeration (fromMaybe nil $ name' <$> n) (convert <$> a)
    convert [TTypedef n d]                  = tie $ Typedef (name' n) (convert d)
    convert [TBuiltin s]                    = tie $ BuiltinT $ name' s
    convert other                           = dieInBreakpoint $ show other
  
  instance Reifiable CEnumerator where
    convert (EnumIdent s) = variable nil (name' s) nil
    convert (EnumAssign s v) = variable nil (name' s) (convert v)
  
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
      _ -> tie $ Attributed (quals ++ specs) baseT
      where 
        baseT    = convert a
        specs    = convert <$> b
        quals    = convert <$> c
        (a, b, c) = partitionSpecifiers them
  
  
  instance Reifiable CBuiltinExpr where
    convert (BuiltinVaArg ex ty) = tie $ VaArg (convert ex) (convert ty)
  
