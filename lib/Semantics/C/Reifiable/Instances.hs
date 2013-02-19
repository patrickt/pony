module Semantics.C.Reifiable.Instances 
  where
  
  import Data.Functor.Fix
  import Data.List (find, foldl', partition)
  import Data.Maybe
  import Language.C99 hiding (char, Empty)
  -- TODO: Provide a sizeOfType function hooking into MachineSizes
  -- import Language.Pony.MachineSizes
  import qualified Language.C99.Literals as Lit
  import Semantics.C.ASG as ASG
  import Semantics.C.Reifiable
  
  -- CTranslationUnit -> Program.
  -- hits: CExternal -> global
  instance Reifiable CTranslationUnit where
    convert (CTranslationUnit ts) = program' $ convert <$> ts
  
  -- CExternal -> Function | Typedef | FunctionProto? | Variable | Declarations
  -- hits: CFunction -> Function
  instance Reifiable CExternal where
    convert (FunctionDecl f)               = convert f
    convert (ExternDecl d) 
      | declarationIsTypedef d             = convert (AsTypedef d)
      | declarationIsComposite d
          && declarationHasFields d
          && declarationIsUnnamed d        = convert (AsComposite d)
      | declarationIsEnum d
          && declarationHasEnumerations d  = convert (AsComposite d)
      | declarationIsComposite d 
          && not (declarationHasFields d)
          && declarationIsUnnamed d        = convert (AsForwardComposite d)
      | declarationIsFunctionPrototype d   = convert (AsPrototype d) -- function prototype
      | otherwise                          = convert (AsVariable d)  -- variable' declaration
      
  -- CBlockItem -> Variable | Expression
  -- Because in open recursion style we can mix statements and declarations freely, the BlockItem class is not useful.
  instance Reifiable CBlockItem where
    convert (BlockDeclaration decl) = convert (AsVariable decl)
    convert (BlockStatement stmt)   = convert stmt
  
  -- CExternal -> Typedef
  -- hits: TypeDeclaration -> Type
  -- SOMEDAY: `typedef foo;` on its own is legal, declaring foo as a type for int (curse you, implicit int)
  newtype TypedefDeclaration = AsTypedef { unAsTypedef  :: CDeclaration }
  instance Reifiable TypedefDeclaration where
      convert (AsTypedef decl@(CDeclaration _ [info])) = tie $ Typedef aliasedType alias
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
  newtype DerivedTypeDeclaration = DTD { unDT  :: ([CSpecifier], CDeclarator)}
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
  newtype VariableDeclaration = AsVariable { unAsVariable  :: CDeclaration }
  instance Reifiable VariableDeclaration where
    -- if there's just one DeclInfo -- i.e. a declaration like `int foo;` we return a Variable
    convert (AsVariable (CDeclaration specs [CDeclInfo { contents = Just decl, initVal, .. }])) 
      = tie Variable 
        { vname  = name
        , vtype  = convert (DTD (specs, decl))
        , vvalue = convert initVal
        } where (Just name) = name' <$> declName decl
    -- if there are more infos, e.g. statements of the form `int foo, bar, *baz;`
    -- then we loop around and convert each of them to Variables and stick them in a Group
    convert (AsVariable (CDeclaration specs infos)) = tie $ Group [ convert (AsVariable (CDeclaration specs [i])) | i <- infos ]  
  
  -- CFunction -> Function
  -- hits: FunctionType -> Type, FunctionArgs -> Arguments, String -> Name, BlockItem -> Group
  -- converting a function is pretty straightforward.
  instance Reifiable CFunction where
    convert f@(CFunction v decl (CompoundStmt body)) = tie Function 
      { ftype = convert $ FT f
      , fname = name
      , fargs = convert $ FA decl
      , fbody = tie $ Group $ convert <$> body
      } where (Just name) = name' <$> declName decl
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
    -- DerivedFunction derived declarator inside here, and it's going to have a list' of CParameters. we convert those into Variables.
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
    convert (CTypeName specs (Just decl)) = convert $ DTD (specs, decl)
    convert (CTypeName specs _) = convert specs
  
    -- this is pretty sus but cool also
  instance (Reifiable a) => Reifiable (Maybe a) where
    convert (Just a) = convert a
    convert _ = nil'
  
  newtype FunctionPrototypeDeclaration = AsPrototype { unAsPrototype :: CDeclaration }
  instance Reifiable FunctionPrototypeDeclaration where
    convert (AsPrototype d@(CDeclaration specs info)) = let 
      -- first thing we do is separate the specifiers into those that belong to the function 
      -- ('extern', 'static', and 'inline') and those that don't.
      (functionSpecs, returnTypeSpecs) = partition specifierBelongsToFunction specs
      -- then we convert the specifiers that belong to the function (this is ugly)
      (_, qualifiers, storages) = partitionSpecifiers functionSpecs
      -- then we get the name of the function
      (Just functionName) = name' <$> nameOfDeclaration d
      (Just declarator) = contents $ head info
      params' = convert <$> declaratorParameters declarator
      params = 
        if declaratorContainsVariadicSpecifier declarator 
          then params' ++ [Fix Variadic]
          else params'
      attr [] x = x
      attr as d = tie $ Attributed ((convert <$> qualifiers) ++ (convert <$> storages)) d
      in 
      attr functionSpecs $ tie $ Prototype functionName (convert (DTD (returnTypeSpecs, declarator { derived = init $ derived declarator}))) (tie $ Arguments params)
  
  -- SUPER SUS
  instance Reifiable String where convert = name'
  
  instance Reifiable CInitializer where
    convert (CInitExpression e) = convert e
    convert (CInitList l) = list' $ convert <$> l
  
  -- BUG: WE still need something to indicate .a=5 and whatnot
  instance Reifiable CInitializerSubfield where
    convert (CInitializerSubfield desigs initial) = convert initial
  
  -- CParameter -> Variable
  -- hits: declaration+specifiers -> type
  instance Reifiable CParameter where
    convert (CParameter specs (Just contents)) = 
      -- if we have a name for the variable', make it a Variable,
      -- otherwise leave it as a plain type
      if hasName
        then tie Variable 
          { vname  = name' $ fromJust dname
          , vtype  = typ
          , vvalue = nil'
          }
        else typ
      where 
        hasName = isJust dname
        dname = declName contents
        typ = convert $ DTD (specs, contents)
    -- if there's no contents its an unmodified type name, so just convert that
    convert (CParameter specs _) = convert specs
    

  -- Composite info declarations are of the form:
  -- (struct|union|enum) { fields+ };
  -- i.e. they are not predeclarations of future composite types,
  -- nor do they declare any variable's.
  -- BUG: NOT CONVERTING __ATTRIBUTES__
  newtype CompositeInfoDeclaration = AsComposite  { unAsComposite  :: CDeclaration }
  instance Reifiable CompositeInfoDeclaration where
    convert (AsComposite (CDeclaration [TSpec (TStructOrUnion name isStruct fields _)] [])) = 
      tie CompositeInfo 
        { cname = convert name
        , ckind = tie $ if isStruct then Struct else Union
        , cfields = group' (convert <$> fields)
        }
    convert (AsComposite (CDeclaration [TSpec (TEnumeration name enums _)] [])) = 
      tie CompositeInfo 
        { cname = convert name
        , ckind = tie Enum
        , cfields = group' (convert <$> enums)
        }
    convert _ = error "error in conversion, invariants not respected"
    
  
  -- We just parse it as a regular declaration than 
  instance Reifiable CField where
    convert (CField d@(CDeclaration _ infos)) 
      | isNothing $ size $ head infos = convert (AsVariable d)
      | otherwise = tie $ Sized (convert (AsVariable d)) (convert $ size $ head infos)
        
  newtype ForwardCompositeDeclaration = AsForwardComposite { unFCD :: CDeclaration }
  instance Reifiable ForwardCompositeDeclaration where
    convert (AsForwardComposite (CDeclaration [TSpec (TStructOrUnion name isStruct [] _)] [])) = 
      tie CompositeInfo { cname = convert name 
                        , ckind = kind isStruct
                        , cfields = nil' }
      where
        kind True = tie Struct
        kind False = tie Union
  
  -- CExpr -> expression
  -- hits: CLiteral -> constant, CBuiltInExpr -> expression
  instance Reifiable CExpr where
    convert (Comma ls)           = tie $ CommaSep (convert <$> ls)
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
  
  -- FIXME: figure out how to represent asm nodes in the ASG
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
    convert (GotoStmt (convert -> s)) = tie $ Goto s
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
    convert [TChar]                         = char' signed'
    convert [TSigned, TChar]                = char' signed'
    convert [TUnsigned, TChar]              = char' unsigned'
    convert [TShort]                        = int' signed' [short']
    convert [TSigned, TShort]               = int' signed' [short']
    convert [TShort, TInt]                  = int' signed' [short']
    convert [TSigned, TShort, TInt]         = int' signed' [short']
    convert [TBool]                         = int' signed' [short'] 
    convert [TUnsigned, TShort]             = int' unsigned' [short']
    convert [TUnsigned, TShort, TInt]       = int' unsigned' [short']
    convert [TInt]                          = int' signed' []
    convert [TSigned]                       = int' signed' []
    convert [TSigned, TInt]                 = int' signed' []
    convert [TUnsigned]                     = int' unsigned' []
    convert [TUnsigned, TInt]               = int' unsigned' []
    convert [TLong]                         = int' signed' [long']
    convert [TSigned, TLong]                = int' signed' [long']
    convert [TLong, TInt]                   = int' signed' [long']
    convert [TSigned, TLong, TInt]          = int' signed' [long']
    convert [TUnsigned, TLong]              = int' unsigned' [long']
    convert [TLong, TUnsigned, TInt]        = int' unsigned' [long']
    convert [TUnsigned, TLong, TInt]        = int' unsigned' [long']
    convert [TLong, TLong]                  = int' signed' [long', long']
    convert [TSigned, TLong, TLong]         = int' signed' [long', long']
    convert [TLong, TLong, TInt]            = int' signed' [long', long']
    convert [TSigned, TLong, TLong, TInt]   = int' unsigned' [long', long']
    convert [TUnsigned, TLong, TLong]       = int' unsigned' [long', long']
    convert [TUnsigned, TLong, TLong, TInt] = int' unsigned' [long', long']
    convert [TInt128]                       = int' unsigned' [verylong']
    convert [TUInt128]                      = int' unsigned' [verylong']
    convert [TFloat]                        = float'
    convert [TDouble]                       = double'
    convert [TLong, TDouble]                = multipart' [long', double'] -- this is weird and doesn't match the way we do ints
    -- FIXME: this is so wrong
    convert [TStructOrUnion mName sou fields attrs] = tie $ CompositeInfo (tie comp) (fromMaybe nil' (name' <$> mName)) (group' $ convert <$> fields) where comp = if sou then Struct else Union
    convert [TEnumeration n a attrs]        = convert n
    convert [TTypedef n d]                  = tie $ Typedef (convert d) (name' n)
    convert [TBuiltin s]                    = tie $ BuiltinT $ name' s
    convert other                           = dieInBreakpoint $ show other
  
  instance Reifiable CEnumerator where
    convert (EnumIdent s) = variable' nil' (name' s) nil'
    convert (EnumAssign s v) = variable' nil' (name' s) (convert v)
  
  instance Reifiable CStringLiteral where
    convert = convert . getExpr
    
  instance Reifiable CLiteral where
    convert (Lit.CInteger i) = tie $ ASG.CInt i
    convert (Lit.CChar c)    = tie $ ASG.CChar c
    convert (Lit.CFloat f)   = tie $ ASG.CFloat $ read f
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
  
