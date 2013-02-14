module Semantics.C.Reifiable.Instances 
  where
  
  
  import Data.Functor.Fix hiding (foldl, foldr, sequence)
  import Data.List (find, foldl', partition, sort)
  import Data.Maybe
  import Language.C99 hiding (char, Empty)
  import qualified Language.C99 as C99
  -- TODO: Provide a sizeOfType function hooking into MachineSizes
  -- import Language.Pony.MachineSizes
  import qualified Language.C99.Literals as Lit
  import Semantics.C.ASG as ASG
  import Semantics.C.Reifiable
  
  builderFromSpecifier :: CSpecifier -> (Fix Sem -> Fix Sem)
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
  builderFromSpecifier x = error $ show x
  
  group'' [] = nil'
  group'' x = group' $ convert <$> x
  
  typeFromSpec :: CSpecifier -> Fix Sem
  typeFromSpec (TSpec TVoid) = void'
  typeFromSpec (TSpec TDouble) = double'
  typeFromSpec (TSpec TFloat) = float' 
  typeFromSpec (TSpec TInt) = int'
  typeFromSpec (TSpec TShort) = short' int'
  typeFromSpec (TSpec TInt128) = verylong'
  typeFromSpec (TSpec TUInt128) = unsigned' verylong'
  typeFromSpec (TSpec (TStructOrUnion name isStruct fields attrs)) 
    = tie $ Composite { ckind = if isStruct then struct' else union'
                      , cname = convert name
                      , cfields = group'' $ unCField <$> fields
                      }
  typeFromSpec (TSpec (TEnumeration name enums attrs)) 
    = tie $ Enumeration { ename = convert name
                        , emembers = group'' enums
                        }
  
  instance Reifiable CEnumerator where
    convert (EnumIdent s) = variable' nil' (name' s) nil'
    convert (EnumAssign s v) = variable' nil' (name' s) (convert v)
  
  fpointer' params = \x -> function' nil' x params nil'
  
  instance Reifiable CParameter where
    convert (CParameter specs decl) = convert (CDeclaration specs [def { contents = fromJust decl }])
  
  builderFromDerived :: CDerivedDeclarator -> (Fix Sem -> Fix Sem) -> (Fix Sem -> Fix Sem)
  builderFromDerived (Pointer qs) base = (foldl (flip makeModifiersFromSpec) base (TQual <$> qs)) . pointer_to'
  builderFromDerived (Array qs Nothing) base = (foldl (flip makeModifiersFromSpec) base (TQual <$> qs)) . (array' nil')
  builderFromDerived (DerivedFunction params variadic) base = fpointer' (arguments' (convert <$> params) variadic) . base
  
  makeModifiersFromSpec :: CSpecifier -> (Fix Sem -> Fix Sem) -> (Fix Sem -> Fix Sem)
  makeModifiersFromSpec spec base = (builderFromSpecifier spec) . base
  
  typeFromSpecifiers :: [CSpecifier] -> Fix Sem
  typeFromSpecifiers specs = (foldr (makeModifiersFromSpec) id (init specs')) (typeFromSpec $ last specs') where specs' = sort specs
  
  makeModifiersFromDeclarator :: CDeclarator -> (Fix Sem -> Fix Sem)
  makeModifiersFromDeclarator (CDeclarator { pointers, body, modifiers, .. }) = foldBody . foldModifiers . foldPointers where
    foldPointers = foldr builderFromDerived id pointers
    foldModifiers = foldl (flip builderFromDerived) id modifiers
    foldBody = case body of
      (CParenBody d) -> makeModifiersFromDeclarator d
      _ -> id
  
  -- BUG: WE still need something to indicate .a=5 and whatnot
  instance Reifiable CInitializerSubfield where
    convert (CInitializerSubfield desigs initial) = convert initial
  
  instance Reifiable CInitializer where
    convert (CInitExpression e) = convert e
    convert (CInitList l) = list' $ convert <$> l
  
    -- this is pretty sus but cool also
  instance (Reifiable a) => Reifiable (Maybe a) where
    convert = maybe nil' convert
    
  instance Reifiable String where
    convert = name'
    
  constructType :: [CSpecifier] -> CDeclarator -> Fix Sem
  constructType specs contents = (makeModifiersFromDeclarator contents) (typeFromSpecifiers specs)
  
  -- CTranslationUnit -> Program.
  -- hits: CExternal -> global
  instance Reifiable CTranslationUnit where
    convert (CTranslationUnit ts) = program' $ convert <$> ts
  
  instance Reifiable CExternal where
    convert (FunctionDecl f) = convert f
    convert (ExternDecl d)   = convert d
    
  instance Reifiable CDeclaration where
    convert decl@(CDeclaration specs [info@(CDeclInfo contents initVal size)]) 
      -- assert: initval is not present
      | isJust size = sized' (convert size) (convert $ decl { declrInfos = [info { size = Nothing }]})
      -- assert: name is present
      | declarationIsTypedef decl = tie $ Typedef { tname = convert $ declName contents, ttype = constructType (tail specs) contents }
      | declarationIsUnnamed decl && (declarationIsComposite decl || declarationIsEnum decl) = tie $ ForwardTypeDeclaration $ constructType specs contents
      | otherwise = tie $ Variable { vname = convert $ declName contents, vtype = constructType specs contents, vvalue = convert initVal }
    convert decl@(CDeclaration specs infos) = group' [ convert $ decl { declrInfos = [i]} | i <- infos ]
  
  instance Reifiable CFunction where
    convert f@(CFunction specs (decl@CDeclarator { modifiers = [(DerivedFunction args var)] }) (CompoundStmt body)) = tie Function 
      { ftype = constructType relevantSpecs (decl { modifiers = [] })
      , fname = name
      , fargs = arguments' (convert <$> args) var
      , fbody = tie $ Group $ convert <$> body
      } where 
        (Just name) = name' <$> declName decl
        relevantSpecs = filter (not . specifierBelongsToFunction) specs
    convert other = error $ "converting function " ++ show other ++ " failed: invariants not respected"
      
  
  -- CBlockItem -> Variable | Expression
  -- Because in open recursion style we can mix statements and declarations freely, the BlockItem class is not useful.
  instance Reifiable CBlockItem where
    convert (BlockDeclaration decl) = convert decl
    convert (BlockStatement stmt)   = convert stmt
    
  -- CTypeName -> Type
  -- hits: derived type declaration -> type
  -- CTypeNames are only hit in casts and sizeof(type) and the like. 
  instance Reifiable CTypeName where
    convert (CTypeName specs d) = constructType specs d
    
  foldPostfix :: FSem -> CPostfix -> FSem
  foldPostfix a (Index b) = brackets' a (convert b)
  foldPostfix a (Call fs) = funcall' a (convert <$> fs)
  foldPostfix a (MemberAccess s) = binary' a "." (name' s)
  foldPostfix a (PointerAccess s) = binary' a "->" (name' s)
  foldPostfix a PostIncrement = unary' a "++"
  foldPostfix a PostDecrement = unary' a "--"
  
  -- CExpr -> expression
  -- hits: CLiteral -> constant, CBuiltInExpr -> expression
  instance Reifiable CExpr where
    convert (Comma l r)          = comma' (convert l) (convert r)
    convert (Constant l)         = convert l
    convert (Identifier i)       = name' i
    convert (PostfixOp base args)= foldl foldPostfix (convert base) args
    convert (CCast tn arg)       = tie $ Cast (convert <$> tn) (convert arg)
    convert (UnaryOp n arg)      = tie $ Unary (name' n) (convert arg)
    convert (BinaryOp n lhs rhs) = tie $ Binary (convert lhs) (name' n) (convert rhs)
    convert (TernaryOp a b c)    = tie $ Ternary (convert a) (convert b) (convert c)
    convert (SizeOfType decl)    = tie $ Unary (name' "sizeof") (convert decl)
    convert (CBuiltin t)         = convert t
    convert (CParen s)           = tie $ Paren (convert s)
  
  instance Reifiable CLiteral where
    convert (Lit.CInteger i) = tie $ ASG.CInt i
    convert (Lit.CChar c)    = tie $ ASG.CChar c
    convert (Lit.CFloat f)   = tie $ ASG.CFloat $ read f
    convert (Lit.CString s)  = tie $ ASG.CStr s
  
  instance Reifiable CBuiltinExpr where
    convert (BuiltinVaArg ex ty) = tie $ VaArg (convert ex) (convert ty)
  
  instance Reifiable CStringLiteral where
    convert = convert . getExpr
    
  instance Reifiable CAsmArgument where
    convert (CAsmArgument l e) = tie $ AssemblyOperand (convert l) (convert e)
    
  list'' [] = nil'
  list'' a = list' $ convert <$> a
  
  -- FIXME: figure out how to represent asm nodes in the ASG
  instance Reifiable CStatement where
    convert (AsmStmt tq (Simple s)) = tie $ Assembly False (convert s) nil' nil' nil'
    convert (AsmStmt tq (GCCAsm s inR outR clobber)) = tie $ Assembly (isJust tq) (convert s) (list'' inR) (list'' outR) (list'' clobber) 
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
    -- convert (LabeledStmt l attrs s) = tie $ Custom (convert <$> attrs) $ convert (LabeledStmt l [] s)
    convert (ReturnStmt mE)         = tie $ Return (convert mE)
    convert (SwitchStmt ex st)      = tie $ Switch (convert ex) [convert st]
    convert (WhileStmt ex st)       = tie $ While (convert ex) (convert st)
  
