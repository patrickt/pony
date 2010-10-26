module Language.Pony.Transformations where
  
  import Data.Generics
  import Language.C
  import Language.C.Data.Ident
  import Language.C.Syntax.AST
  import Language.FiletOFish.PureExpressions
  
  purifyCConst :: CConst -> PureExpr
  purifyCConst (CIntConst (CInteger hInt _ flags) _) = 
    CLInteger signedness unknown hInt where 
      signedness = if testFlag FlagUnsigned flags then Unsigned else Signed
      unknown = error "size of an integer constant is undefined until explicitly declared"
  purifyCConst (CCharConst (CChar hChar isWide) _) = CLChar hChar
  purifyCConst (CFloatConst (CFloat hStrFloat) _) = CLFloat $ read hStrFloat
  purifyCConst (CStrConst (CString str isWide) _) = Quote str
  
  purifyTypeSpec :: CTypeSpec -> TypeExpr
  purifyTypeSpec (CVoidType _) = TVoid
  purifyTypeSpec (CCharType _) = TChar
  purifyTypeSpec _ = error "provided type specifier has no semantic meaning"
  
  helloToGoodbye :: CString -> CString
  helloToGoodbye _ = CString "goodbye, world" False
  
  helloTransform :: GenericT
  helloTransform = mkT helloToGoodbye
  
  checkMalloc :: CStat -> CStat
  checkMalloc (CExpr (Just expr@(CAssign CAssignOp _ (CCall (CVar (Ident "malloc" _ _) _) _ _) _)) _) = 
    CIf (CUnary CNegOp expr internalNode) (CReturn (Just (CConst (CIntConst (cInteger (-1)) internalNode))) internalNode) Nothing internalNode
  checkMalloc anythingElse = anythingElse
  
  mallocTransform :: GenericT
  mallocTransform = mkT checkMalloc