module Language.Pony.Transformations where
  
  import Data.Generics
  import Language.C
  import Language.C.Data.Ident
  
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