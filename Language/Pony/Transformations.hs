module Language.Pony.Transformations where
  
  import Data.Generics
  import Debug.Trace
  import Language.C
  import Language.C.Data.Ident
  
  helloToGoodbye :: CString -> CString
  helloToGoodbye _ = CString "goodbye, world" False
  
  mallocToXmalloc :: CStat -> CStat
  mallocToXmalloc stat@(CExpr (Just expr@(CAssign CAssignOp lvalue (CCall (CVar (Ident "malloc" huh info''') info'') b info') info)) info'''') = 
    CIf (CUnary CNegOp expr internalNode) (CReturn (Just (CConst (CIntConst (cInteger (-1)) internalNode))) internalNode) Nothing internalNode
  mallocToXmalloc it = it