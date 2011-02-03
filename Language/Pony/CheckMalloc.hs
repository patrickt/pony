module Language.Pony.CheckMalloc where
  
  import Language.C
  import Semantics.C.Nodes
  
  checkMallocs :: Statement -> Statement
  checkMallocs (ExpressionS call@(Binary left "=" (FunctionCall (Ident "malloc") args))) =
    IfThen (Unary "!" call) (Return (Just (Literal (CInteger 1)))) 
  checkMallocs other = other