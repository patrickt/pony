module Language.Pony.LogicalShift where
  
  import Language.C
  import Semantics.C.Nodes
  
  convertLogicalShift :: Expression -> Expression
  convertLogicalShift (Binary l ">>>" r) = 
    Binary (Binary l ">>" l) "&" (Binary (Unary "~" (Binary (Binary l ">>" (Binary (Binary (SizeOfSType signedInt) "<<" (Literal (CInteger 3))) "-" (Literal (CInteger 1)))) "<<" (Binary (Binary (SizeOfSType signedInt) "<<" (Literal (CInteger 3))) "-" (Literal (CInteger 1))))) ">>" (Binary l "-" (Literal (CInteger 1))))
  convertLogicalShift other = other