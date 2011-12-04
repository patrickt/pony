module Main where
  
  import Language.Pony
  import Language.C99.Literals
  
  -- bug: need to evaluate the arguments here...
  convertLogicalShift :: Expression -> Expression
  convertLogicalShift (Binary l ">>>" r) = 
    Binary (Binary l ">>" l) "&" (Binary (Unary "~" (Binary (Binary l ">>" (Binary (Binary (SizeOfSType signedInt) "<<" (Literal (CInteger 3))) "-" (Literal (CInteger 1)))) "<<" (Binary (Binary (SizeOfSType signedInt) "<<" (Literal (CInteger 3))) "-" (Literal (CInteger 1))))) ">>" (Binary l "-" (Literal (CInteger 1))))
  convertLogicalShift other = other
  
  logicalShift :: GenericT
  logicalShift = mkT convertLogicalShift
  
  main :: IO ()
  main = run $ pony {
    operators = [Bitwise ">>>"],
    transformations = [MkTrans "LogicalShift" BottomUp logicalShift ]
  }