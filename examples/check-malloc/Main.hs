module Main where
  
  import Language.Pony
  
  checkMallocDeclarations :: [Local] -> [Local]
  checkMallocDeclarations = concatMap convert where
    convert d@(LDeclaration (Variable name _ (Just (FunctionCall (Ident "malloc") _)))) =
      [ d, LStatement (IfThen (Unary "!" (Ident name)) (ExpressionS (FunctionCall (Ident "abort") []))) ]
    convert x = [x]
  
  checkMallocAssignments :: Statement -> Statement
  checkMallocAssignments (ExpressionS call@(Binary left "=" (FunctionCall (Ident "malloc") args))) =
    IfThen (Unary "!" call) (ExpressionS (FunctionCall (Ident "abort") []))
  checkMallocAssignments x = x
  
  checkMalloc :: GenericT
  checkMalloc = mkT checkMallocAssignments `extT` checkMallocDeclarations
  
  main :: IO ()
  main = run $ pony { 
    transformations = [MkTrans "CheckMalloc" TopDown checkMalloc ]
  }