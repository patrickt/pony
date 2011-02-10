module Language.Pony.CheckMalloc where
  
  import Control.Monad
  import Language.C
  import Semantics.C.Nodes
  import Data.List
  import Data.Generics
  import Data.Generics.Zipper
  import Data.Maybe
  import Debug.Trace
  
  checkMallocDeclarations :: [SLocal] -> [SLocal]
  checkMallocDeclarations = concatMap convert where
    convert d@(LDeclaration (Variable name _ (Just (FunctionCall (Ident "malloc") _)))) =
      [ d, LStatement (IfThen (Unary "!" (Ident name)) (ExpressionS (FunctionCall (Ident "abort") []))) ]
    convert x = [x]
  
  checkMallocAssignments :: Statement -> Statement
  checkMallocAssignments (ExpressionS call@(Binary left "=" (FunctionCall (Ident "malloc") args))) =
    (IfThen (Unary "!" call) (ExpressionS (FunctionCall (Ident "abort") [])))
  checkMallocAssignments x = x
  
  checkMalloc :: GenericT
  checkMalloc = (mkT checkMallocAssignments) `extT` checkMallocDeclarations