module Language.Pony.Transformations.Predefined.CheckMalloc where
  
  import Control.Monad
  import Language.C99
  import Semantics.C.ASG
  import Data.List
  import Data.Generics
  import Data.Maybe
  import Debug.Trace
  
  checkMallocDeclarations :: [Local] -> [Local]
  checkMallocDeclarations = concatMap convert where
    convert d@(LDeclaration (Variable name _ (Just (FunctionCall (Ident "malloc") _))) li ) =
      [ d, LStatement (IfThen (Unary "!" (Ident name)) (ExpressionS (FunctionCall (Ident "abort") []))) li ]
    convert x = [x]
  
  checkMallocAssignments :: Statement -> Statement
  checkMallocAssignments (ExpressionS call@(Binary left "=" (FunctionCall (Ident "malloc") args))) =
    IfThen (Unary "!" call) (ExpressionS (FunctionCall (Ident "abort") []))
  checkMallocAssignments x = x
  
  checkMalloc :: GenericT
  checkMalloc = mkT checkMallocAssignments `extT` checkMallocDeclarations