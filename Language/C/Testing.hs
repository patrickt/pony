module Language.C.Testing where
  
  import Debug.Trace
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Expressions
  import Test.QuickCheck
  
  unsafeParse :: (Show a) => a -> CExpr
  unsafeParse a = case runParser constantExpression mkInternals "" (show a) of
    (Left e) -> error "whoops"
    (Right expr) -> expr
  
  integersParse_prop :: Integer -> Bool
  integersParse_prop x = case (unsafeParse x) of
      (Constant (CInteger i)) -> (i == x)
      (UnaryOp "-" (Constant (CInteger i))) -> (x == -i)
      _ -> False
      
  floatsParse_prop :: Double -> Bool
  floatsParse_prop x = case (unsafeParse x) of
    (Constant (CFloat f)) -> (f == x)
    (UnaryOp "-" (Constant (CFloat f))) -> (x == -f)