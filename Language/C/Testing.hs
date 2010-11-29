module Language.C.Testing where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Expressions
  import Test.QuickCheck
  
  unsafeParse :: (Show a) => a -> CExpr
  unsafeParse a = case runParser constantExpression mkInternals "" (show a) of
    (Left e) -> error "whoops"
    (Right expr) -> expr
  
  checkIntParsing :: Integer -> Bool
  checkIntParsing x = case (unsafeParse x) of
      (Constant (CInteger i)) -> (i == x)
      (UnaryOp "-" (Constant (CInteger i))) -> (x == -i)
      _ -> False