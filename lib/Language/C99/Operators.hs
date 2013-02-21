module Language.C99.Operators where
  
  import Language.C99.AST
  import Text.Parsec.ByteString
  import Text.Parsec.Expr (Assoc)
  
  
  data GenOperator p
    = Infix   { infixParser   :: p (CExpr -> CExpr -> CExpr), precedence :: Int, associativity :: Assoc }
    | Prefix  { prefixParser  :: p (CExpr -> CExpr),          precedence :: Int}
    | Postfix { postfixParser :: p (CExpr -> CExpr),          precedence :: Int}
  
  
  
  