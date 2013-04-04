module Language.C99.Operators where
  
  import Language.C99.Syntax
  import Text.Parsec.ByteString
  import Text.Parsec.Expr (Assoc)
  
  
  data GenOperator p
    = Infix   { infixParser   :: p (CSyn -> CSyn -> CSyn), precedence :: Int, associativity :: Assoc }
  
  
  
  