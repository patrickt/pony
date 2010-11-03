module Language.C.AST where
  
  -- TODO: make everything derive Typeable and Data
  
  data CExpr
    = Constant CLiteral
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Show, Eq)