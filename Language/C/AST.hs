module Language.C.AST where
  
  -- TODO: make everything derive Typeable and Data
  
  data CExpr
    = Constant CLiteral
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    deriving (Show, Eq)
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Show, Eq)