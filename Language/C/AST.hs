module Language.C.AST where
  
  -- TODO: make everything derive Typeable and Data
  
  data CExpr
    = Constant CLiteral
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    | Cast CExpr CExpr
    | UnaryOp String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    deriving (Show, Eq)
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Show, Eq)
  
  data TypeSpecifier
   = TVoid
   | TChar
   | TShort
   | TInt
   | TLong
   | TFloat
   | TDouble
   | TSigned
   | TUnsigned
   | TStruct String
   | TUnion String
   | TEnumeration String
  
  