module Language.C.AST where
  
  import Data.Tree
  import Data.Either
  
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
    deriving (Eq)
    
  inParens x = "(" ++ x ++ ")"
  
  instance Show CExpr where
    show (Constant l) = show l
    show (Identifier s) = s
    show (Index lhs rhs) = inParens $ (show lhs) ++ "[" ++ (show rhs) ++ "]"
    show (Call func args) = inParens $ (show func) ++ "(" ++ (show args) ++ ")"
    show (Cast typ expr) = "(" ++ show typ ++ ")" ++ show expr
    show (UnaryOp str expr) = inParens $ str ++ show expr
    show (BinaryOp str lhs rhs) = inParens $ show lhs ++ " " ++ str ++ " " ++ show rhs
    show (TernaryOp a b c) = inParens $ show a ++ " ? " ++ show b ++ show " : " ++ show c 
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Eq)
    
  instance Show CLiteral where
    show (CInteger i) = show i
    show (CChar c) = show c
    show (CFloat f) = show f
    show (CString s) = s
  
  data StorageSpecifier
    = SAuto
    | SRegister
    | SStatic
    | SExtern
    | STypedef
    deriving (Eq, Show)
  -- TODO: __attribute__(()) should go in here, too
    
  data TypeQualifier
    = QConst
    | QRestrict
    | QVolatile
    deriving (Eq)
  
  instance Show TypeQualifier where
    show QConst = "const"
    show QRestrict = "restrict"
    show QVolatile = "volatile"
  
  -- Language.C lumps 'inline' in with the type qualifiers. I disapprove.
  
  data FunctionSpecifier 
    = FInline
    deriving (Eq, Show)
  
  data Specifier 
    = TSpec TypeSpecifier
    | TQual TypeQualifier
    | SSpec StorageSpecifier
    deriving (Eq, Show)
  
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
   | TStructOrUnion String
   | TEnumeration String
   | TTypedef String
   deriving (Eq, Show)
   
  data CDeclaration 
    = TopLevel [Specifier] CDeclarator (Maybe CExpr)
    deriving (Eq, Show)
    
  
  data CDeclarator
   = Named String [DerivedDeclarator]
   | Abstract [DerivedDeclarator]
   deriving (Eq, Show)
  
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] CExpr
   | FunctionPointer [CDeclarator]
   deriving (Eq, Show)
