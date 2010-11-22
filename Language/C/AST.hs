{-# LANGUAGE GADTs #-}

module Language.C.AST where
  
  import Data.Tree
  import Data.Either
  import Text.Printf
  
  -- TODO: make everything derive Typeable and Data
  
  data CStatement where
    DeclarationStmt :: CDeclaration -> CStatement -- declarations aren't statements, this is a hack
    ExpressionStmt :: CExpr -> CStatement
    EmptyStmt :: CStatement
    CaseStmt :: CExpr -> CStatement -> CStatement
    CompoundStmt :: [CStatement] -> CStatement
    DefaultStmt :: CStatement -> CStatement
    LabeledStmt :: String -> CStatement -> CStatement
    ContinueStmt :: CStatement
    BreakStmt :: CStatement
    GotoStmt :: String -> CStatement
    ReturnStmt :: Maybe CExpr -> CStatement
  
  -- this could all be a lot prettier if CStatement were an instance of PrintfArg
  instance Show CStatement where
    show (ExpressionStmt e) = show e ++ ";"
    show EmptyStmt = "()"
    show (CompoundStmt a) = "{" ++ show a ++ "}"
    show (CaseStmt expr s) = printf "case %s: %s;" (show expr) (show s)
    show (DefaultStmt expr) = "default: " ++ show expr ++ ";"
    show (LabeledStmt l s) = printf "%s: %s;" l (show s)
    show ContinueStmt = "continue;"
    show BreakStmt = "break;"
    show (GotoStmt s) = printf "goto %s;" s
    show (ReturnStmt opt) = case opt of
      (Just e) -> printf "return %s;" (show e)
      Nothing -> "return;"
  
  data CFunction = CFunction [Specifier] CDeclarator [CDeclaration] CStatement
    deriving (Show)
  
  data CExpr
    = Constant CLiteral
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    | Cast CDeclaration CExpr
    | UnaryOp String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    deriving (Eq)
  
  instance Show CExpr where
    show (Constant l) = show l
    show (Identifier s) = s
    show (Index lhs rhs) = printf "( %s[%s] )" (show lhs) (show rhs)
    show (Call func args) = printf "( %s(%s) )" (show func) (show args)
    show (Cast typ expr) = printf "(%s)%s" (show typ) (show expr)
    show (UnaryOp str expr) = printf "(%s %s)" str (show expr)
    show (BinaryOp str lhs rhs) = printf "(%s %s %s)" (show lhs) str (show rhs)
    show (TernaryOp a b c) = printf "(%s ? %s : %s)" (show a) (show b) (show c)
  
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
    | FInline
    deriving (Eq)
  
  instance Show TypeQualifier where
    show QConst = "const"
    show QRestrict = "restrict"
    show QVolatile = "volatile"
    show FInline = "inline"
  
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
   | TTypedef String TypeSpecifier
   deriving (Eq, Show)
  
  data CDeclaration 
    = TopLevel [Specifier] CDeclarator Initializer
    | Multiple [Specifier] [(CDeclarator, Initializer)]
    | Sized CDeclaration CExpr
    | TypeName [Specifier] (Maybe CDeclarator)
    deriving (Eq, Show)
  
  data CDeclarator
   = Named String [DerivedDeclarator]
   | Abstract [DerivedDeclarator]
   deriving (Eq, Show)
  
  data Initializer 
    = InitExpression CExpr
    | InitList 
    | Uninitialized
    deriving (Eq, Show)
  
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] (Maybe CExpr)
   | Function [CDeclaration] Bool
   deriving (Eq, Show)
