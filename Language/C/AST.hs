{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.AST where
  
  import Data.Either
  import Text.Printf
  import Data.Generics
  
  type BlockItem = Either CDeclaration CStatement
  
  data CStatement 
    = BreakStmt 
    | CaseStmt CExpr CStatement
    | CompoundStmt [BlockItem]
    | ContinueStmt
    | DefaultStmt CStatement
    | DoWhileStmt CStatement CExpr
    | EmptyStmt
    | ExpressionStmt CExpr
    | ForStmt (Maybe CExpr) (Maybe CExpr) (Maybe CExpr) CStatement
    | ForDeclStmt CDeclaration (Maybe CExpr) (Maybe CExpr) CStatement
    | GotoStmt String
    | IfStmt CExpr CStatement (Maybe CStatement)
    | LabeledStmt String CStatement
    | ReturnStmt (Maybe CExpr)
    | SwitchStmt CExpr CStatement
    | WhileStmt CExpr CStatement
    deriving (Eq, Show, Typeable, Data)
  
  data CFunction = CFunction [Specifier] CDeclarator CStatement
    deriving (Eq, Show, Typeable, Data)
    
  data CExternal 
    = FunctionDecl CFunction
    | ExternDecl CDeclaration
    deriving (Eq, Show, Typeable, Data)
  
  data CExpr
    = Constant CLiteral
    | Comma [CExpr]
    | Identifier String
    | Index CExpr CExpr
    | Call CExpr [CExpr]
    | Cast CDeclaration CExpr
    | UnaryOp String CExpr
    | BinaryOp String CExpr CExpr
    | TernaryOp CExpr CExpr CExpr
    | SizeOfType CDeclaration
    deriving (Eq, Typeable, Data)
  
  instance Show CExpr where
    show (Constant l) = show l
    show (Comma exprs) = show exprs
    show (Identifier s) = s
    show (Index lhs rhs) = printf "( %s[%s] )" (show lhs) (show rhs)
    show (Call func args) = printf "( %s(%s) )" (show func) (show args)
    show (Cast typ expr) = printf "(%s)%s" (show typ) (show expr)
    show (UnaryOp str expr) = printf "(%s %s)" str (show expr)
    show (BinaryOp str lhs rhs) = printf "(%s %s %s)" (show lhs) str (show rhs)
    show (TernaryOp a b c) = printf "(%s ? %s : %s)" (show a) (show b) (show c)
    show (SizeOfType t) = printf "sizeof(%s)" (show t)
  
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Eq, Typeable, Data)
    
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
    | SAttribute [CExpr]
    deriving (Eq, Show, Typeable, Data)
    
  data TypeQualifier
    = QConst
    | QRestrict
    | QVolatile
    | FInline
    deriving (Eq, Typeable, Data)
  
  instance Show TypeQualifier where
    show QConst = "const"
    show QRestrict = "restrict"
    show QVolatile = "volatile"
    show FInline = "inline"
  
  data Specifier 
    = TSpec TypeSpecifier
    | TQual TypeQualifier
    | SSpec StorageSpecifier
    deriving (Eq, Show, Typeable, Data)
    
  data Enumerator 
    = EnumIdent String
    | EnumAssign String CExpr
    deriving (Eq, Show, Typeable, Data)
  
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
     | TBool
     | TStructOrUnion (Maybe String) Bool [CDeclaration]
     | TEnumeration (Maybe String) [Enumerator]
     | TTypedef String CDeclaration
     | TTypeOfExpr CExpr
     deriving (Eq, Show, Typeable, Data)
  
  data CSize 
    = Sized CExpr
    | Unsized
    deriving (Eq, Show, Typeable, Data)
  
  data CDeclaration 
    = TopLevel [Specifier] [(CDeclarator, Initializer, CSize)]
    | Parameter [Specifier] CDeclarator
    | TypeName [Specifier] (Maybe CDeclarator)
    deriving (Eq, Show, Typeable, Data)
  
  data CDeclarator
   = Named String [DerivedDeclarator] (Maybe String) 
   | Abstract [DerivedDeclarator]
   deriving (Eq, Show, Typeable, Data)
  
  data Initializer 
    = InitExpression CExpr
    | InitList [Initializer]
    | Uninitialized
    deriving (Eq, Show, Typeable, Data)
  
  data DerivedDeclarator
   = Pointer [TypeQualifier]
   | Array [TypeQualifier] (Maybe CExpr)
   | Function [CDeclaration] Bool
   | Block
   deriving (Eq, Show, Typeable, Data)
   