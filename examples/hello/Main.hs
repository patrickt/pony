{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Main where
  
  import Language.C99.AST (CBuiltinExpr (BuiltinVaArg), CExpr (Constant), CTypeName (..), CDeclaration (..), 
    CSpecifier (..), CTypeQualifier (..), CTypeSpecifier (..), CStorageSpecifier (..), CDeclInfo (..))
  import Generics.Regular.Rewriting
  import Generics.Regular.Rewriting.Base
  import Generics.Regular.TH
  import Language.Pony

  
  deriveAll ''Signedness "PFSignedness"
  type instance PF Signedness = PFSignedness

  deriveAll ''IntegerFlags "PFIntegerFlags"
  type instance PF IntegerFlags = PFIntegerFlags

  deriveAll ''Enumeration "PFEnumeration"
  type instance PF Enumeration = PFEnumeration

  deriveAll ''FloatFlags "PFFloatFlags"
  type instance PF FloatFlags = PFFloatFlags

  deriveAll ''Attribute "PFAttribute"
  type instance PF Attribute = PFAttribute

  deriveAll ''Parameter "PFParameter"
  type instance PF Parameter = PFParameter

  deriveAll ''CompositeInfo "PFCompositeInfo"
  type instance PF CompositeInfo = PFCompositeInfo

  deriveAll ''CompositeType "PFCompositeType"
  type instance PF CompositeType = PFCompositeType

  deriveAll ''EnumerationInfo "PFEnumerationInfo"
  type instance PF EnumerationInfo = PFEnumerationInfo

  deriveAll ''Field "PFField"
  type instance PF Field = PFField

  deriveAll ''CLiteral "PFCLiteral"
  type instance PF CLiteral = PFCLiteral

  deriveAll ''Expression "PFExpression"
  type instance PF Expression = PFExpression

  deriveAll ''CBuiltinExpr "PFCBuiltinExpr"
  type instance PF CBuiltinExpr = PFCBuiltinExpr

  deriveAll ''InitList "PFInitList"
  type instance PF InitList = PFInitList

  deriveAll ''SType "PFSType"
  type instance PF SType = PFSType

  deriveAll ''CExpr "PFCExpr"
  type instance PF CExpr = PFCExpr

  deriveAll ''CTypeName "PFCTypeName"
  type instance PF CTypeName = PFCTypeName

  deriveAll ''CDeclaration "PFCDeclaration"
  type instance PF CDeclaration = PFCDeclaration

  deriveAll ''CSpecifier "PFCSpecifier"
  type instance PF CSpecifier = PFCSpecifier

  deriveAll ''CTypeSpecifier "PFCTypeSpecifier"
  type instance PF CTypeSpecifier = PFCTypeSpecifier

  deriveAll ''CStorageSpecifier "PFCStorageSpecifier"
  type instance PF CStorageSpecifier = PFCStorageSpecifier

  deriveAll ''CTypeQualifier "PFCTypeQualifier"
  type instance PF CTypeQualifier = PFCTypeQualifier

  deriveAll ''CDeclInfo "PFCDeclInfo"
  type instance PF CDeclInfo = PFCDeclInfo

  instance (LRBase a) => LRBase (Maybe a) where
    leftb = Just leftb
    rightb = Nothing

  instance LRBase Signedness where
    leftb = Unsigned 
    rightb = Signed

  instance LRBase CompositeType where
    leftb = Struct 
    rightb = Union

  instance LRBase Attribute where
    leftb = Volatile
    rightb = Const

  instance LRBase FloatFlags where
    leftb = FFloat
    rightb = FDouble

  instance LRBase IntegerFlags where
    leftb = IntegerFlags leftb leftb
    rightb = IntegerFlags rightb rightb

  instance LRBase CompositeInfo where
    leftb = CompositeInfo leftb leftb leftb
    rightb = CompositeInfo rightb rightb rightb

  instance LRBase EnumerationInfo where
    leftb = EnumerationInfo leftb leftb
    rightb = EnumerationInfo rightb rightb

  instance LRBase Parameter where
    leftb = Parameter leftb leftb
    rightb = Parameter rightb rightb

  instance LRBase CLiteral where 
    leftb = CInteger leftb
    rightb = CString rightb

  instance LRBase Expression where
    leftb = Literal leftb
    rightb = Ident rightb

  instance LRBase SType where
    leftb = SVoid leftb
    rightb = SInt rightb rightb

  instance LRBase Enumeration where
    leftb = Enumeration leftb leftb
    rightb = Enumeration rightb rightb

  instance LRBase Field where
    leftb = Field leftb leftb leftb
    rightb = Field rightb rightb rightb

  instance LRBase CExpr where
    leftb = Constant leftb
    rightb = Constant rightb

  instance LRBase InitList where
    leftb = InitList leftb
    rightb = InitList rightb

  instance LRBase CTypeName where
    leftb = CTypeName leftb
    rightb = CTypeName rightb

  instance LRBase CBuiltinExpr where
    leftb = BuiltinVaArg leftb leftb
    rightb = BuiltinVaArg rightb rightb

  instance LRBase CDeclaration where
    leftb = CDeclaration leftb leftb
    rightb = CDeclaration rightb rightb

  instance LRBase Designator where
    leftb = ArrayDesignator leftb
    rightb = ArrayDesignator rightb

  instance LRBase Initializer where
    leftb = InitExpression leftb
    rightb = InitExpression rightb


  instance LRBase CTypeSpecifier where
    leftb = TVoid
    rightb = TChar


  instance LRBase CTypeQualifier where
    leftb = CConst
    rightb = CVolatile

  instance LRBase CSpecifier where
    leftb = TSpec leftb
    rightb = TQual rightb

  instance LRBase CStorageSpecifier where
    leftb = CAuto
    rightb = CRegister

  instance LRBase CDeclInfo where
    leftb = CDeclInfo Nothing Nothing leftb
    rightb = CDeclInfo Nothing Nothing rightb

  instance (LRBase a, LRBase b) => LRBase (a, b) where
    leftb = (leftb, leftb)
    rightb = (rightb, rightb)

  instance Rewrite CSpecifier
  instance Rewrite Enumeration
  instance Rewrite CompositeType
  instance Rewrite EnumerationInfo
  instance Rewrite CompositeInfo
  instance Rewrite Signedness
  instance Rewrite FloatFlags
  instance Rewrite IntegerFlags
  instance Rewrite Attribute
  instance Rewrite Parameter
  instance Rewrite CLiteral
  instance Rewrite SType
  instance Rewrite CBuiltinExpr
  instance Rewrite CDeclaration
  instance Rewrite Expression

  helloRule :: Rule Expression
  helloRule = rule $ (Ident "hello") :~> FunctionCall (Ident "printf") [(CStr "Hello from Pony!")]

  helloToPrintf :: Expression -> Expression
  helloToPrintf x = rewrite helloRule x
  
  helloT :: GenericT
  helloT = mkT $ rewrite helloRule
  
  main :: IO ()
  main = run $ pony {
    transformations = [MkTrans "Hello" TopDown helloT ]
  }