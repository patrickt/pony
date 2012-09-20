{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeOperators, TypeFamilies, FlexibleContexts #-}


module Language.Pony.Rewriting where

  import Semantics.C
  import Language.C99.Literals
  import Generics.Regular.Rewriting
  import Generics.Regular.TH

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

  deriveAll ''InitList "PFInitList"
  type instance PF InitList = PFInitList

  deriveAll ''SType "PFSType"
  type instance PF SType = PFSType
  
  deriveAll ''SGlobal "PFSGlobal"
  type instance PF SGlobal = PFSGlobal
  
  deriveAll ''SBuiltin "PFSBuiltin"
  type instance PF SBuiltin = PFSBuiltin

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

  instance LRBase InitList where
    leftb = InitList leftb
    rightb = InitList rightb
  
  instance LRBase Designator where
    leftb = ArrayDesignator leftb
    rightb = ArrayDesignator rightb

  instance LRBase Initializer where
    leftb = InitExpression leftb
    rightb = InitExpression rightb
  
  instance LRBase SGlobal where
    leftb = GTypedef leftb leftb
    rightb = GEnumeration rightb
  
  instance LRBase SBuiltin where
    leftb = SVaArg leftb leftb
    rightb = SVaArg rightb rightb

  instance (LRBase a, LRBase b) => LRBase (a, b) where
    leftb = (leftb, leftb)
    rightb = (rightb, rightb)
    
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
  instance Rewrite Expression