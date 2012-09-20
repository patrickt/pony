{-# LANGUAGE DeriveDataTypeable #-}

module Semantics.C.ASG 
  ( Attribute (..)
  , AsmOp (..)
  , CompositeInfo (..)
  , CompositeType (..)
  , Designator (..)
  , Enumeration (..)
  , EnumerationInfo (..)
  , Expression (..)
  , Field (..)
  , FloatFlags (..)
  , Function (..)
  , Initializer (..)
  , InitList (..)
  , IntegerFlags (..)
  , Local (..)
  , Name
  , Parameter (..)
  , Program
  , SBuiltin (..)
  , SGlobal (..)
  , Signedness (..)
  , Statement (..)
  , SType (..)
  , Variable (..)
  , char
  , double
  , float
  , int128
  , intToLiteral
  , longDouble
  , longLongSignedInt
  , longlongUnsignedInt
  , longSignedInt
  , longUnsignedInt
  , setAttributes
  , shortSignedInt
  , shortUnsignedInt
  , signedChar
  , signedInt
  , uint128
  , unsignedChar
  , unsignedInt
  , void
  ) where
  
  import Data.Monoid
  import Language.Pony.Prelude
  import Data.Generics
  import Language.Pony.MachineSizes
  import Language.C99.Literals
  
  type Name = String
  
  -- | A semantic function has five components: its attributes, its return type (a 'SType'),
  -- its name, its parameters (a list of 'Variables'), and a boolean that 
  -- determines whether it is variadic or not.
  data Function 
    = Function [Attribute] SType Name [Parameter] [Local] Bool
    deriving (Show, Eq, Typeable, Data)
  
  data Signedness 
    = Unsigned 
    | Signed 
    deriving (Show, Typeable, Eq, Data)
  
  data IntegerFlags 
    = IntegerFlags Signedness Int deriving (Show, Eq, Typeable, Data)
  
  data FloatFlags 
    = FFloat 
    | FDouble 
    | FLongDouble deriving (Show, Eq, Typeable, Data)
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar (Maybe Signedness) [Attribute]
    | SPointerTo SType [Attribute]
    | SFunctionPointer SType [Parameter] [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
    | STypedef Name SType [Attribute]
    | SBuiltinType Name [Attribute]
    deriving (Show, Eq, Typeable, Data)
  
  setAttributes :: SType -> [Attribute] -> SType
  setAttributes (SVoid _) a = SVoid a
  setAttributes (SInt f _) a = SInt f a
  setAttributes (SFloat f _) a = SFloat f a
  setAttributes (SChar s _) a = SChar s a
  setAttributes (SPointerTo t _) a = SPointerTo t a
  setAttributes (SFunctionPointer t p _) a = SFunctionPointer t p a
  setAttributes (SArray t e _) a = SArray t e a
  setAttributes (SComposite i _) a = SComposite i a
  setAttributes (SEnum i _) a = SEnum i a
  setAttributes (STypedef n t _) a = STypedef n t a
  setAttributes (SBuiltinType n _) a = SBuiltinType n a
  
  data EnumerationInfo = EnumerationInfo (Maybe Name) [Enumeration]
    deriving (Show, Eq, Typeable, Data)
  
  data Enumeration = Enumeration Name (Maybe Expression)
    deriving (Show, Eq, Typeable, Data)
  
  data CompositeType = Struct | Union deriving (Show, Eq, Typeable, Data)
  
  data CompositeInfo = CompositeInfo CompositeType (Maybe Name) [Field]
    deriving (Show, Eq, Typeable, Data)
  
  data Field = Field (Maybe Name) SType (Maybe Expression)
    deriving (Show, Eq, Typeable, Data)
  
  data Variable = Variable Name SType (Maybe Expression) deriving (Show, Eq, Typeable, Data)
  
  data Parameter = Parameter (Maybe Name) SType deriving (Show, Eq, Typeable, Data)
  
  data AsmOp = AsmOp Expression (Maybe Expression) deriving (Show, Eq, Typeable, Data)
  
  data Statement
    = Asm Bool Expression [AsmOp] [AsmOp] [Expression]
    | Break
    | Case Expression Statement
    | Compound [Local]
    | Continue
    | Default Statement
    | DoWhile Statement Expression
    | EmptyS
    | ExpressionS Expression
    | For (Maybe Local) (Maybe Expression) (Maybe Expression) Statement
    | GoTo Expression
    | IfThen Expression Statement
    | IfThenElse Expression Statement Statement
    | Labeled Name [Attribute] Statement
    | Return (Maybe Expression)
    | Switch Expression Statement
    | While Expression Statement 
    deriving (Show, Eq, Typeable, Data)
  
  instance Monoid Statement where
    mempty = EmptyS
    mappend (Compound ls) (Compound rs) = Compound $ ls ++ rs
    mappend l EmptyS = l
    mappend EmptyS r = r
    mappend (Compound ls) r = Compound $ ls ++ [LStatement r]
    mappend l (Compound rs) = Compound ((LStatement l) : rs)
    mappend l r = Compound $ [LStatement l, LStatement r]
    
  data Expression
    = Literal CLiteral
    | CStr String
    | Ident Name
    | Brackets Expression Expression
    | FunctionCall Expression [Expression]
    | Cast SType Expression
    | Unary Name Expression
    | Binary Expression Name Expression
    | Ternary Expression Expression Expression
    | SizeOfSType SType
    | Builtin SBuiltin
    -- | Initializers can *only* appear on the right hand side of an assignment expression.
    -- Woe betide you if you do not abide by this rule.
    | InitializerList InitList
    deriving (Show, Eq, Typeable, Data)
  
  data Designator 
    = ArrayDesignator Expression
    | MemberDesignator Name
    deriving (Show, Eq, Typeable, Data)
    
  data Initializer 
    = InitExpression Expression
    | Composite InitList
    deriving (Show, Eq, Typeable, Data)
  
  data InitList = InitList [([Designator], Initializer)]
    deriving (Show, Eq, Typeable, Data)
  
  data SBuiltin
    = SVaArg Expression SType
    deriving (Eq, Show, Typeable, Data)
  
  intToLiteral :: Int -> Expression
  intToLiteral i = Literal (CInteger (toInteger i))
  
  data Attribute 
    = Auto
    | Const 
    | Extern
    | Inline
    | Register
    | Restrict
    | Static
    | Volatile
    | Custom [Expression]
    deriving (Show, Eq, Typeable, Data)
  
  void, char, signedChar, unsignedChar, shortSignedInt, shortUnsignedInt, 
    signedInt, unsignedInt, longSignedInt, longUnsignedInt, longLongSignedInt,
    longlongUnsignedInt, int128, uint128, float, double, longDouble :: SType
  
  void = SVoid []
  char = SChar Nothing []
  signedChar = SChar (Just Signed) []
  unsignedChar = SChar (Just Unsigned) []
  shortSignedInt = SInt (IntegerFlags Signed sizeOfShort) []
  shortUnsignedInt = SInt (IntegerFlags Unsigned sizeOfShort) []
  signedInt = SInt (IntegerFlags Signed sizeOfInt) []
  unsignedInt = SInt (IntegerFlags Unsigned sizeOfInt) []
  longSignedInt = SInt (IntegerFlags Signed sizeOfLong) []
  longUnsignedInt = SInt (IntegerFlags Unsigned sizeOfLong) []
  longLongSignedInt = SInt (IntegerFlags Signed sizeOfLongLong) []
  longlongUnsignedInt = SInt (IntegerFlags Unsigned sizeOfLongLong) []
  int128 = SInt (IntegerFlags Signed sizeOfInt128) []
  uint128 = SInt (IntegerFlags Unsigned sizeOfInt128) []
  float = SFloat FFloat []
  double = SFloat FDouble []
  longDouble = SFloat FLongDouble []
  
  -- Do we need to distinguish between statements and instructions, like CIL does?
  -- Will we need a separate ADT for typedefs? I feel that global typedefs are a good first step.
  data Local
    = LDeclaration Variable
    | LStatement Statement
    deriving (Show, Eq, Typeable, Data)
  
  -- todo: drop S-prefix
  data SGlobal
    = GFunction Function
    | GVariable Variable
    | GFunctionPrototype SType Name [Parameter] Bool
    | GTypedef Name SType
    | GEnumeration EnumerationInfo
    | GComposite CompositeInfo
    deriving (Show, Eq, Typeable, Data)
  
  type Program = [SGlobal]
  