{-# LANGUAGE DeriveDataTypeable #-}

module Semantics.C.Nodes where
  
  import Data.Generics
  import Language.Pony.MachineSizes
  import Language.C.Literals
  import qualified Language.C.AST as AST
  
  type Name = String
  
  -- | A semantic function has five components: its attributes, its return type (a 'SType'),
  -- its name, its parameters (a list of 'SVariables'), and a boolean that 
  -- determines whether it is variadic or not.
  data SFunction 
    = SFunction [Attribute] SType Name [SParameter] [SLocal] Bool
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
    | SFunctionPointer SType [SParameter] [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
    | Typedef Name SType [Attribute]
    | SBuiltinType Name [Attribute]
    deriving (Show, Eq, Typeable, Data)
  
  setAttributes :: SType -> [Attribute] -> SType
  setAttributes (SVoid _) a = SVoid a
  setAttributes (SInt f _) a = SInt f a
  setAttributes (SFloat f _) a = SFloat f a
  setAttributes (SChar s _) a = SChar s a
  setAttributes (SPointerTo t _) a = SPointerTo t a
  setAttributes (SArray t e _) a = SArray t e a
  setAttributes (SComposite i _) a = SComposite i a
  setAttributes (SEnum i _) a = SEnum i a
  setAttributes (Typedef n t _) a = Typedef n t a
  setAttributes (SBuiltinType n _) a = SBuiltinType n a
  
  data EnumerationInfo = EnumerationInfo (Maybe Name) [Enumeration]
    deriving (Show, Eq, Typeable, Data)
  
  data Enumeration = Enumeration Name (Maybe Expression)
    deriving (Show, Eq, Typeable, Data)
  
  data CompositeType = Struct | Union deriving (Show, Eq, Typeable, Data)
  
  data CompositeInfo = CompositeInfo CompositeType (Maybe Name) [SField]
    deriving (Show, Eq, Typeable, Data)
  
  data SField = SField (Maybe Name) SType (Maybe Expression)
    deriving (Show, Eq, Typeable, Data)
  
  data SVariable = SVariable Name SType (Maybe Expression) deriving (Show, Eq, Typeable, Data)
  
  data SParameter = SParameter (Maybe Name) SType deriving (Show, Eq, Typeable, Data)
  
  data Statement
    = Asm Bool Expression (Maybe Expression) (Maybe Expression) (Maybe Expression)
    | Break
    | Case Expression Statement
    | Compound [SLocal]
    | Continue
    | Default Statement
    | DoWhile Statement Expression
    | EmptyS
    | ExpressionS Expression
    | For (Maybe SLocal) (Maybe Expression) (Maybe Expression) Statement
    | GoTo Name
    | IfThen Expression Statement
    | IfThenElse Expression Statement Statement
    | Labeled Name [Attribute] Statement
    | Return (Maybe Expression)
    | Switch Expression Statement
    | While Expression Statement 
    deriving (Show, Eq, Typeable, Data)
    
  data Expression
    = Literal CLiteral
    | Str String
    | Ident Name
    | Brackets Expression Expression
    | FunctionCall Expression [Expression]
    | Cast SType Expression
    | Unary Name Expression
    | Binary Expression Name Expression
    | Ternary Expression Expression Expression
    | SizeOfSType SType
    | Builtin AST.BuiltinExpr
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
  data SLocal
    = LDeclaration SVariable
    | LStatement Statement
    deriving (Show, Eq, Typeable, Data)
  
  type FunctionBody = [SLocal]
  
  data SGlobal
    = GFunction SFunction
    | GVariable SVariable
    | GFunctionPrototype SType Name [SParameter] Bool
    | GTypedef Name SType
    | GEnumeration EnumerationInfo
    | GComposite CompositeInfo
    deriving (Show, Eq, Typeable, Data)
  
  type Program = [SGlobal]
  