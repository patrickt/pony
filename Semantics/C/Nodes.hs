module Semantics.C.Nodes where
  
  type Name = String
  type SParameter = ()
  type SStatement = ()
  type Expression = ()
  type CompositeInfo = ()
  type EnumerationInfo = ()
  
  -- The parameters should really be SParameters.
  data SFunction = SFunction SType Name [SType] [SStatement] deriving (Show)
  
  data Signedness = Unsigned | Signed deriving (Show)
  data Width = Short | Regular | Long | LongLong deriving (Show)
  
  data IntegerFlags = IntegerFlags Signedness Width deriving (Show)
  
  data FloatFlags = FFloat | FDouble | FLongDouble deriving (Show)
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar Signedness [Attribute]
    | SPointerTo SType [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SFunctionPointer SFunction [Attribute] -- hmm...
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
    deriving (Show)
  
  data Attribute 
    = Const 
    | Restrict
    | Volatile
    deriving (Show)
  
  void = SVoid []
  signedChar = SChar Signed []
  unsignedChar = SChar Unsigned []
  shortSignedInt = SInt (IntegerFlags Signed Short) []
  shortUnsignedInt = SInt (IntegerFlags Unsigned Short) []
  signedInt = SInt (IntegerFlags Signed Regular) []
  unsignedInt = SInt (IntegerFlags Unsigned Regular) []
  longSignedInt = SInt (IntegerFlags Signed Long) []
  longUnsignedInt = SInt (IntegerFlags Unsigned Long) []
  longLongSignedInt = SInt (IntegerFlags Signed LongLong) []
  longlongUnsignedInt = SInt (IntegerFlags Unsigned LongLong) []
  float = SFloat FFloat []
  double = SFloat FDouble []
  longDouble = SFloat FLongDouble []