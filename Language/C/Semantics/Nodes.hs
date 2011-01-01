module Language.C.Semantics.Nodes where
  
  type Name = String
  type SParameter = ()
  type SStatement = ()
  type Attribute = ()
  type Expression = ()
  type CompositeInfo = ()
  type EnumerationInfo = ()
  
  data SFunction = SFunction SType Name [SParameter] [SStatement] deriving (Show)
  
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
  
  