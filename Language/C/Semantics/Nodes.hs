module Language.C.Semantics.Nodes where
  
  type Name = String
  
  data SFunction = SFunction SType Name [SParameter] [SStatement]  
  
  data Signedness = Unsigned | Signed
  data Width = Short | Regular | Long | LongLong
  
  data IntegerFlags = IntegerFlags Signedness Width
  
  data FloatFlags = FFloat | FDouble | FLongDouble
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar Signedness [Attribute]
    | SPointerTo SType [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SFunctionPointer SFunction [Attribute]
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
  
  