module Language.C.Semantics.Nodes where
  
  type Name = String
  
  data SFunction = SFunction SType Name [SParameter] [SStatement]  
  
  data Signedness = Unsigned | Signed
  
  data IntegerFlags
  
  data FloatFlags
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar Signedness [Attribute]
    | SPointerTo CType [Attribute]
    | SArray CType (Maybe Expression) [Attribute] 
    | SFunctionPointer SFunction [Attribute]
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
  
  data family Attributed 