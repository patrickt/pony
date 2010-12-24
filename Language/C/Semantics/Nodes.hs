module Language.C.Semantics.Nodes where
  
  type Name = String
  
  data SFunction = SFunction SType Name [SParameter] [SStatement]  
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SPointerTo CType [Attribute]
    | SArray CType (Maybe Expression) [Attribute] 
    | SFunctionPointer SFunction [Attribute]
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
  
  