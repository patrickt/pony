module Semantics.C.Nodes where
  
  import Language.C.AST (CExpr)
  
  type Name = String
  type SParameter = ()
  type Expression = CExpr
  type CompositeInfo = ()
  type EnumerationInfo = ()
  type SFields = ()
  
  -- The parameters should really be SParameters.
  data SFunction = SFunction SType Name [SVariable] [SLocal] deriving (Show)
  
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
    
  data SVariable = Variable Name SType deriving (Show)
  
  data Statement
    = Break
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
    deriving (Show)
  
  data Attribute 
    = Auto
    | Const 
    | Extern
    | Restrict
    | Static
    | Register
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
  
  -- Do we need to distinguish between statements and instructions, like CIL does?
  -- Will we need a separate ADT for typedefs? I feel that global typedefs are a good first step.
  data SLocal
    = LDeclaration SVariable
    | LStatement Statement
    deriving (Show)
  
  
  data SGlobal
    = GFunction SFunction
    | GVariable SVariable
    | GFunctionPrototype SFunction
    | GTypedef Name SType
    | GComposite Bool [SFields]
    deriving (Show)
  
  type Program = [SGlobal]