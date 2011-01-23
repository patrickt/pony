{-# LANGUAGE DeriveDataTypeable #-}

module Semantics.C.Nodes where
  
  -- TODO: When this is reasonably stable, rename it to Semantics.C.Tree
  
  import Data.Generics
  import Language.Pony.MachineSizes
  import Control.Applicative ((<$>))
  import Language.C.AST (CExpr)
  import Semantics.C.Pretty
  
  type Name = String
  type SParameter = ()
  type Expression = CExpr
  type CompositeInfo = ()
  type EnumerationInfo = ()
  type SFields = ()
  
  data SFunction = SFunction SType Name [SVariable] [SLocal] 
    deriving (Show, Typeable, Data)
  
  instance Pretty SFunction where
    pretty (SFunction retType name params body) = 
      pretty retType 
        <+> pretty name 
        <+> (parens $ hsep $ punctuate comma (pretty <$> params)) 
        $+$ braces (nest 2 (vcat $ pretty <$> body))
  
  
  data Signedness = Unsigned | Signed deriving (Show, Typeable, Data)
  instance Pretty Signedness where
    pretty Unsigned = text "u"
    pretty Signed = empty
  
  data IntegerFlags = IntegerFlags Signedness Int deriving (Show, Typeable, Data)
  
  data FloatFlags = FFloat | FDouble | FLongDouble deriving (Show, Typeable, Data)
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar Signedness [Attribute]
    | SPointerTo SType [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
    deriving (Show, Typeable, Data)
    
  instance Pretty SType where
    pretty (SVoid _) = text "void"
    pretty (SInt (IntegerFlags s w) _) = pretty s <> text "int" <> pretty w
    pretty (SFloat FFloat _) = text "float"
    pretty (SFloat FDouble _) = text "double"
    pretty (SFloat FLongDouble _) = text "long double"
    pretty (SChar signedness _) = text "char"
    pretty (SPointerTo t _) = pretty t <+> text "*"
    pretty (SArray t mE _) = pretty t <> text "[]"
    pretty (SComposite _ _) = undefined
    pretty (SEnum _ _) = undefined
    
  data SVariable = Variable Name SType deriving (Show, Typeable, Data)
  
  instance Pretty SVariable where
    pretty (Variable n t) = pretty t <+> pretty n
  
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
    deriving (Show, Typeable, Data)
  
  data Attribute 
    = Auto
    | Const 
    | Extern
    | Restrict
    | Static
    | Register
    | Volatile
    deriving (Show, Typeable, Data)
  
  void = SVoid []
  signedChar = SChar Signed []
  unsignedChar = SChar Unsigned []
  shortSignedInt = SInt (IntegerFlags Signed sizeOfShort) []
  shortUnsignedInt = SInt (IntegerFlags Unsigned sizeOfShort) []
  signedInt = SInt (IntegerFlags Signed sizeOfInt) []
  unsignedInt = SInt (IntegerFlags Unsigned sizeOfInt) []
  longSignedInt = SInt (IntegerFlags Signed sizeOfLong) []
  longUnsignedInt = SInt (IntegerFlags Unsigned sizeOfLong) []
  longLongSignedInt = SInt (IntegerFlags Signed sizeOfLongLong) []
  longlongUnsignedInt = SInt (IntegerFlags Unsigned sizeOfLongLong) []
  float = SFloat FFloat []
  double = SFloat FDouble []
  longDouble = SFloat FLongDouble []
  
  -- Do we need to distinguish between statements and instructions, like CIL does?
  -- Will we need a separate ADT for typedefs? I feel that global typedefs are a good first step.
  data SLocal
    = LDeclaration SVariable
    | LStatement Statement
    deriving (Show, Typeable, Data)
    
  instance Pretty SLocal where
    pretty _ = text "local;"
  
  
  data SGlobal
    = GFunction SFunction
    | GVariable SVariable
    | GFunctionPrototype SFunction
    | GTypedef Name SType
    | GComposite Bool [SFields]
    deriving (Show, Typeable, Data)
  
  type Program = [SGlobal]