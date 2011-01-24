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
    deriving (Show, Eq, Typeable, Data)
  
  instance Pretty SFunction where
    pretty (SFunction retType name params body) = 
      pretty retType 
        <+> pretty name 
        <+> (parens $ hsep $ punctuate comma (pretty <$> params)) 
        $+$ braces (nest 2 (vcat $ pretty <$> body))
  
  
  data Signedness = Unsigned | Signed deriving (Show, Typeable, Eq, Data)
  instance Pretty Signedness where
    pretty Unsigned = text "u"
    pretty Signed = empty
  
  data IntegerFlags = IntegerFlags Signedness Int deriving (Show, Eq, Typeable, Data)
  
  data FloatFlags = FFloat | FDouble | FLongDouble deriving (Show, Eq, Typeable, Data)
  
  data SType 
    = SVoid [Attribute]
    | SInt IntegerFlags [Attribute]
    | SFloat FloatFlags [Attribute]
    | SChar Signedness [Attribute]
    | SPointerTo SType [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
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
    
  data SVariable = Variable Name SType deriving (Show, Eq, Typeable, Data)
  
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
    deriving (Show, Eq, Typeable, Data)
    
  instance Pretty Statement where
    pretty Break = text "break"
    pretty (Case _ _) = text "case (TODO)"
    pretty (Compound _) = text "compound (TODO)"
    pretty Continue = text "continue"
    pretty (Default s) = text "default:" <+> pretty s
    pretty (DoWhile _ _) = text "dowhile (TODO)"
    pretty EmptyS = empty
    pretty (ExpressionS s) = text $ show s
    pretty (For _ _ _ _) = text "for(TODO)"
    pretty (GoTo n) = text "goto" <+> pretty n
    pretty (IfThen _ _) = text "if(TODO)"
    pretty (IfThenElse _ _ _) = text "ifelse(TODO)"
    pretty (Labeled name _ s) = pretty name <> colon <+> pretty s
    pretty (Return _) = text "return (TODO)"
    pretty (Switch _ s) = text "switch(TODO)" <+> pretty s
    pretty (While _ _) = text "while(TODO)"
  
  data Attribute 
    = Auto
    | Const 
    | Extern
    | Restrict
    | Static
    | Register
    | Volatile
    deriving (Show, Eq, Typeable, Data)
  
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
    deriving (Show, Eq, Typeable, Data)
    
  instance Pretty SLocal where
    pretty (LStatement s) = pretty s <> semicolon
    pretty _ = text "local;"
  
  
  data SGlobal
    = GFunction SFunction
    | GVariable SVariable
    | GFunctionPrototype SFunction
    | GTypedef Name SType
    | GComposite Bool [SFields]
    deriving (Show, Eq, Typeable, Data)
  
  type Program = [SGlobal]