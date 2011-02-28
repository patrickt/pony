{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

module Semantics.C.Nodes where
  
  import Data.Generics
  import Language.Pony.MachineSizes
  import Control.Applicative ((<$>))
  import Language.C.AST (CLiteral (..))
  import qualified Language.C.AST as AST
  import Semantics.C.Pretty
  
  type Name = String
  type SParameter = ()
  type SFields = ()
  
  -- | A semantic function has four components: its return type (a 'SType'),
  -- its name, its parameters (a list of 'SVariables'), and a boolean that 
  -- determines whether it is variadic or not.
  data SFunction 
    = SFunction SType Name [SVariable] [SLocal] Bool
    deriving (Show, Eq, Typeable, Data)
  
  instance Pretty SFunction where
    pretty (SFunction retType name params body isVariadic) = 
      pretty retType <+> pretty name <> parens (parameters <> ellipsis) 
      <+> lbrace 
        $$ bodyContents 
      $$ rbrace
        where parameters = hsep $ punctuate comma (pretty <$> params)
              bodyContents = nest 2 (vcat $ pretty <$> body)
              ellipsis = if isVariadic then text ", ..." else empty
  
  data Signedness 
    = Unsigned 
    | Signed 
    deriving (Show, Typeable, Eq, Data)
  
  instance Pretty Signedness where
    pretty Unsigned = text "u"
    pretty Signed = empty
  
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
    | SChar Signedness [Attribute]
    | SPointerTo SType [Attribute]
    | SFunctionPointer SType [SVariable] [Attribute]
    | SArray SType (Maybe Expression) [Attribute] 
    | SComposite CompositeInfo [Attribute]
    | SEnum EnumerationInfo [Attribute]
    | STypedef String [Attribute]
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
  setAttributes (STypedef n _) a = STypedef n a
    
  instance Pretty SType where
    pretty (SVoid _) = text "void"
    pretty (SInt (IntegerFlags s w) _) = pretty s <> text "int" <> pretty w
    pretty (SFloat FFloat _) = text "float"
    pretty (SFloat FDouble _) = text "double"
    pretty (SFloat FLongDouble _) = text "long double"
    pretty (SChar signedness _) = text "char"
    pretty (SPointerTo t _) = pretty t <+> text "*"
    pretty (SArray t mE _) = pretty t <> text "[]"
    pretty (SComposite _ _) = pretty "FIXME"
    pretty (SEnum i _) = pretty i
    pretty (STypedef s _) = text s
  
  data EnumerationInfo = EnumerationInfo Name [(Name, Expression)]
    deriving (Show, Eq, Typeable, Data)
    
  instance Pretty EnumerationInfo where 
    pretty (EnumerationInfo n vals) =
      text "enum" <+> text n $+$ braces values where
        values = vcat $ map pretty' vals
        pretty' (n, e) = text n <+> equals <+> pretty e
  
  -- use Either instead of a Bool here.
  
  data CompositeInfo = CompositeInfo Bool (Maybe Name) [SField]
    deriving (Show, Eq, Typeable, Data)
  
  data SField = SField Name SType (Maybe Expression)
    deriving (Show, Eq, Typeable, Data)
    
  instance Pretty SField where
    pretty (SField n t Nothing) = pretty n <+> pretty t <> semicolon
    pretty (SField n t (Just i)) = pretty n <+> pretty t <> colon <+> pretty i <> semicolon
    
  data SVariable = Variable Name SType (Maybe Expression) deriving (Show, Eq, Typeable, Data)
  
  instance Pretty SVariable where
    pretty (Variable n t Nothing) = pretty t <+> pretty n
    pretty (Variable n t (Just e)) = pretty t <+> pretty n <+> equals <+> pretty e
  
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
    pretty (Case e s) = text "case" <+> pretty e <> colon <+> pretty s
    pretty (Compound b) = lbrace $$ nest 2 (vcat (pretty <$> b)) $$ rbrace
    pretty Continue = text "continue"
    pretty (Default s) = text "default:" <+> pretty s
    pretty (DoWhile s e) = text "do" <+> pretty s <+> text "while" <+> parens (pretty e)
    pretty EmptyS = empty
    pretty (ExpressionS s) = pretty s
    pretty (For _ _ _ _) = text "for(TODO)"
    pretty (GoTo n) = text "goto" <+> pretty n
    pretty (IfThen e s) = text "if" <+> parens (pretty e) <+> pretty s
    pretty (IfThenElse e s s') = text "if" <+> parens (pretty e) <+> pretty s <+> text "else" <+> pretty s'
    pretty (Labeled name _ s) = pretty name <> colon <+> pretty s
    pretty (Return Nothing) = text "return"
    pretty (Return (Just e)) = text "return" <+> pretty e
    pretty (Switch e s) = text "switch" <+> parens (pretty e) <+> pretty s
    pretty (While e s) = text "while" <+> parens (pretty e) <+> pretty s
    
  data Expression
    = Literal AST.CLiteral
    | Ident Name
    | Brackets Expression Expression
    | FunctionCall Expression [Expression]
    | SCast SType Expression
    | Unary Name Expression
    | Binary Expression Name Expression
    | Ternary Expression Expression Expression
    | SizeOfSType SType
    | Builtin AST.BuiltinExpr
    deriving (Show, Eq, Typeable, Data)
    
  instance Pretty AST.CLiteral where
    pretty (CInteger i) = textS i
    pretty (CChar c) = textS c
    pretty (CFloat f) = textS f
    pretty (CString s) = textS s 
    
  instance Pretty Expression where
    pretty (Literal l) = pretty l
    pretty (Ident n) = text n
    pretty (Brackets lhs rhs) = pretty lhs <> brackets (pretty rhs)
    pretty (FunctionCall lhs args) = pretty lhs <> parens (hcat $ punctuate comma (pretty <$> args))
    pretty (SCast t e) = parens $ pretty t <> pretty e
    pretty (Unary n e) = text n <> pretty e
    pretty (Binary lhs op rhs) = parens $ pretty lhs <+> text op <+> pretty rhs
    pretty (Ternary a b c) = pretty a <+> question <+> pretty b <+> colon <+> pretty c
    pretty (SizeOfSType t) = text "sizeof" <> parens (pretty t)
    pretty (Builtin b) = textS b
    
  intToLiteral :: Int -> Expression
  intToLiteral i = Literal (CInteger (toInteger i))
  
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
    pretty (LDeclaration d) = pretty d <> semicolon
  
  type FunctionBody = [SLocal]
  
  data SGlobal
    = GFunction SFunction
    | GVariable SVariable
    | GFunctionPrototype SFunction
    | GTypedef Name SType
    | GComposite Bool [SFields]
    deriving (Show, Eq, Typeable, Data)
  
  instance Pretty SGlobal where
    pretty (GFunction g) = pretty g
    pretty (GVariable v) = pretty v
    pretty _ = pretty "FIXME"
  
  type Program = [SGlobal]
  instance Pretty Program where
    pretty a = vcat $ pretty <$> a
  
  