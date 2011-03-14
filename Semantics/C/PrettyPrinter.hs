{-# LANGUAGE TypeSynonymInstances #-}

module Semantics.C.PrettyPrinter where
  
  import Control.Applicative ((<$>))
  import Semantics.C.Nodes
  import Language.Pony.MachineSizes
  import Language.C.Literals
  import Text.Pretty
  
  instance Pretty SFunction where
    pretty (SFunction retType name params body isVariadic) = 
      pretty retType <+> pretty name <> parens (parameters <> ellipsis) 
      <+> lbrace 
        $$ bodyContents 
      $$ rbrace
        where parameters = hsep $ punctuate comma (pretty <$> params)
              bodyContents = nest 2 (vcat $ pretty <$> body)
              ellipsis = if isVariadic then text ", ..." else empty
  
  instance Pretty Signedness where
    pretty Unsigned = text "unsigned"
    pretty Signed = text "signed"
  
  instance Pretty SType where
    pretty (SVoid _) = text "void"
    pretty (SInt (IntegerFlags Signed w) _) = text $ intTypeFromSize w
    pretty (SInt (IntegerFlags Unsigned w) _) = (text "unsigned") <+> (text $ intTypeFromSize w)
    pretty (SFloat FFloat _) = text "float"
    pretty (SFloat FDouble _) = text "double"
    pretty (SFloat FLongDouble _) = text "long double"
    pretty (SChar signedness _) = pretty signedness <+> text "char"
    pretty (SPointerTo t _) = pretty t <+> text "*"
    pretty (SArray t Nothing _) = pretty t <> text "[]"
    pretty (SArray t (Just e) _) = pretty t <> brackets (pretty e)
    pretty (SComposite i _) = pretty i
    pretty (SEnum i _) = pretty i
    pretty (Typedef s _ _) = text s
    pretty (SBuiltinType n _) = text n
    pretty (SFunctionPointer t vs _) = parens (pretty t) <> parens (hsep $ punctuate comma (pretty <$> vs))
    -- pretty x = text ("undefined for " ++ show x)
  
  instance Pretty CompositeType where
    pretty Struct = text "struct"
    pretty Union = text "union"
    
  instance Pretty Attribute where
    pretty Auto = text "auto"
    pretty Const = text "const"
    pretty Extern = text "extern"
    pretty Register = text "register"
    pretty Restrict = text "restrict"
    pretty Static = text "static"
    pretty Volatile = text "volatile"
    pretty (Custom es) = text "__attribute__" <> parens (parens (hsep $ punctuate comma (pretty <$> es)))
  
  instance Pretty CompositeInfo where
    pretty (CompositeInfo t (Just n) []) = pretty t <+> text n
    pretty (CompositeInfo t (Just n) fields) = 
      pretty t <+> text n $+$ braces (vcat $ pretty <$> fields)
    pretty (CompositeInfo t Nothing fields) =
      pretty t $+$ braces (vcat $ pretty <$> fields)
      
  instance Pretty Enumeration where
    pretty (Enumeration n (Just e)) = pretty n <+> equals <+> pretty e
    pretty (Enumeration n Nothing)  = pretty n
      
  instance Pretty EnumerationInfo where 
    pretty (EnumerationInfo n vals) =
      text "enum" <+> text n $+$ braces values where
        values = vcat $ (pretty <$> vals)
  
  instance Pretty SVariable where
    pretty (Variable n (SPointerTo (SComposite (CompositeInfo t n' []) []) []) Nothing) = 
      pretty t <+> pretty n' <+> star <> pretty n
    -- stupid C and its stupid decision to put array sizes after the variable name
    pretty (Variable n (SFunctionPointer rt params _) _) = pretty rt <+> parens (star <> text n) <> parens (hsep $ punctuate comma (pretty <$> params)) <> semicolon
    pretty (Variable n (SArray t size _) _) = pretty t <+> pretty n <> brackets (pretty size)
    pretty (Variable n t Nothing) = pretty t <+> pretty n
    pretty (Variable n t (Just e)) = pretty t <+> pretty n <+> equals <+> pretty e

  instance Pretty SParameter where
    pretty (SParameter Nothing t) = pretty t
    pretty (SParameter (Just n) t) = pretty t <+> text n
  
  instance Pretty SField where
    pretty (SField n (SPointerTo (SComposite (CompositeInfo t n' []) []) []) Nothing) = 
      pretty t <+> pretty n' <+> star <> pretty n <> semicolon
    -- function pointer syntax is the devil, and when I say the devil, I actually mean
    -- Satan. You know, the guy who lives in Hell.
    pretty (SField n (SFunctionPointer rt params _) _) = pretty rt <+> parens (star <> pretty n) <> parens (hsep $ punctuate comma (pretty <$> params)) <> semicolon
    -- stupid C and its stupid decision to put array sizes after the variable name
    pretty (SField n (SArray t size _) _) = pretty t <+> pretty n <> brackets (pretty size) <> semicolon
    pretty (SField n t Nothing) = pretty t <+> pretty n <> semicolon
    pretty (SField n t (Just i)) = pretty t <+> pretty n <> colon <+> pretty i <> semicolon

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
    
  instance Pretty CLiteral where
    pretty (CInteger i) = textS i
    pretty (CChar c) = textS c
    pretty (CFloat f) = textS f
    pretty (CString s) = doubleQuotes $ text s

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
    
  instance Pretty SLocal where
    pretty (LStatement s) = pretty s <> semicolon
    pretty (LDeclaration d) = pretty d <> semicolon
    
  instance Pretty SGlobal where
    pretty (GFunction g) = pretty g
    pretty (GVariable v) = pretty v <> semicolon
    pretty (GTypedef n (SArray t size _)) = text "typedef" <+> pretty t <+> pretty n <> brackets (pretty size) <> semicolon
    pretty (GTypedef n t) = text "typedef" <+> pretty t <+> pretty n <> semicolon
    pretty (GComposite i) = pretty i <> semicolon
    pretty (GFunctionPrototype t n p _) = pretty t <+> pretty n <> parens (hcat $ punctuate comma (pretty <$> p)) <> semicolon
    
  instance Pretty Program where
    pretty a = vcat $ pretty <$> a

  