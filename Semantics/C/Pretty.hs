{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Semantics.C.Pretty 
  ( PrettyAlg (..)
  )
  where
  
  import Control.Applicative ((<$>))
  import Data.Monoid hiding ((<>))
  import Semantics.C.ASG
  import Language.Pony.MachineSizes
  import Language.C99.Literals
  import Text.Pretty
  
  class (Functor f) => PrettyAlg f where
    evalPretty :: f Doc -> Doc
  
  instance PrettyAlg Sem where
    evalPretty (Name n) = text n
    evalPretty Unsigned = "unsigned"
    evalPretty Signed   = "signed"
    
    evalPretty (Size t) = "A SIZE LOL"
  
    evalPretty (Function typ name params body) = 
      typ <+> name <> (parens params) <+> "{" $$ (nest 4 $ vcat body) $$ "}"
    
    evalPretty VoidT           = "void"
    evalPretty (IntT _ sign)   = sign <+> "int"
    evalPretty FloatT          = "float"
    evalPretty DoubleT         = "double"
    evalPretty LongDoubleT     = "long double"
    evalPretty (CharT sign)    = sign <+> "char"
    evalPretty (PointerToT a)  = a <> "*"
    -- arrays need to have three parts: their modifier, their size, and their original type
    evalPretty (ArrayT size t) = size <> "[]"
    
    evalPretty (Variable t n Nothing) = t <+> n
    evalPretty (Variable t n (Just init)) = t <+> n <+> equals <+> init
    
    -- statements
    evalPretty Break = "break"
    evalPretty (Compound sts) = braces $ hsep sts 
    -- evalPretty (Default sts) = "default:" $$ hsep sts
    evalPretty (Case a b) = "case" <+> a <> ":" $$ hsep b
    evalPretty (Return Nothing) = "return;"
    evalPretty (Return (Just a)) = "return" <+> a <> semi
    
    -- literals
    evalPretty (CInt t) = textS t
    evalPretty (CStr s) = doubleQuotes $ text s
    
    evalPretty (Attributed as t) = (hsep as) <+> t
    
    evalPretty Auto     = "auto"
    evalPretty Const    = "const"
    evalPretty Extern   = "extern"
    evalPretty Inline   = "inline"
    evalPretty Register = "register"
    evalPretty Restrict = "restrict"
    evalPretty Static   = "static"
    evalPretty Volatile = "volatile"
    
    evalPretty (Declarations t) = hsep $ punctuate comma t
    
    evalPretty Empty = mempty
    
    evalPretty (Program p) = vcat p
    evalPretty (Typedef name typ) = "typedef " <> name <+> typ 
    
    evalPretty x = error $ "not defined for " ++ show x
  
  {-
  
  instance Pretty Function where
    pretty (Function attrs retType name params body isVariadic) = 
      pretty attrs <+> pretty retType <+> pretty name <> parens (parameters <> ellipsis) 
      <+> lbrace 
        $$ bodyContents 
      $$ rbrace
        where parameters = commaSep params
              bodyContents = nest 2 (vcat $ pretty <$> body)
              ellipsis = if isVariadic then ", ..." else empty
  
  instance Pretty Signedness where
    pretty Unsigned = "unsigned"
    pretty Signed = "signed"
  
  instance Pretty AsmOp where
    pretty (AsmOp x Nothing) = pretty x
    pretty (AsmOp x (Just a)) = pretty x <> parens' a
  
  instance Pretty SType where
    pretty (SVoid attrs) = pretty attrs <+> "void"
    pretty (SInt (IntegerFlags s w) attrs) 
        | (w == sizeOfInt128) && (s == Signed)   = pretty attrs <+> "__int128_t"
        | (w == sizeOfInt128) && (s == Unsigned) = pretty attrs <+> "__uint128_t"
        | s == Signed                         = pretty attrs <+> text (intTypeFromSize w)
        | s == Unsigned                       = pretty attrs <+> "unsigned" <+> text (intTypeFromSize w)
    pretty (SFloat FFloat attrs) = pretty attrs <+> "float"
    pretty (SFloat FDouble attrs) = pretty attrs <+> "double"
    pretty (SFloat FLongDouble attrs) = pretty attrs <+> "long double"
    pretty (SChar signedness attrs) = pretty attrs <+> pretty signedness <+> "char"
    pretty (SPointerTo t attrs) = pretty t <+> pretty attrs <+> "*"
    pretty (SArray t Nothing _) = pretty t <> "[]"
    pretty (SArray t (Just e) _) = pretty t <> brackets (pretty e)
    pretty (SComposite i _) = pretty i
    pretty (SEnum i _) = pretty i
    pretty (STypedef s _ _) = text s
    pretty (SBuiltinType n _) = text n
    pretty (SFunctionPointer t vs _) = parens' t <> commaSep vs
    pretty whoops = error ("pretty-print not defined for " ++ show whoops)
  
  instance Pretty CompositeType where
    pretty Struct = "struct"
    pretty Union = "union"
    
  instance Pretty Attribute where
    pretty Auto = "auto"
    pretty Const = "const"
    pretty Extern = "extern"
    pretty Inline = "inline"
    pretty Register = "register"
    pretty Restrict = "restrict"
    pretty Static = "static"
    pretty Volatile = "volatile"
    pretty (Custom es) = "__attribute__" <> parens (parens $ commaSep es)
  
  instance Pretty [Attribute] where
    pretty = hsep'
  
  instance Pretty CompositeInfo where
    pretty (CompositeInfo t (Just n) []) = pretty t <+> text n
    pretty (CompositeInfo t (Just n) fields) = 
      pretty t <+> text n <+> lbrace $$ body $$ rbrace where
        body = nest 2 (vcat $ pretty <$> fields)
    pretty (CompositeInfo t Nothing fields) =
      pretty t $+$ braces (vcat $ pretty <$> fields)
      
  instance Pretty Enumeration where
    pretty (Enumeration n (Just e)) = pretty n <+> equals <+> pretty e
    pretty (Enumeration n Nothing)  = pretty n
      
  instance Pretty EnumerationInfo where 
    pretty (EnumerationInfo n vals) =
      "enum" <+> pretty n <+> braces values where
        values = commaSep vals
  
  instance Pretty Variable where
    pretty (Variable n (SPointerTo (SComposite (CompositeInfo t n' []) []) []) Nothing) = 
      pretty t <+> pretty n' <+> star <> pretty n
    -- stupid C and its stupid decision to put array sizes after the variable name
    pretty (Variable n (SFunctionPointer rt params _) _) = pretty rt <+> parens (star <> text n) <> parens (commaSep params) <> semicolon
    pretty (Variable n (SArray t size _) _) = pretty t <+> pretty n <> brackets (pretty size)
    pretty (Variable n t Nothing) = pretty t <+> pretty n
    pretty (Variable n t (Just e)) = pretty t <+> pretty n <+> equals <+> pretty e

  instance Pretty Parameter where
    pretty (Parameter Nothing (SFunctionPointer rt params _)) = pretty rt <+> parens star <> parens (commaSep params)
    pretty (Parameter Nothing t) = pretty t
    pretty (Parameter (Just n) (SFunctionPointer rt params _)) = pretty rt <+> parens (star <> pretty n) <> parens (commaSep params)
    pretty (Parameter (Just n) (SArray t Nothing _)) = pretty t <+> text n <> "[]"
    pretty (Parameter (Just n) t) = pretty t <+> text n
  
  instance Pretty Field where
    pretty (Field n (SPointerTo (SComposite (CompositeInfo t n' []) []) []) Nothing) = 
      pretty t <+> pretty n' <+> star <> pretty n <> semicolon
    -- function pointer syntax is the devil, and when I say the devil, I actually mean
    -- Satan. You know, the guy who lives in Hell.
    pretty (Field n (SFunctionPointer rt params _) _) = pretty rt <+> parens (star <> pretty n) <> parens (commaSep params) <> semicolon
    -- stupid C and its stupid decision to put array sizes after the variable name
    pretty (Field n (SArray t size _) _) = pretty t <+> pretty n <> brackets (pretty size) <> semicolon
    pretty (Field n t Nothing) = pretty t <+> pretty n <> semicolon
    pretty (Field n t (Just i)) = pretty t <+> pretty n <> colon <+> pretty i <> semicolon

  instance Pretty Statement where
    pretty (Asm True a b c d) = 
      "asm volatile" <> parens (pretty a <:> 
                                commaSep b <:> 
                                commaSep c <:> 
                                commaSep d) where
    pretty (Asm False a b c d) = 
      "asm" <> parens (pretty a <:> 
                       commaSep b <:> 
                       commaSep c <:> 
                       commaSep d) where
    pretty Break = "break;" 
    pretty (Case e s) = "case" <+> pretty e <> colon <+> pretty s
    pretty (Compound b) = lbrace $$ nest 2 (vcat (pretty <$> b)) $$ rbrace
    pretty Continue = "continue;"
    pretty (Default s) = "default:" <+> pretty s <> semicolon
    pretty (DoWhile s e) = "do" <+> pretty s <+> "while" <+> parens' e
    pretty EmptyS = empty
    pretty (ExpressionS s) = pretty s <> semicolon
    pretty (For a b c s) = "for" <> parens contents <+> pretty s where
      contents = pretty a <+> pretty b <> semicolon <+> pretty c
    pretty (GoTo n) = "goto" <+> pretty n <> semicolon
    pretty (IfThen e s) = "if" <+> parens' e <+> pretty s
    pretty (IfThenElse e s s') = "if" <+> parens' e <+> pretty s <+> "else" <+> pretty s'
    pretty (Labeled name _ s) = pretty name <> colon <+> pretty s
    pretty (Return Nothing) = "return;" 
    pretty (Return (Just e)) = "return" <+> pretty e <> semicolon
    pretty (Switch e s) = "switch" <+> parens' e <+> pretty s
    pretty (While e s) = "while" <+> parens' e <+> pretty s
    
  instance Pretty CLiteral where
    pretty (CInteger i) = textS i
    pretty (CChar c) = textS c
    pretty (CFloat f) = textS f
    pretty (CString s) = textS s

  instance Pretty Expression where
    pretty (Literal l) = pretty l
    pretty (Ident n) = text n
    pretty (CStr n) = doubleQuotes $ text n
    pretty (Brackets lhs rhs) = pretty lhs <> brackets (pretty rhs)
    pretty (FunctionCall lhs args) = pretty lhs <> parens (commaSep args)
    pretty (Cast t e) = parens' t <> pretty e
    pretty (Unary "++ post" e) = pretty e <> "++"
    pretty (Unary "-- post" e) = pretty e <> "--"
    -- terrible hack pending workaround
    pretty (Unary "sizeof" e) = "sizeof" <> parens' e
    pretty (Unary n e) = text n <> pretty e
    pretty (Binary lhs op rhs) = pretty lhs <+> text op <+> pretty rhs
    pretty (Ternary a b c) = pretty a <+> question <+> pretty b <+> colon <+> pretty c
    pretty (SizeOfSType t) = "sizeof" <> parens' t
    pretty (Builtin b) = pretty b
    pretty (InitializerList i) = braces $ pretty i
  
  instance Pretty SBuiltin where
    pretty (SVaArg expr ty) = "va_arg" <> parens (pretty expr <> comma <+> pretty ty)
    
  instance Pretty InitList where
    pretty (InitList i) = braces $ hcat $ (r <$> i) where
      r (desigs, initializer) = hcat (pretty <$> desigs) <+> equals <+> pretty initializer
    
  instance Pretty Designator where
    pretty (MemberDesignator n) = dot <> text n
    pretty (ArrayDesignator e) = brackets $ pretty e
    
  instance Pretty Initializer where
    pretty (InitExpression e) = pretty e
    pretty (Composite l) = pretty l
    
  instance Pretty Local where
    pretty (LStatement s) = pretty s
    pretty (LDeclaration d) = pretty d <> semicolon
    
  instance Pretty SGlobal where
    pretty (GFunction g) = pretty g
    pretty (GVariable v) = pretty v <> semicolon
    pretty (GTypedef n (SArray t size _)) = "typedef" <+> pretty t <+> pretty n <> brackets (pretty size) <> semicolon
    pretty (GTypedef n (SFunctionPointer rt params _)) = "typedef" <+> pretty rt <+> name <> parens (commaSep params) <> semicolon where
      name = parens (star <> text n)
    pretty (GTypedef n t) = "typedef" <+> pretty t <+> pretty n <> semicolon
    pretty (GEnumeration i) = pretty i <> semicolon
    pretty (GComposite i) = pretty i <> semicolon
    pretty (GFunctionPrototype t n p False) = pretty t <+> pretty n <> parens (hcat $ punctuate comma (pretty <$> p)) <> semicolon
    pretty (GFunctionPrototype t n p True) = pretty t <+> pretty n <> parens (hcat (punctuate comma (pretty <$> p)) <> ", ...") <> semicolon
    
  instance Pretty Program where
    pretty a = vcat $ pretty <$> a
    
  -}

  
