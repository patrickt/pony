{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ViewPatterns, RankNTypes, NamedFieldPuns, RecordWildCards #-}

module Semantics.C.Pretty 
  ( PrettyAlg (..)
  , prettyPrint
  )
  where
  
  import Control.Applicative hiding (Const)
  import Data.Fixed
  import Data.Functor.Fix
  import Language.Pony.MachineSizes
  import Semantics.C.ASG hiding (group)
  import Text.Pretty
  
  isEmpty x = show x == ""
  infixr 5 <?+>
  a <?+> r = if isEmpty a then r else a <+> r
  
  prettyPrint :: (PrettyAlg f) => Mu f -> Doc e
  prettyPrint = para' evalPretty
  
  class (Functor f) => PrettyAlg f where
    evalPretty :: Mu f -> f (Doc e) -> Doc e
  
  instance PrettyAlg Sem where
    evalPretty _ (Name n)  = text n
    
    evalPretty _ Enum      = "enum"
    evalPretty _ Signed    = empty
    evalPretty _ (Size s)  = pretty s
    evalPretty _ Struct    = "struct"
    evalPretty _ Union     = "union"
    evalPretty _ Unsigned  = "unsigned"
    evalPretty _ Variadic  = "..."
    
    evalPretty _ ShortM    = "short"
    evalPretty _ LongM     = "long"
    evalPretty _ VeryLongM = "int128_t"
    
    evalPretty _ (Function {ftype, fname, fargs, fbody}) = 
      ftype <+> fname <> fargs <+> 
        "{" `above` 
          indent 2 fbody 
            `above` "}"

    evalPretty _ VoidT                   = "void"    
    evalPretty _ FloatT                  = "float"
    evalPretty _ DoubleT                 = "double"
    
    evalPretty (µ -> IntT (µ -> Unsigned) (µ -> MultipartT [Fix VeryLongM])) _   = "__uint128_t"
    evalPretty (µ -> IntT (µ -> Signed)   (µ -> MultipartT [Fix VeryLongM])) _   = "__int128_t"
    evalPretty _ (IntT { isign, ibase }) = isign <?+> ibase <?+> "int"
    
    evalPretty _ (MultipartT a)          = hsep a
    evalPretty _ (CharT sign)            = sign <?+> "char"
    evalPretty _ (PointerToT a)          = a <+> "*"
    evalPretty _ (ArrayT t size)         = t <> brackets size
    evalPretty _ (FunctionPointerT _ b)  = b
    evalPretty _ (TypedefT n)            = n
    
    evalPretty (µ -> (CompositeT (µ -> CompositeInfo typ name _))) _ = prettyPrint typ <+> prettyPrint name
    
    evalPretty (µ -> Variable   (µ -> (FunctionPointerT ftype args)) _ (µ -> Empty)) (Variable _ name _) = 
      prettyPrint ftype <+> parens ("*" <> name) <> prettyPrint args
    
    evalPretty (out -> Variable (µ -> (FunctionPointerT ftype args)) _ val) (Variable _ name _) = 
      prettyPrint ftype <+> parens ("*" <> name) <> prettyPrint args <+> equals <+> prettyPrint val

    evalPretty (µ -> Variable (µ -> a@(ArrayT _ _))  _ (µ -> Empty)) (Variable _ name val) = foundationType (Fix a) <+> name <> foldArrays a
    evalPretty (µ -> Variable (µ -> a@(ArrayT _ _))  _ _) (Variable _ name val) = foundationType (Fix a) <+> name <> foldArrays a <+> equals <+> val

    evalPretty (µ -> Variable _ _ (µ -> Empty)) (Variable t n _)   = t <+> n
    evalPretty _                                (Variable t n val) = t <+> n <+> equals <+> val
    
    -- there's a BIG bug here where case statements don't "keep" the things they own
    -- statements
    evalPretty _ Break                = "break"
    evalPretty _ (Case a b)           = "case" <+> a <> colon </> hsep b
    evalPretty _ (Compound sts)       = "{" `above` indent 2 (vcat sts) `above` "}" 
    evalPretty _ (Default sts)        = "default:"  -- TODO: figure out what to do about default statements and shit
    evalPretty _ (DoWhile a b)        = "do" <+> a <+> "while" <+> parens b
    evalPretty _ (Return a)           = "return" <+> a
    evalPretty _ (Goto s)             = "goto" <+> s
    evalPretty _ (IfThen c s)         = "if" <> parens c <> s
    evalPretty _ (IfThenElse c s alt) = "if" <+> parens c <+> s <+> "else" <+> parens s <+> alt
    evalPretty _ (Labeled l s)        = l <> ":" <> s
    evalPretty _ (While c a)          = "while" <+> parens c <+> a
    
    -- literals
    evalPretty _ (CInt t) = pretty t
    evalPretty _ (CStr s) = dquotes $ text s
    evalPretty _ (CFloat s) = text $ show ((fromRational $ toRational s) :: Double) -- shenanigans to prevent trailing zeroes.
    evalPretty _ (CChar c) = text $ show c
    
    -- expressions
    evalPretty _ (Cast t v)           = parens t <> v
    evalPretty _ (Unary op a) = op <> a
    evalPretty _ (Binary a op b) = a <+> op <+> b
    evalPretty _ (Ternary a b c) = a <> "?" <> b <> colon <> c
    evalPretty _ (Paren a) = parens a
    evalPretty _ (FunCall a bs) = a <> parens (sep $ punctuate comma bs)
    evalPretty _ (Brackets a b) = a <> brackets b
    
    evalPretty (out -> (Attributed _ (out -> PointerToT _))) (Attributed as t) = t <+> hsep as
    evalPretty _ (Attributed as t) = hsep as <+> t
    
    evalPretty _ Auto     = "auto"
    evalPretty _ Const    = "const"
    evalPretty _ Extern   = "extern"
    evalPretty _ Inline   = "inline"
    evalPretty _ Register = "register"
    evalPretty _ Restrict = "restrict"
    evalPretty _ Static   = "static"
    evalPretty _ Volatile = "volatile"
    
    evalPretty _ (Enumeration a b) = a <+> semiBraces b
    evalPretty _ (BuiltinT b) = b
    
    evalPretty (out -> Sized _ (out -> Empty)) (Sized t _) = t
    evalPretty _                        (Sized t s) = t <> ":" <> s
    
    evalPretty _ (Prototype { pname, ptype, pargs }) = ptype <+> pname <> pargs
    
    evalPretty _ (Program p) = vcat [ s <> semi | s <- p  ]
    evalPretty _ (Arguments t) = parens $ hsep $ punctuate comma t
    evalPretty _ (List t) = braces $ hsep $ punctuate comma t
    evalPretty _ (Group ts) = vcat [t <> semi | t <- ts]
    
    evalPretty _ Empty = empty 
    
    -- Anonymous structures and unions appear before the type they are being declared to.
    evalPretty (µ -> (Typedef _ (µ -> CompositeInfo { cname = (Fix Empty) }))) (Typedef name typ) = "typedef" <+> typ <+> name
      
    evalPretty _ (Typedef name typ) = "typedef" <+> name <+> typ 
    
    -- Don't print any fields if they aren't included.
    -- Should we rewrite (Group []) to be Empty?
    evalPretty (µ -> CompositeInfo _ _ (µ -> Group [])) (CompositeInfo { ckind, cname, .. }) = ckind <+> cname 
    evalPretty (µ -> CompositeInfo _ _ (µ -> Empty)) (CompositeInfo { ckind, cname, .. }) = ckind <+> cname 
    -- Don't make space for a name if it's an anonymous composite
    evalPretty (µ -> CompositeInfo { cname = (Fix Empty) }) (CompositeInfo { ckind, cfields })
      = ckind <+> "{" `above` indent 2 cfields `above` "}"
    
    evalPretty _ (CompositeInfo { ckind, cname, cfields }) = ckind <+> cname <+> "{" `above` indent 2 cfields `above` "}"
    
    evalPretty _ x = error $ "not defined for " ++ show x
  
  foldArrays :: Sem (Mu Sem) -> Doc e
  foldArrays (ArrayT a@(Fix (ArrayT _ _)) size) = foldArrays (out a) <> brackets (prettyPrint size)
  foldArrays (ArrayT _ size) = brackets $ prettyPrint size
  foldArrays t = error "foldArrays called improper type"
  
  foundationType :: Mu Sem -> Doc e
  foundationType (Fix (ArrayT t _)) = foundationType t
  foundationType x = prettyPrint x
  
  
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
  
