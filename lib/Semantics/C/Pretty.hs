module Semantics.C.Pretty 
  ( PrettyAlg (..)
  , prettyPrint
  )
  where
  
  import Control.Applicative hiding (Const)
  import Data.Fixed
  import Data.Functor.Fix
  import Language.Pony.MachineSizes
  import Semantics.C.ASG
  import Text.PrettyPrint.Free
  
  isEmpty x = show x == ""
  infixr 5 <?+>
  a <?+> r = if isEmpty a then r else a <+> r
  
  prettyPrint :: (PrettyAlg f) => Mu f -> Doc e
  prettyPrint = para' evalPretty
  
  class (Functor f) => PrettyAlg f where
    evalPretty :: Mu f -> f (Doc e) -> Doc e
  
  instance PrettyAlg Sem
  
  {-
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
    evalPretty _ (CommaSep a b)    = a <> comma <+> b
    evalPretty _ (Cast t v)      = (sep $ (parens <$> t)) <> v
    evalPretty (µ -> Unary (µ -> Name "sizeof") _) (Unary _ it) = "sizeof" <> parens it
    evalPretty _ (Unary op a)    = op <> a
    evalPretty _ (Binary a op b) = a <+> op <+> b
    evalPretty _ (Ternary a b c) = a <> "?" <> b <> colon <> c
    evalPretty _ (Paren a)       = parens a
    evalPretty _ (FunCall a bs)  = a <> parens (sep $ punctuate comma bs)
    evalPretty _ (Brackets a b)  = a <> brackets b
    
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
  
  -}
  
  foldArrays :: Sem (Mu Sem) -> Doc e
  foldArrays (ArrayT a@(Fix (ArrayT _ _)) size) = foldArrays (out a) <> brackets (prettyPrint size)
  foldArrays (ArrayT _ size) = brackets $ prettyPrint size
  foldArrays t = error "foldArrays called improper type"
  
  foundationType :: Mu Sem -> Doc e
  foundationType (Fix (ArrayT t _)) = foundationType t
  foundationType x = prettyPrint x
  