module Language.C99.Pretty 
  where
  
  import Data.Functor.Fix
  import Language.C99.Syntax
  import Language.Pony.Overture
  import Language.C99.Parser
  import Text.PrettyPrint.Free hiding ((<>))
  
  prettyTest :: (PrettyAlg f) => Parser (Mu f) -> ByteString -> IO ()
  prettyTest p s = print $ prettyPrint $ parseUnsafe p s
  
  
  prettyPrint :: (PrettyAlg f) => Mu f -> Doc e
  prettyPrint = para' evalPretty
  
  class (Functor f) => PrettyAlg f where
    evalPretty :: Mu f -> f (Doc e) -> Doc e
  
  printInit :: Fix C99 -> Doc e
  printInit (Fix Empty) = empty
  printInit a = " " <> equals <+> prettyPrint a
  
  instance PrettyAlg C99 where
    evalPretty _ (Name n)  = text n
    
    evalPretty (µ1 -> Unsigned VeryLongT) _ = "__uint128_t"
    evalPretty (µ1 -> Signed VeryLongT) _   = "__int128_t"
    
    evalPretty _ (Signed t)           = "signed" <+> t
    evalPretty _ (Unsigned t)         = "unsigned" <+> t
    evalPretty _ (ShortM t)           = "short" <+> t
    evalPretty _ (LongM t)            = "long" <+> t
    evalPretty _ (Attributed attrs t) = "__attribute__" <> parens (tupled attrs) <+> t
    
    evalPretty _ VoidT        = "void"
    evalPretty _ IntT         = "int"
    evalPretty _ FloatT       = "float"
    evalPretty _ DoubleT      = "double"
    evalPretty _ CharT        = "char"
    evalPretty _ VeryLongT    = "__int128_t"
    evalPretty _ Struct       = "struct"
    evalPretty _ Union        = "union"
    evalPretty _ BoolT        = "_Bool"
    evalPretty _ (BuiltinT t) = t
    evalPretty _ (Typedef {typ})  = typ

    evalPretty (µ1 -> Const (PointerToT _)) (Const t)       = t <+> "const"
    evalPretty (µ1 -> Volatile (PointerToT _)) (Volatile t) = t <+> "volatile"
    evalPretty (µ1 -> Restrict (PointerToT _)) (Restrict t) = t <+> "restrict"
    
    evalPretty _ (PointerToT t) = t <+> "*"
    evalPretty _ (Const t)      = "const" <+> t
    evalPretty _ (Volatile t)   = "volatile" <+> t
    evalPretty _ (Restrict t)   = "restrict" <+> t
    evalPretty _ (Inline f)     = "inline" <+> f
    evalPretty _ (Auto t)       = "auto" <+> t
    evalPretty _ (Extern t)     = "extern" <+> t
    evalPretty _ (Register t)   = "register" <+> t
    evalPretty _ (Static t)     = "static" <+> t

    evalPretty _ (Function {typ, name, args, body}) = typ <+> name <> args <+> body
    
    -- To pretty-print arrays, we break them down into a form that looks like this:
    -- baseTypeAndName[optlen] = initializer;
    -- We recursively print baseTypeAndName (which may itself be an array), then append the brackets and initializer.
    evalPretty (µ -> v@(Variable (µ -> ArrayT { typ, len }) name value)) _ = 
      baseDeclaration <> brackets l <> initializer where
        baseDeclaration = prettyPrint $ tie $ v { typ = typ, value = nil' }
        l = prettyPrint len
        initializer = printInit value
      
    evalPretty (µ1 -> Variable {typ = Function {typ, args}}) (Variable {name}) = prettyPrint typ <+> name <> prettyPrint args
    evalPretty (µ -> Variable {value}) (Variable t n _) = t <+> n <> printInit value
    
    evalPretty _ Break                = "break"
    evalPretty _ (Case a b)           = "case" <+> a <> colon <+> b
    evalPretty _ Continue             = "continue"
    evalPretty _ (Default sts)        = "default:" <+> sts
    evalPretty _ (DoWhile a b)        = "do" <+> a <+> "while" <+> parens b
    evalPretty _ Empty                = empty
    evalPretty _ (For a b c block)    = "for" <> (parens $ cat $ semi `punctuate` [a,b,c]) <+> block
    evalPretty _ (Goto s)             = "goto" <+> s
    evalPretty _ (IfThenElse c s alt) = "if" <+> parens c <+> s <+> "else" <+> parens s <+> alt
    evalPretty _ (Labeled l e)        = l <> colon <+> e
    evalPretty _ (Return a)           = "return" <+> a
    evalPretty _ (While c a)          = "while" <+> parens c <+> a
    
    -- literals
    evalPretty _ (CInt t) = pretty t
    evalPretty _ (CStr s) = dquotes $ text s
    evalPretty _ (CFloat s) = text s
    evalPretty _ (CChar c) = text $ show c
    
    -- expressions
    evalPretty _ (Cast t v)      = parens t <> v
    evalPretty _ (CommaSep a b)  = a <> comma <+> b
    evalPretty _ (Unary op a)    = op <> a
    evalPretty _ (Binary a op b) = a <+> op <+> b
    evalPretty _ (Ternary a b c) = a <> "?" <> b <> colon <> c
    evalPretty _ (Paren a)       = parens a
    evalPretty _ (Call a bs)  = a <> tupled bs
    evalPretty _ (Index a b)  = a <> brackets b
    
    evalPretty _ (Attributed as t) = hsep as <+> t

    evalPretty _ (Enumeration a b) = "enum" <+> a <+> b
    evalPretty _ (Composite {kind, name, fields}) = kind <+> name <+> fields
    evalPretty _ (Program p) = vcat [ s <> semi | s <- p  ]
    evalPretty _ (Group ts) = semiBraces ts
    evalPretty _ (List ts) = cat $ comma `punctuate` ts
    
    evalPretty _ (Arguments ts False) = tupled ts
    evalPretty _ (Arguments ts True) = tupled $ ts ++ ["..."]
    
    evalPretty _ (ForwardDeclaration t) = t
    evalPretty _ (Sized s t) = t <+> colon <+> s
    evalPretty _ (List t) = braces $ hsep $ punctuate comma t
    evalPretty _ (Typedef name typ) = "typedef" <+> name <+> typ 
    
    evalPretty x _ = error $ show x
  
  foldArrays :: Mu C99 -> Doc e
  foldArrays (Fix (ArrayT {typ = a@(Fix (ArrayT _ _)), len})) = foldArrays a <> brackets (prettyPrint len)
  foldArrays (Fix (ArrayT {len})) = brackets $ prettyPrint len
  foldArrays t = error "foldArrays called improper type"
  
  foundationType :: Mu C99 -> Doc e
  foundationType (Fix (ArrayT { typ })) = foundationType typ
  foundationType x = prettyPrint x

  
