module Language.C99.Pretty 
  where
  
  import Data.Functor.Fix
  import Data.Generics.Fixplate.Zipper
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
  
  isPointer :: CSyn -> Bool
  isPointer (µ -> PointerToT _) = True
  isPointer _ = False
  
  commaSep ts = cat $ punctuate ", " ts
  
  -- could make this prettier
  printParens :: Doc e -> Loc C99 -> Doc e -> Maybe (Doc e)
  printParens d t post = do
    let
      pointer = maybe False (isPointer . focus) (moveUp t)
      seed    = if pointer then parens d else d
      in moveDownL t >>= printDecl (seed <> post)
    
  
  printDecl :: Doc e -> Loc C99 -> Maybe (Doc e)
  printDecl d t@(focus -> (Fix (Const (Fix (PointerToT _)))))    = moveDownL t >>= printDecl (" const" <+> d)
  printDecl d t@(focus -> (Fix (Volatile (Fix (PointerToT _))))) = moveDownL t >>= printDecl (" volatile" <+> d)
  printDecl d t@(focus -> (Fix (PointerToT _)))                  = moveDownL t >>= printDecl ("*" <> d)
  printDecl d t@(focus -> Fix (ArrayT _ len))                    = printParens d t (brackets (prettyPrint len))
  printDecl d t@(focus -> Fix (Function { args }))               = printParens d t $ prettyPrint args
  printDecl d (focus -> t)                                       = Just $ prettyPrint t <+> d
  
  printDecl' :: Doc e -> Loc C99 -> Doc e
  printDecl' a b = error ("Error in printing type " ++ show b) `fromMaybe` printDecl a b
  
  
  instance PrettyAlg Maybe where
    evalPretty _ (Just a) = a
    evalPretty _ _ = mempty
  
  instance PrettyAlg C99 where
    evalPretty _ (Name n)  = text n
    
    evalPretty (µ1 -> Unsigned VeryLongT) _ = "__uint128_t"
    evalPretty (µ1 -> Signed VeryLongT) _   = "__int128_t"
    
    evalPretty _ (Signed t)           = "signed" <+> t
    evalPretty _ (Unsigned t)         = "unsigned" <+> t
    evalPretty _ (ShortM t)           = "short" <+> t
    evalPretty _ (LongM t)            = "long" <+> t
    evalPretty _ (Attributed attrs t) = "__attribute__" <> parens (tupled attrs) <+> t
    
    evalPretty _ VoidT           = "void"
    evalPretty _ IntT            = "int"
    evalPretty _ FloatT          = "float"
    evalPretty _ DoubleT         = "double"
    evalPretty _ CharT           = "char"
    evalPretty _ VeryLongT       = "__int128_t"
    evalPretty _ Struct          = "struct"
    evalPretty _ Union           = "union"
    evalPretty _ BoolT           = "_Bool"
    evalPretty _ (BuiltinT t)    = t
    evalPretty _ (Typedef {typ}) = typ
    evalPretty _ (TypeOfT t)     = "typeof" <> parens t

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
    
    evalPretty _ (ArrayT { typ, len }) = typ <> brackets len
    evalPretty (µ2 -> (Function { body = Empty })) _ = ""
    evalPretty _ (Function {typ, name, args, body}) = typ <+> name <> args <+> body
    
    evalPretty (µ -> Variable { typ }) (Variable { name, value = Just val }) = printDecl' name (root typ) <+> "=" <+> val
    evalPretty (µ -> Variable { typ }) (Variable { name, value = Nothing }) = printDecl' name (root typ)
    
    evalPretty _ Break                     = "break;"
    evalPretty _ (Case a b)                = "case" <+> a <> colon <+> b <> semi
    evalPretty _ Continue                  = "continue;"
    evalPretty _ (Default sts)             = "default:" <+> sts
    evalPretty _ (DoWhile a b)             = "do" <+> a <+> "while" <+> parens b <> semi
    evalPretty _ Empty                     = empty
    evalPretty _ (For a b c block)         = "for" <> (parens $ cat $ semi `punctuate` [commaSep a,b,c]) <+> block
    evalPretty _ (Goto s)                  = "goto" <+> s <> semi
    evalPretty _ (IfThenElse c s (Just e)) = "if" <+> parens c <+> s <+> "else" <+> e <> semi
    evalPretty _ (IfThenElse c s _)        = "if" <+> parens c <+> s <> semi
    evalPretty _ (Labeled l s)             = l <> colon <+> s
    evalPretty _ (Return a)                = "return" <+> a <> ";"
    evalPretty _ (While c a)               = "while" <+> parens c <+> a
    evalPretty _ (Switch s b)              = "switch" <+> parens s <+> b
    
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
    evalPretty _ (Ternary a b c) = a <+> "?" <+> b <+> colon <+> c
    evalPretty _ (Paren a)       = parens a
    evalPretty _ (Call a bs)  = a <> tupled bs
    evalPretty _ (Index a b)  = a <> brackets b
    evalPretty _ (Access a access b) = hcat [a, access, b]

    evalPretty _ (Enumeration a b) = "enum" <+> a <+> b
    evalPretty (µ1 -> Composite { fields = Group []}) (Composite {kind, name}) = kind <+> name
    evalPretty _ (Composite {kind, name, fields}) = kind <+> name <+> fields
    evalPretty _ (Program p) = vcat [ s <> semi | s <- p  ]
    evalPretty (µ -> Group nodes) (Group docs) = lbrace `above` groupBody `above` rbrace
      where 
        groupBody = indent 2 $ vcat $ (uncurry go <$> zip nodes docs)
        go (Fix (Variable {})) v = v <> semi
        go node doc = if isStatement (unFix node) then doc else doc <> semi
      
      
    evalPretty _ (List ts) = commaSep ts 
    
    evalPretty _ (Arguments ts False) = parens $ hcat $ punctuate ", " ts
    evalPretty _ (Arguments ts True) = parens $ hcat $ punctuate ", " (ts ++ ["..."])
    
    evalPretty (µ1 -> ForwardDeclaration (Typedef { name, typ })) _ = "typedef" <+> printDecl' (prettyPrint name) (root typ)
    evalPretty _ (ForwardDeclaration t) = t
    evalPretty _ (Sized s t) = t <+> colon <+> s
    evalPretty _ (CommaGroup cs) = braces $ commaSep cs
    evalPretty _ (Assembly { isVolatile, asmText, inRegs = [], outRegs = [] }) =
      "asm" <> volatility <> parens asmText where volatility = if isVolatile then " volatile " else " "
    evalPretty _ (Assembly { isVolatile, asmText, inRegs, outRegs, clobberList }) =
      "asm" <> volatility <> parens (asmText <:> commaSep inRegs <:> commaSep outRegs <:> commaSep clobberList)  where 
        x <:> y = x <> ":" <> y
        volatility = if isVolatile then " volatile " else " "
    
    -- evalPretty x _ = error $ "died in evalPretty " ++ show x
