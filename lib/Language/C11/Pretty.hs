{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE UndecidableInstances, TupleSections, OverloadedStrings #-}

module Language.C11.Pretty where
  
  import Control.Lens hiding (para)
  import Data.Comp.Ops
  import Data.Foldable
  import Data.Scientific
  import Data.Traversable hiding (size)
  import Data.Scientific
  import StringTable.Atom
  import Language.Pony.Overture
  import Language.C11.Syntax
  import Data.String
  import Text.PrettyPrint.Free hiding ((<>))
  import qualified Numeric.Lens as NL
  import Debug.Trace
  import Data.Coproduct hiding (size)
  import qualified Numeric.Lens as N
  
  class (Functor f) => PrettyAlg f where
    prettyA :: Alg f (Doc e)
    
  class (Functor f) => PrettyRAlg f where
    prettyR :: RAlg f (Doc e)
  
  instance PrettyAlg f => PrettyRAlg f where
    prettyR = prettyA . fmap snd
  
  instance (PrettyRAlg f) => Pretty (Term f) where
    pretty = para prettyR
  
  instance (Pretty (f a), Pretty (g a)) => Pretty ((f :+: g) a) where
    pretty = caseF pretty pretty
  
  instance Pretty Scientific where 
    pretty = pretty . formatScientific Fixed Nothing
  
  instance Pretty (Numeric Integer) where
    pretty i = mconcat [pref, asBase, suff] where
      b      = i ^. base
      suff   = views suffix pretty i
      asBase = views inBase pretty i
      inBase = number . re (NL.base b)
      pref = case b of; 16 -> "0x"; 10 -> ""; 8 -> "0"
  
  instance PrettyAlg Literal where
    prettyA (StrLit s) = dquotes $ pretty s
    prettyA (ChrLit c) = pretty $ show c
    prettyA (IntLit i) = pretty i
    prettyA (FltLit f) = (views number pretty f) <> (views suffix pretty f)
    
  
  
  {-
  instance PrettyAlg IntLit where prettyA (IntLit { .. }) = pretty _intValue
  instance PrettyAlg FltLit where prettyA (FltLit { .. }) = text $ show _fltValue
  
  instance PrettyAlg StrLit where prettyA (StrLit s) = dquotes $ pretty s
  instance PrettyAlg ChrLit where prettyA (ChrLit c) = squotes $ char c
  
  instance PrettyAlg Ident where
    prettyA (Ident i) = pretty i
  
  instance PrettyRAlg CType where 
    prettyR CVoid        = "void"
    prettyR CInt         = "int"
    prettyR CFloat       = "float"
    prettyR CDouble      = "double"
    prettyR CChar        = "char"
    prettyR CBool        = "bool"
    prettyR CInt128      = "__int128_t"
    prettyR (CBuiltin a) = pretty (fromAtom a :: ByteString)
    
    prettyR (µf -> Unsigned CInt128) = "__uint128"
    
    prettyR (Const    ((µ -> Pointer _), t)) = t <+> "const"
    prettyR (Volatile ((µ -> Pointer _), t)) = t <+> "volatile"
    prettyR (Restrict ((µ -> Pointer _), t)) = t <+> "restrict"
    
    prettyR (Const    (_, t)) = "const" <+> t
    prettyR (Volatile (_, t)) = "volatile" <+> t
    prettyR (Restrict (_, t)) = "restrict" <+> t
    prettyR (Inline   (_, t)) = "inline" <+> t
    prettyR (Auto     (_, t)) = "auto" <+> t
    prettyR (Extern   (_, t)) = "extern" <+> t
    prettyR (Register (_, t)) = "register" <+> t
    prettyR (Static   (_, t)) = "static" <+> t
    prettyR (Signed   (_, t)) = "static" <+> t
    prettyR (Unsigned (_, t)) = "unsigned" <+> t
    prettyR (Pointer  (_, t)) = t <+> "*"
    prettyR (Short    (_, t)) = "short" <+> t
    prettyR (Long     (_, t)) = "short" <+> t
    prettyR (a@(Array {})) = a^.typ._2 <> brackets (a^.size._2)
    
  -- instace PrettyAlg 
  
  -- instance PrettyAlg CType where
  --   prettyA = para prettyR
  
  instance PrettyAlg Expr where
    prettyA e@(Binary {})  = e^.left <+> pretty (e^.operation) <+> e^.right
    prettyA e@(Ternary {}) = e^.left <+> "?" <+> pretty (e^.condition) <+> ":" <+> e^.right
    prettyA e@(Cast {})    = parens (e^.left) <> e^.right
    prettyA e@(Index {})   = e^.left <> brackets (e^.right)
    prettyA e@(Call {})    = e^.target <> tupled (e^.arguments)
    prettyA (Paren t)      = parens t
    prettyA a              = fold a
-}
