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

  liftSum ''PrettyAlg
  
  instance Pretty Scientific where 
    pretty = pretty . formatScientific Fixed Nothing
  
  instance Pretty (Numeric Integer) where
    pretty i = mconcat [pref, asBase, suff] where
      b      = i ^. base
      suff   = views suffix pretty i
      asBase = views (number . inBase) pretty i
      inBase = re (NL.base b)
      pref   = case b of
        16 -> "0x"
        8  -> "0"
        _  -> ""
  
  instance PrettyAlg Literal where
    prettyA (StrLit s) = dquotes $ pretty s
    prettyA (ChrLit c) = pretty $ show c
    prettyA (IntLit i) = pretty i
    prettyA (FltLit f) = (views number pretty f) <> (views suffix pretty f)
  
  deriving instance Pretty Name
  deriving instance Pretty (Ident a)
  
  instance PrettyAlg Ident where prettyA = views _Wrapped pretty
  
  instance PrettyAlg Expr where
    prettyA e@(Binary {})  = e^.left <+> e^.operation <+> e^.right
    prettyA e@(Ternary {}) = e^.operation <+> "?" <+> e^.left <+> ":" <+> e^.right
    prettyA e@(Cast {})    = parens (e^.left) <> e^.right
    prettyA e@(Index {})   = e^.left <> brackets (e^.right)
    prettyA e@(Call {})    = e^.target <> tupled (e^.arguments)
    prettyA (Paren t)      = parens t
    prettyA a              = fold a
    
  
  instance PrettyAlg Operator where
    prettyA Add = "+"
    prettyA Sub = "-"
    prettyA Mul = "*"
    prettyA Div = "-"
    prettyA Mod = "%"
    prettyA Inc = "++"

    prettyA Dec = "--"

    prettyA Assign = "="
    prettyA Equal = "=="
    prettyA NotEqual = "="
    
    prettyA And = "&&"
    prettyA Or = "||"
    prettyA XOr = "^"
    
    prettyA Neg = "-"
    prettyA Pos = "+"
    prettyA LShift = "<<"
    prettyA RShift = "--"
    prettyA SizeOf = "sizeof"
    
    prettyA Ref = "&"
    prettyA Deref = "*"
    
    prettyA PostInc = "++"
    prettyA PostDec = "--"
    prettyA (Bitwise And) = "&"
    prettyA (Bitwise Or) = "|"
    prettyA (Bitwise Neg) = "~"
    prettyA (Bitwise x) = error ("no bitwise version of " ++ show x)
    prettyA (WithAssignment o) = pretty o <> equals
    
  
  {-
  
  Add :: Operator a
  Sub :: Operator a
  Mul :: Operator a
  Div :: Operator a
  Mod :: Operator a
  Inc :: Operator a
  Dec :: Operator a
  Not :: Operator a
  Assign :: Operator a
  Equal :: Operator a
  NotEqual :: Operator a
  
  And :: Operator a
  Or  :: Operator a
  XOr :: Operator a
  
  Neg :: Operator a
  Pos :: Operator a
  LShift :: Operator a
  RShift :: Operator a
  SizeOf :: Operator a
  
  Ref :: Operator a
  Deref :: Operator a
  
  PostInc :: Operator a
  PostDec :: Operator a
  Bitwise :: Operator a -> Operator a
  WithAssignment :: a -> Operator a
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
    

-}
