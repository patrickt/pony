module Testing.Examples where
  
  import Language.Pony.Overture
  import Control.Lens
  import Data.Coproduct
  import Language.C11.Syntax
  
  -- nameGetter :: (HasName a, a :<: f) => Prism' (Term f) ByteString
  -- nameGetter = prism' (view name) project
  
  forwardPrintf :: Term (Decl :+: CType :+: Function :+: Attribute :+: Ident)
  forwardPrintf = iForwardDecl s where
    s = iStatic f
    f = iFunction iCVoid "printf" a t
    a = [ (iRestrict $ iConst iCChar), iVariadic ]
    t = [ iAsmName "printf_", iCAttribute [iIdent "whatever"] ]
  
  
  