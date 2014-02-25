module Testing.Examples where
  
  import Language.Pony.Overture
  import Control.Lens
  import Data.Coproduct
  import Language.C11.Syntax
  import Data.Comp.Sum
  
  -- nameGetter :: (HasOldName a, a :<: f) => Prism' (Term f) ByteString
  -- nameGetter = prism' (view name) project
  
  -- (Decl :+: CType :+: Function :+: Attribute :+: Ident)
  
  -- forwardPrintf :: (Decl :<: f, CType :<: f, CFunction :<: f, Attribute :<: f, Ident :<: f) => Term f
  
  type FunctionSig = Decl :+: CType :+: Function :+: Attribute :+: Ident
  
  forwardPrintf :: Term FunctionSig
  forwardPrintf = iForwardDecl s where
    s = iStatic f
    f = iFunction iCVoid "printf" a t
    a = [ (iRestrict $ iConst iCChar), iVariadic ]
    t = [ iAsmName "printf_", iCAttribute [iIdent "whatever"] ]
  
  
  