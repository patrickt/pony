module Semantics.C.QuasiQuote 
  ( c99
  , expr
  , csplice
  , here)
  where
  
  import Language.C99
  import Semantics.C.Reifiable
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax
  
  qq = QuasiQuoter { quoteExp = error "fill me in"
                   , quotePat = error "no quasiquoting for patterns"
                   , quoteType = error "no quasiquoting for types"
                   , quoteDec = error "no quasiquoting for declarations" }
  
  here :: QuasiQuoter
  here = qq { quoteExp = stringE }
  
  c99 :: QuasiQuoter
  c99 = qq { quoteExp = csplice 'preprocessedC }
  
  expr :: QuasiQuoter
  expr = qq { quoteExp = \str -> [| parseUnsafe expression $(liftString str) |] }
  
  csplice parser str = [| parseUnsafe $(varE parser) $(liftString str) |]
  