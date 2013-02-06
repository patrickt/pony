module Semantics.C.QuasiQuote 
  ( module Language.Haskell.TH
  , module Language.Haskell.TH.Quote
  , module Language.Haskell.TH.Syntax
  , c99
  , expr
  , csplice
  , here)
  where
  
  import Language.C99
  import Language.Pony
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax
  
  here = QuasiQuoter { quoteExp = stringE }
  
  c99 :: QuasiQuoter
  c99 = QuasiQuoter { quoteExp = csplice 'preprocessedC }
  
  expr :: QuasiQuoter
  expr = QuasiQuoter { quoteExp = \str -> [| convert $ parseUnsafe expression $(liftString str) |] }
  
  csplice parser str = [| convert $ parseUnsafe $(varE parser) $(liftString str) |]
  