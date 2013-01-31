{-# LANGUAGE TemplateHaskell #-}

module Semantics.C.QuasiQuote 
  where
  
  import Language.C99
  import Language.Pony
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax
  
  c99 :: QuasiQuoter
  c99 = QuasiQuoter { quoteExp = csplice 'preprocessedC }
  
  expr :: QuasiQuoter
  expr = QuasiQuoter { quoteExp = csplice 'expression }
  
  --| Invoke as $(csplice 'whateverparseryouwanttouse "str")
  csplice :: Name -> 
  csplice parser str = [| convert $ parseUnsafe $(varE parser) $(liftString str) |]
  