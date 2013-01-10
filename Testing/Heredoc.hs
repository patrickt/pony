module Testing.Heredoc (here) where
  
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  
  here = QuasiQuoter { quoteExp = stringE }
  