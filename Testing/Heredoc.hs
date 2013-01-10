module Testing.Heredoc (here) where
  
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import qualified Data.ByteString.Char8 as B
  
  here = QuasiQuoter { quoteExp = stringE }
  