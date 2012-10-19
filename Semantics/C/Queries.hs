module Semantics.C.Queries 
  where
  
  import Prelude hiding (pred)
  import Data.Functor.Fix
  import Semantics.C.ASG
  import qualified Data.Foldable as F
  import Control.Applicative
  import Debug.Trace
  
  traceIt x = traceShow x x
  
  deepElem, deepElem' :: FSem -> FSem -> Bool
  deepElem needle haystack = para' phi haystack where
    phi conc repr = conc == needle || F.or repr
    
  deepElem' needle haystack = elem needle $ universe haystack 
    
  deepNotElem :: FSem -> FSem -> Bool
  deepNotElem a b = not $ deepElem a b
  
  deepFind :: (FSem -> Bool) -> FSem -> (Maybe FSem)
  deepFind pred haystack = para' phi haystack where
    phi fixed rest = if pred fixed then Just fixed else Nothing <|> F.msum rest
  
  deepFind' :: (FSem -> Bool) -> FSem -> (Maybe FSem)
  deepFind' pred f = F.find pred $ universe f
  
  deepAny :: (FSem -> Bool) -> FSem -> Bool
  deepAny pred haystack = para' phi haystack where
    phi fixed rest = pred fixed || F.or rest
    
  deepAll :: (FSem -> Bool) -> FSem -> Bool
  deepAll pred haystack = para' phi haystack where
    phi fixed rest = pred fixed && F.and rest
  
  contents :: FSem -> [FSem]
  contents = universe
  
  
  
  -- deepFind' pred haystack = F.find pred $ universe haystack
  

