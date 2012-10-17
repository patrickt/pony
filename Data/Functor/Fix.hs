module Data.Functor.Fix 
  ( module Data.Generics.Fixplate
  , Fix
  , tie
  , out
  , µ ) 
  where
    
  import Data.Generics.Fixplate
  
  type Fix = Mu -- I don't know whether I like calling it Fix or Mu. We will find out.
  tie = Fix
  out = unFix
  µ = unFix
    
  