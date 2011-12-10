module Language.Pony.Prelude 
  ( module Data.Monoid
  , (+>)
  , (<+)
  ) where
  
  import Data.Monoid
  
  infixl 4 +>
  infixl 4 <+
  (+>), (<+) :: Monoid a => a -> a -> a
  (+>) = mappend
  (<+) = flip mappend
  
  