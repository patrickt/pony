{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}

-- Delete this.

module Text.Pretty
( module Text.PrettyPrint.Free )
  where
    
    import Text.PrettyPrint.Free
    
    instance Eq (Doc e) where
      a == b = (show a) == (show b)
