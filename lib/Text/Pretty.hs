-- Delete this.

module Text.Pretty
( module Text.PrettyPrint.Free )
  where
    
    import Text.PrettyPrint.Free
    
    -- specious hack to save me some effort in the pretty-printer, I'm sorry
    instance Eq (Doc e) where
      a == b = show a == show b
