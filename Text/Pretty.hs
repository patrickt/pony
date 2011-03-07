{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.Pretty
  ( module Text.PrettyPrint.HughesPJ
  , semicolon
  , question
  , textS
  , Pretty (pretty)
  )
  where
  
  import Text.PrettyPrint.HughesPJ hiding (char, int, integer, float, double)
  
  textS :: (Show a) => a -> Doc
  textS = text . show
  
  class Pretty a where
    pretty :: a -> Doc
    
  instance Pretty String where
    pretty = text
  
  instance Pretty Int where
    pretty = textS
    
  semicolon = text ";"
  question = text "?"
