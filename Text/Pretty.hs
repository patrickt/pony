{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.Pretty
  ( module Text.PrettyPrint.HughesPJ
  , semicolon
  , question
  , star
  , textS
  , hsep'
  , parens'
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
  
  instance (Pretty a) => Pretty (Maybe a) where
    pretty Nothing = empty
    pretty (Just a) = pretty a
  
  hsep' :: (Pretty a) => [a] -> Doc
  hsep' xs = hsep (pretty `map` xs)
  
  parens' :: (Pretty a) => a -> Doc
  parens' = parens . pretty
  
  semicolon, question, star :: Doc  
  semicolon = text ";"
  question = text "?"
  star = text "*"
  