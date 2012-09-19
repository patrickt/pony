{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}

module Text.Pretty
  ( module Text.PrettyPrint.HughesPJ
  , semicolon
  , question
  , star
  , dot
  , arrow
  , textS
  , hsep'
  , parens'
  , commaSep
  , (<:>)
  , Pretty (pretty)
  , IsString (..)
  )
  where
  
  import GHC.Exts ( IsString(..) )
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
  
  commaSep :: (Pretty a) => [a] -> Doc
  commaSep p = hsep $ punctuate comma (pretty `map` p) 
  
  (<:>) :: Doc -> Doc -> Doc
  a <:> b = a <> text ":" <> b
  
  semicolon, question, star, dot, arrow :: Doc  
  semicolon = text ";"
  question = text "?"
  star = text "*"
  dot = text "."
  arrow = text "->"
  