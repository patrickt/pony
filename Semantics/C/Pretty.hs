{-# LANGUAGE FlexibleInstances #-}

module Semantics.C.Pretty
  ( module Text.PrettyPrint.HughesPJ
  , semicolon
  , question
  , textS
  , Pretty (pretty)
  )
  where
  
  import Text.PrettyPrint.HughesPJ
  
  textS :: (Show a) => a -> Doc
  textS = text . show
  
  class Pretty a where
    pretty :: a -> Doc
    
  instance Pretty [Char] where
    pretty = text
  
  instance Pretty Int where
    pretty = textS
    
  semicolon = text ";"
  question = text "?"