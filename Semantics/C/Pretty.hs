{-# LANGUAGE FlexibleInstances #-}

module Semantics.C.Pretty
  ( module Text.PrettyPrint.HughesPJ
  , Pretty (pretty)
  )
  where
  
  import Text.PrettyPrint.HughesPJ
  
  class Pretty a where
    pretty :: a -> Doc
    
  instance Pretty [Char] where
    pretty = text
  
  instance Pretty Int where
    pretty = text . show