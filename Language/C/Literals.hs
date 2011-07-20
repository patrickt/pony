{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.Literals 
  ( CLiteral (..), CStringLiteral (..) )
  where
  
  import Data.Generics
  
  -- | C literals.
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString CStringLiteral
    deriving (Eq, Typeable, Data, Show)
  
  data CStringLiteral = CStringLiteral String
    deriving (Show, Eq, Typeable, Data)
  