{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.Literals 
  ( CLiteral (..) )
  where
  
  import Data.Generics
  
  -- | C literals.
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat Double
    | CString String
    deriving (Eq, Typeable, Data, Show)