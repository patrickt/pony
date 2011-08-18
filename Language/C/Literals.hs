{-# LANGUAGE DeriveDataTypeable #-}

module Language.C.Literals 
  ( CLiteral (..) )
  where
  
  import Data.Generics
  
  -- | C literals.
  data CLiteral
    = CInteger Integer
    | CChar Char
    | CFloat String -- | Strings don't lose precision, unlike Doubles.
    | CString String
    deriving (Eq, Typeable, Data, Show)