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
    deriving (Eq, Typeable, Data)
    
  instance Show CLiteral where
    show (CInteger i) = show i
    show (CChar c) = show c
    show (CFloat f) = show f
    show (CString s) = s
  