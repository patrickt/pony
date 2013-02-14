{-# LANGUAGE ViewPatterns #-}

module Main where
  
  import Language.Pony
  import Language.C99.Literals
  
  op :: String -> Fix Sem -> Fix Sem -> Fix Sem
  op n a b = tie $ Binary a (name' n) b
  
  infixl 7 .>>.
  (.>>.) = op ">>"
  
  int_ = tie $ IntT (tie Signed) nil'
  
  uint_ = tie $ IntT (tie Unsigned) nil'
  
  paren x = tie $ Paren x
  
  cast :: Fix Sem -> Fix Sem -> Fix Sem
  cast typ arg = tie $ Cast typ arg
  
  -- bug: need to evaluate the arguments here...
  convertLogicalShift :: Fix Sem -> Sem (Fix Sem)
  convertLogicalShift (µ -> (Binary l (µ -> Name ">>>") r)) = out $ cast int_ $ paren $ cast uint_ $ l .>>. r
  convertLogicalShift other = out other
  
  main :: IO ()
  main = run $ def 
    { bitwiseOperators = [">>>"]
    , anamorphisms = [convertLogicalShift]
    }