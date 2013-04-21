{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where
  
  import Language.Pony
    
  infixl 8 .>>.
  a .>>. b = binary' a ">>" b
  
  -- bug: need to evaluate the arguments here...
  convertLogicalShift :: CSyn -> C99 CSyn
  convertLogicalShift (µ -> (Binary l (µ -> Name ">>>") r)) = µ $ cast' int' $ paren' $ cast' (unsigned' int') $ l .>>. r
  convertLogicalShift other = out other
  
  main :: IO ()
  main = run $ def 
    { binaryOperators = defaultOperators ++ [ cautiousBinaryOp ">>>" 8 ]
    , anamorphisms = [ convertLogicalShift ]
    }