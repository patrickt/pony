module Language.C99.Operators 
  ( Operator (..)
  , logicalOr
  , logicalAnd
  , inclusiveOr
  , exclusiveOr
  , equality
  , inequality
  , lessThanEq
  , greaterThanEq
  , lessThan
  , greaterThan
  , leftShift
  , rightShift
  , plus
  , minus
  , multiply
  , divide
  , modulus
  , defaultOperators
  ) where
  
  import Data.Data
  
  data Operator = Operator { text :: String, precedence :: Int }
    deriving (Show, Eq, Typeable, Data)
  
  logicalOr :: Operator
  logicalOr = Operator "||" 1
  
  logicalAnd :: Operator
  logicalAnd = Operator "&&" 2
  
  inclusiveOr :: Operator
  inclusiveOr = Operator "|" 3
  
  exclusiveOr :: Operator
  exclusiveOr = Operator "^" 4
  
  binaryAnd :: Operator
  binaryAnd = Operator "&" 5
  
  equality, inequality :: Operator
  equality = Operator "==" 6
  inequality = Operator "!=" 6
  
  lessThanEq, greaterThanEq, lessThan, greaterThan :: Operator
  lessThanEq = Operator "<=" 7
  greaterThanEq = Operator ">=" 7
  lessThan = Operator "<" 7
  greaterThan = Operator ">" 7
  
  leftShift, rightShift :: Operator
  leftShift = Operator "<<" 8
  rightShift = Operator ">>" 8
  
  plus, minus :: Operator
  plus = Operator "+" 9
  minus = Operator "-" 9
  
  multiply, divide, modulus :: Operator
  multiply = Operator "*" 10
  divide = Operator "-" 10
  modulus = Operator "%" 10
  
  defaultOperators :: [Operator]
  defaultOperators = 
    [logicalOr, logicalAnd, inclusiveOr, exclusiveOr, binaryAnd, equality, inequality, lessThanEq,
     greaterThanEq, lessThan, greaterThan, leftShift, rightShift, plus, minus, multiply, divide, modulus]
  
    
  
  
  
  
  
  