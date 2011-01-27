module Testing.QuickCheck.Constants
  (tests)
  where 
    
  import Data.List (intersperse)
  import Test.Framework (Test, defaultMain)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Expressions
  import Test.QuickCheck

  parseConstant = runParser constantExpression mkInternals "test data"

  tests :: [Test]
  tests = [ testProperty "decimal integers" prop_decimalIntegers
          , testProperty "characters" prop_characters
          , testProperty "floating-point literals" prop_floatingPoint
          , testProperty "string literals" prop_strings
          , testProperty "space-separated string literals" prop_spaceSeparatedStrings
          ]

  prop_decimalIntegers :: Integer -> Bool
  prop_decimalIntegers n = case (parseConstant (show n)) of
      (Right (Constant (CInteger i)))               -> (i == n)
      (Right (UnaryOp "-" (Constant (CInteger i)))) -> (i == negate n)
      (Left _)                                      -> False
    
  prop_floatingPoint :: Double -> Bool
  prop_floatingPoint f = case (parseConstant (show f)) of
    (Right (Constant (CFloat f')))               -> (f == f')
    (Right (UnaryOp "-" (Constant (CFloat f')))) -> (f == negate f')
    (Left _)                                     -> False

  prop_characters :: Char -> Bool
  prop_characters c = case (parseConstant (show c)) of
    (Right (Constant (CChar c'))) -> (c == c')
    (Left _) -> False    

  prop_strings :: String -> Bool
  prop_strings s = case (parseConstant (show s)) of
    (Right (Constant (CString s'))) -> (s == s')
    (Left _) -> False
  
  prop_spaceSeparatedStrings :: [String] -> Bool
  prop_spaceSeparatedStrings s = case (parseConstant separated) of
    (Right (Constant (CString s'))) -> (concat s == s')
    (Left _) -> False
    where separated = unwords $ map show s