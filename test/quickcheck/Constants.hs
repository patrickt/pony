import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Language.C.Parser
import Language.C.AST
import Language.C.Expressions
import Test.QuickCheck

parseConstant = runParser constantExpression mkInternals "test data"

tests :: [Test]
tests = [ testProperty "decimalIntegers" prop_decimalIntegers
        , testProperty "characters" prop_characters
        , testProperty "floatingPoint" prop_floatingPoint
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

main = defaultMain tests
  