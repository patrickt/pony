module Testing.QuickCheck.Constants
  (tests)
  where 
  
  import Test.Framework(Test)
  import Test.Framework.Providers.QuickCheck2(testProperty)
  import Language.C99
  import Language.Pony.Overture
  import Data.Default
  import qualified Data.ByteString.Char8 as B
  
  bshow :: (Show a) => a -> ByteString
  bshow = B.pack . show
  
  
  parseConstant :: ByteString -> Either ParseError CExpr
  parseConstant = runParser constantExpression def "test data"

  tests :: [Test]
  tests = [ testProperty "decimal integers" prop_decimalIntegers
          , testProperty "characters" prop_characters
          , testProperty "floating-point literals" prop_floatingPoint
          , testProperty "string literals" prop_strings
          , testProperty "space-separated string literals" prop_spaceSeparatedStrings
          ]

  prop_decimalIntegers :: Integer -> Bool
  prop_decimalIntegers n = case parseConstant (bshow n) of
      (Right (Constant (CInteger i)))               -> i == n
      (Right (UnaryOp "-" (Constant (CInteger i)))) -> i == negate n
      _                                             -> False
  
  prop_floatingPoint :: Double -> Bool
  prop_floatingPoint f = case parseConstant (bshow f) of
    (Right (Constant (CFloat f')))               -> f' == (show f)
    (Right (UnaryOp "-" (Constant (CFloat f')))) -> f' == (tail $ show f)
    _                                            -> False
  
  prop_characters :: Char -> Bool
  prop_characters c = case parseConstant (bshow c) of
    (Right (Constant (CChar c'))) -> c == c'
    _ -> False    

  prop_strings :: String -> Bool
  prop_strings s = case parseConstant (bshow s) of
    (Right (Constant (CString s'))) -> s == s'
    _ -> False
  
  prop_spaceSeparatedStrings :: [String] -> Bool
  prop_spaceSeparatedStrings s = case parseConstant separated of
    (Right (Constant (CString s'))) -> concat s == s'
    _ -> s == []
    where separated = B.pack $ unwords $ map show s