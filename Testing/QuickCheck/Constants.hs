{-# LANGUAGE ScopedTypeVariables #-}

module Testing.QuickCheck.Constants
  ( tests )
  where 
  
  import Data.Char
  import Test.Framework (Test)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.Framework.TH
  import Test.QuickCheck
  import Language.Pony
  import Data.Default
  import Data.ByteString.Char8 (pack)
  import Text.PrettyPrint.Free hiding ((<>))
  
  bshow :: (Show a) => a -> ByteString
  bshow = pack . show
  
    
  roundTrip :: ByteString -> Bool
  roundTrip code = case runParser constantExpression def "test data" code of
    (Left x) -> error $ show x
    (Right c) -> if pretty code == prettyPrint c then True else error $ show (c, code)
  
  tests :: [Test]
  tests = [$(testGroupGenerator)]
    
  prop_decimalIntegers :: Integer -> Bool
  prop_decimalIntegers = roundTrip . bshow
  -- 
  prop_floatingPoint :: Double -> Bool
  prop_floatingPoint = roundTrip . bshow
  
  prop_characters :: Char -> Bool
  prop_characters = roundTrip . bshow
  
  newtype QuotedString = QuotedString { unQuoted :: String } deriving (Show, Eq)
  
  instance Arbitrary QuotedString where 
    arbitrary = do
      t <- listOf1 $ elements ['A' .. 'Z']
      return $ QuotedString ("\"" <> t <> "\"")
  
  prop_strings :: QuotedString -> Bool
  prop_strings = roundTrip . pack . unQuoted
  
  newtype Identifier = Identifier { unIdent :: String } deriving (Show, Eq)
  instance Arbitrary Identifier where arbitrary = Identifier <$> (listOf1 $ elements (['A'..'Z'] <> ['a'..'z']))
  
  prop_identifiers :: Identifier -> Bool
  prop_identifiers = roundTrip . pack . unIdent
  