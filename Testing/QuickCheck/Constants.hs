{-# LANGUAGE ScopedTypeVariables #-}

module Testing.QuickCheck.Constants
  ( constantTestGroup )
  where 
  
  import Test.Framework (Test)
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.Framework.TH
  import Test.QuickCheck
  import Language.Pony
  import Data.Default
  import qualified Data.ByteString.Char8 as B
  import Text.PrettyPrint.Free
  
  bshow :: (Show a) => a -> ByteString
  bshow = B.pack . show
  
  emit :: String -> Doc a
  emit x = para' evalPretty $ convert $ parseUnsafe constantExpression (B.pack x)
  
  instance Eq (Doc a) where
    a == b = (show a) == (show b)
    
  roundTrip :: ByteString -> Bool
  roundTrip b = pretty b == emit (B.unpack b)
  
  constantTestGroup :: Test
  constantTestGroup = $(testGroupGenerator)
  
  
  parseConstant :: ByteString -> Either ParseError CExpr
  parseConstant = runParser constantExpression def "test data"

  prop_decimalIntegers :: Integer -> Bool
  prop_decimalIntegers n = roundTrip (bshow n)
  
  -- prop_floatingPoint :: Double -> Bool
  -- prop_floatingPoint f = roundTrip (bshow f)
  
  prop_characters :: Char -> Bool
  prop_characters c = roundTrip (bshow c)
  
  prop_idents :: Gen Bool 
  prop_idents = (listOf1 $ elements ("abcdefghijklmnopqrstuvwxyz" :: String)) >>= \(c :: String) -> return $ pretty c == emit c
  
  -- 
  -- prop_strings :: String -> Bool
  -- prop_strings s = case parseConstant (bshow s) of
  --   (Right (Constant (CString s'))) -> s == s'
  --   _ -> False
  -- 
  -- prop_spaceSeparatedStrings :: [String] -> Bool
  -- prop_spaceSeparatedStrings s = case parseConstant separated of
  --   (Right (Constant (CString s'))) -> concat s == s'
  --   _ -> s == []
  --   where separated = B.pack $ unwords $ map show s