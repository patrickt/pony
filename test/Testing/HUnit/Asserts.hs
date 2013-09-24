module Testing.HUnit.Asserts 
  ( assertLeft
  , assertRight
  , assertParsingSucceeds
  , assertParsingFails
  ) where

  import Data.ByteString.Char8
  import Data.Default
  import Data.Either
  import Data.Monoid
  import Language.Pony
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)

  
  assertLeft :: (Show b) => Either a b -> Assertion
  assertLeft (Left _) = assertBool "" True
  assertLeft (Right x) = assertFailure ("Got Right " <> show x <> ", expected a Left")
  
  assertRight :: (Show a) => Either a b -> Assertion
  assertRight (Right _) = assertBool "" True
  assertRight (Left x) = assertFailure ("Got Left " <> show x <> ", expected a Right")
  
  assertParsingSucceeds :: ByteString -> Assertion
  assertParsingSucceeds code = assertRight (runParser preprocessedC def "test" code)
  
  assertParsingFails :: ByteString -> Assertion
  assertParsingFails code = assertLeft (runParser preprocessedC def "test" code)

