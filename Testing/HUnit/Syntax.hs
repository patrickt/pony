module Testing.HUnit.Syntax (tests) where
  
  import qualified Data.ByteString.Char8 as B
  import Language.C99.Specifiers
  import Testing.Heredoc
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  
  -- TODO more detailed tests, test for enum membership (should have three)
  trailingCommaEnum :: B.ByteString
  trailingCommaEnum = B.pack [here|
    enum 
    {
        red,
        blue,
        green,
    } color;
  |]
  
  tests :: [Test]
  tests = 
    [ assertParsingSucceeds "enums with trailing commas" trailingCommaEnum
    ]

