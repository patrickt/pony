{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Testing.HUnit.Syntax 
  ( tests
  ) where
  
  import qualified Data.ByteString.Char8 as B
  import Language.Pony
  import Language.C99.QuasiQuote
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax
  
  -- TODO more detailed tests, test for enum membership (should have three)
  case_parse_enum_with_trailing_comma :: Assertion
  case_parse_enum_with_trailing_comma = assertParsingSucceeds $ B.pack [here|
    enum 
    {
        red,
        blue,
        green,
    } color;
  |]
  
  parse' :: B.ByteString -> CSyn
  parse' = parseUnsafe expression
  
  case_attributed_structs :: Assertion
  case_attributed_structs = assertParsingSucceeds "struct foo { int blah; int bar; } __attribute__((deprecated)) blah;"
  
  case_attributed_enums :: Assertion
  case_attributed_enums = assertParsingSucceeds "enum foo { baz, baf, } __attribute__((deprecated)) whatsit;"
  
  case_whitespace_not_equal_to :: Assertion
  case_whitespace_not_equal_to = parse' "a!=b" @=? parse' "a != b"
     
  case_whitespace_not_equal_to_deref :: Assertion
  case_whitespace_not_equal_to_deref = parse' "a!=*b" @=? parse' "a != *b"
  
  case_whitespace_deref_not_equal_to_deref :: Assertion
  case_whitespace_deref_not_equal_to_deref = parse' "*a!=*b" @=? parse' "*a != *b"
  
  case_declaration_with_typeof_expression :: Assertion
  case_declaration_with_typeof_expression = assertParsingSucceeds "typeof(x) y = 1;"
  
  case_declaration_with_typeof_type :: Assertion
  case_declaration_with_typeof_type = assertParsingSucceeds "typeof(int *) y = 1;"
  
  tests :: [Test]
  tests = [$(testGroupGenerator)]