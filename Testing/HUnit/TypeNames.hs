module Testing.HUnit.TypeNames 
  ( typenameTestGroup
  ) where
  
  import Data.Either
  import Data.Generics.Fixplate
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Pony
  import qualified Data.ByteString.Char8 as B
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  import Text.PrettyPrint.Free
  
  emit :: String -> Doc a
  emit x = para' evalPretty $ convert $ parseUnsafe typeName (B.pack x)
  
  instance Eq (Doc a) where
    a == b = (show a) == (show b)
    
  roundTrip :: ByteString -> Assertion
  roundTrip b = pretty b @=? emit (B.unpack b)
  
  -- should fail since TT is not a type in bar
  innerScopeBad :: B.ByteString
  innerScopeBad = B.pack [here|
    void foo() {
      typedef char TT;
      TT x;
    }
    void bar() {
      TT y;
    } |]
  
  innerScopeGood :: B.ByteString
  innerScopeGood = B.pack [here|
    void foo() {
      typedef char TT;
      TT x;
    }
    void bar() {
        unsigned TT;
    } |]
  
  case_int                     = roundTrip "int"
  case_intp                    = roundTrip "int *"
  case_constint                = roundTrip "const int"
  case_constintp               = roundTrip "const int *"
  case_staticconstint          = roundTrip "static const int"
  case_staticconstintp         = roundTrip "static const int *"
  case_intpconst               = roundTrip "int * const"
  case_constintpconst          = roundTrip "const int * const"
  case_staticconstintpconst    = roundTrip "static const int * const"
  case_intpp                   = roundTrip "int * *"
  case_intpconstp              = roundTrip "int * const *"
  case_constintpconstp         = roundTrip "const int * const *"
  case_constintpconstprestrict = roundTrip "const int * const * restrict"
  case_char                    = roundTrip "char"
  case_chararr                 = roundTrip "char[]"
  case_chararr5                = roundTrip "char[5]"
  case_typedefscopegood        = assertParsingSucceeds innerScopeGood
  case_typedefscopebad         = assertParsingFails innerScopeBad
  
  typenameTestGroup :: Test
  typenameTestGroup = $(testGroupGenerator)
