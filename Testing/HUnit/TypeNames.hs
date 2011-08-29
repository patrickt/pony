module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C99
  import Semantics.C
  
  tests :: [Test]
  tests = [ testCase "const int * const * restrict => restrict pointer to const pointer to const int" testRestrictPtrToConstPtrToInt ]
    
  testRestrictPtrToConstPtrToInt :: Assertion
  testRestrictPtrToConstPtrToInt = assertEqual "whatever" theory practice where
    practice = convert $ parseUnsafe typeName "const int * const * restrict "
    theory = SPointerTo (SPointerTo (SInt (IntegerFlags Signed 32) [Const]) [Const]) [Restrict]
  
  