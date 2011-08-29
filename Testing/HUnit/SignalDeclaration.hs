module Testing.HUnit.SignalDeclaration 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C99
  import Semantics.C
  
  tests :: [Test]
  tests = [ testCase "signal(3) declaration" testSignal
          ]
  
  testSignal :: Assertion
  testSignal = assertEqual "signal(3)" practice theory
    where theory = convert $ parseUnsafe preprocessedC "void (*signal(int sig, void (*func)(int)))(int);"
          practice = [GVariable $ Variable "signal" (SFunctionPointer void [Parameter (Just "sig") signedInt, 
                                                                           Parameter (Just "func") (SFunctionPointer void [Parameter Nothing signedInt] [])] []) Nothing]
