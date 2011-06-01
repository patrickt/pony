module Testing.HUnit.SignalDeclaration 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C
  import Semantics.C
  
  tests :: [Test]
  tests = [ testCase "signal(3) declaration" testSignal
          ]
  
  testSignal :: Assertion
  testSignal = assertEqual "signal(3)" practice theory
    where theory = convert $ parseUnsafe preprocessedC "void (*signal(int sig, void (*func)(int)))(int);"
          practice = [GVariable $ SVariable "signal" (SFunctionPointer void [SParameter (Just "sig") signedInt, 
                                                                           SParameter (Just "func") (SFunctionPointer void [SParameter Nothing signedInt] [])] []) Nothing]
