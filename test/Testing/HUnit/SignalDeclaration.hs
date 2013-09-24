{-# LANGUAGE OverloadedStrings #-}

module Testing.HUnit.SignalDeclaration 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C99
  import Text.PrettyPrint.Free
  import Data.ByteString.Char8 (unpack)
  import Test.Framework.TH
  
  tests :: [Test]
  tests = [ $(testGroupGenerator) ]
  
  testSignal :: Assertion
  testSignal = assertEqual "signal(3)" practice (text $ unpack theory)
    where theory = "void (*signal(int sig, void (*func)(int)))(int);"
          practice = prettyPrint $ head $ parseUnsafe declarations theory
                                                                           