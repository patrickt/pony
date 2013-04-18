module Main where
  import Test.Framework
  import qualified Testing.HUnit.TypeNames as TN
  import qualified Testing.HUnit.InitializerLists as IL
  import qualified Testing.QuickCheck.Constants as QC
  import qualified Testing.HUnit.SignalDeclaration as SD
  import qualified Testing.HUnit.Syntax as S
  
  tests :: [Test]
  tests = [ testGroup "HUnit: syntax" S.tests
          , testGroup "HUnit: type names" TN.tests
          , testGroup "HUnit: initializers" IL.tests
          , testGroup "QuickCheck: constants" QC.tests
          , testGroup "HUnit: signal(3) declaration" SD.tests
          ]
  
  main :: IO ()
  main = defaultMain tests