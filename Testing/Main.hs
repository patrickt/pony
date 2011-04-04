module Main where
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.Providers.QuickCheck2
  import qualified Testing.HUnit.TypeNames as TN
  import qualified Testing.QuickCheck.Constants as QC
  import qualified Testing.HUnit.SpaceFreeParsing as SFP
  import qualified Testing.HUnit.PostfixExpressions as PE
  import qualified Testing.HUnit.SignalDeclaration as SD
  
  tests :: [Test]
  tests = [ testGroup "HUnit: type names" TN.tests
          , testGroup "QuickCheck: constants" QC.tests
          , testGroup "HUnit: space-free parsing" SFP.tests
          , testGroup "HUnit: postfix expressions" PE.tests
          , testGroup "HUnit: signal(3) declaration" SD.tests
          ]
  
  main :: IO ()
  main = defaultMain tests