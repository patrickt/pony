module Main where
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.Providers.QuickCheck2
  import qualified Testing.HUnit.TypeNames as TN
  import qualified Testing.QuickCheck.Constants as QC
  
  tests :: [Test]
  tests = [ testGroup "HUnit: type names" TN.tests
          , testGroup "QuickCheck: constants" QC.tests
          ]
  
  main :: IO ()
  main = defaultMain tests