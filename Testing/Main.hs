module Main where
  import Test.Framework
  import qualified Testing.HUnit.TypeNames as TN
  import qualified Testing.HUnit.Assembly as A
  import qualified Testing.QuickCheck.Constants as QC
  import qualified Testing.HUnit.DeclarationPrinting as DP
  import qualified Testing.HUnit.SpaceFreeParsing as SFP
  import qualified Testing.HUnit.PostfixExpressions as PE
  import qualified Testing.HUnit.SignalDeclaration as SD
  import qualified Testing.HUnit.UtilityFunctions as UF
  
  tests :: [Test]
  tests = [ testGroup "HUnit: type names" TN.tests
          , testGroup "HUnit: utility functions" UF.tests
          , testGroup "HUnit: array declaration" DP.tests
          , testGroup "HUnit: assembly parsing" A.tests
          -- , testGroup "QuickCheck: constants" QC.tests
          -- , testGroup "HUnit: space-free parsing" SFP.tests
          -- , testGroup "HUnit: postfix expressions" PE.tests
          -- , testGroup "HUnit: signal(3) declaration" SD.tests
          ]
  
  main :: IO ()
  main = defaultMain tests