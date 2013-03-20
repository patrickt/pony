module Main where
  import Test.Framework
  -- import Testing.HUnit.TypeNames
  -- import Testing.HUnit.DeclarationPrinting
  -- import qualified Testing.HUnit.Assembly as A
  -- import Testing.QuickCheck.Constants
  -- import qualified Testing.HUnit.DeclarationPrinting as DP
  -- import qualified Testing.HUnit.SpaceFreeParsing as SFP
  -- import qualified Testing.HUnit.PostfixExpressions as PE
  -- import qualified Testing.HUnit.SignalDeclaration as SD
  -- import Testing.HUnit.Syntax
  -- import qualified Testing.HUnit.UtilityFunctions as UF
  
  -- -- TODO there's some template haskell that can help us with this
  -- 
  -- tests :: [Test]
  -- tests = [ -- testGroup "HUnit: type names" TN.tests
  --         -- , testGroup "HUnit: utility functions" UF.tests
  --         -- , testGroup "HUnit: array declaration" DP.tests
  --         -- , testGroup "HUnit: assembly parsing" A.tests
  --         -- , testGroup "QuickCheck: constants" QC.tests
  --         -- -- , testGroup "HUnit: space-free parsing" SFP.tests
  --         -- , testGroup "HUnit: postfix expressions" PE.tests
  --         -- , testGroup "HUnit: signal(3) declaration" SD.tests
  --         testGroup "HUnit: syntax" S.tests
  --         ]
  
  main :: IO ()
  main = defaultMain []