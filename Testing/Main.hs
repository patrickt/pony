module Main where
  import Test.Framework
  import qualified Testing.HUnit.TypeNames as TN
  import qualified Testing.HUnit.InitializerLists as IL
  import qualified Testing.QuickCheck.Constants as QC
  import qualified Testing.HUnit.SignalDeclaration as SD
  import qualified Testing.HUnit.Syntax as S
  import qualified Testing.HUnit.Typedefs as TD
  import qualified Testing.HUnit.ControlStatements as CS
  
  tests :: [Test]
  tests = [ testGroup "HUnit: syntax" S.tests
          , testGroup "HUnit: type names" TN.tests
          , testGroup "HUnit: initializers" IL.tests
          , testGroup "QuickCheck: constants" QC.tests
          , testGroup "HUnit: signal(3) declaration" SD.tests
          , testGroup "HUnit: Typedefs" TD.tests
          , testGroup "Hunit: Control Statemnts" CS.tests
          ]
  
  main :: IO ()
  main = defaultMain tests