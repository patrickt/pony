module Semantics.C.ASG.Arbitrary where 

  import Control.Applicative
  import Data.Functor.Fix
  import Semantics.C.ASG
  import Semantics.C.ASG.Newtypes
  import Test.QuickCheck

  instance Arbitrary FixSignedness where 
    arbitrary = elements $ (FSign . tie) <$> [Signed, Unsigned]

  instance Arbitrary FixSize where 
    arbitrary = elements $ (FSize . tie . MultipartT) <$> [[Fix ShortM], [Fix LongM]]

  instance Arbitrary FixType where
    arbitrary = do 
      (FSign base) <- arbitrary
      (FSize sign) <- arbitrary
      let intGen = return $ FType $ tie $ IntT { isign = sign, ibase = base }
      let charGen = return $ FType $ tie $ CharT sign
      let otherGen = elements $ (FType . tie) <$> [VoidT, FloatT, DoubleT]
      let pointerGen = arbitrary >>= \(FType t) -> return $ FType $ tie $ PointerToT t
      oneof [intGen, charGen, otherGen, pointerGen]

  instance Arbitrary FixNonVoidType where 
    arbitrary = do
      (FType typ) <- arbitrary `suchThat` notVoid
      return $ FNVType typ
      where
        notVoid (FType (Fix VoidT)) = False
        notVoid _ = True

  instance Arbitrary FixName where
    arbitrary = do 
      text <- listOf1 $ elements (['A'..'Z'] ++ ['a'..'z'])
      return $ FName $ tie $ Name text

  instance Arbitrary FixVar where 
    arbitrary = do
      (FType typ) <- arbitrary
      (FName nam) <- arbitrary
      return $ FVar $ tie $ Variable typ nam nil