{-# LANGUAGE ViewPatterns #-}

module Language.Pony.Transformations.Sanitizers 
  (runReplacer, unfoldGroups, flatGroupsAxiom) where
  
  import Semantics.C
  import Control.Monad.State
  import Data.Functor.Fix
  import Language.Pony.Overture
  
  type Recorder = State [FSem]
  
  replaceTypedefs :: FSem -> Recorder FSem
  replaceTypedefs x@(Fix (Typedef name@(Fix (Name _)) _)) = do
    contained <- gets (elem x)
    unless contained (modify (x :))
    return $ if contained then (tie $ TypedefT name) else x
  replaceTypedefs x = pure x

  runReplacer :: FSem -> FSem
  runReplacer x = evalState (replaceTypedefs `transformM` x) []
  
  unfoldGroups :: FSem -> [FSem]
  unfoldGroups (µ -> Group a) = a >>= unfoldGroups
  unfoldGroups x = [x]

  flatGroupsAxiom :: FSem -> Sem FSem
  flatGroupsAxiom (µ -> Group a) = Group $ a >>= unfoldGroups
  flatGroupsAxiom (µ -> Program a) = Program $ a >>= unfoldGroups
  flatGroupsAxiom x = out x
  
    
  

