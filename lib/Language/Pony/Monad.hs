module Language.Pony.Monad where
  
  import Control.Applicative
  import Control.Arrow
  import Control.Monad.State
  import Data.ByteString (ByteString)
  import Data.Default
  import Data.Functor.Fix
  import Data.Map (Map)
  import Language.C99 hiding (State, Const) -- TODO: this should not be exported
  import Language.Pony.Overture
  import Text.PrettyPrint.Free
  
  import qualified Data.ByteString as B
  import qualified Data.Map as M
  
  data PonyEnv a where
    PE :: { topLevel :: (Functor a) => Mu a
          , bindings :: Map ByteString (Mu a)} -> PonyEnv a
  
  type Pony a r = State (PonyEnv a) r
  type CPony a = Pony C99 a
  
  opT t op = do
    it <- gets topLevel
    modify (\s -> s { topLevel = t op it })
    
  opM t op = do
    it <- gets topLevel
    transformed <- t op it
    modify (\s -> s { topLevel = transformed })
  
  bottomUp :: Functor a => (Mu a -> Mu a) -> Pony a ()
  bottomUp = opT transform
  
  bottomUpM :: Traversable a => (Mu a -> Pony a (Mu a)) -> Pony a ()
  bottomUpM = opM transformM
  
  topDown_ :: Functor a => (Mu a -> Mu a) -> Pony a ()
  topDown_ = opT topDownTransform
  
  topDownM :: Traversable a => (Mu a -> Pony a (Mu a)) -> Pony a ()
  topDownM = opM topDownTransformM
  
  descendLevel :: Functor a => (Mu a -> Mu a) -> Pony a ()
  descendLevel = opT descend
  
  descendLevelM :: Traversable a => (Mu a -> Pony a (Mu a)) -> Pony a ()
  descendLevelM = opM descendM
  
  rewriteAll :: Functor a => (Mu a -> Maybe (Mu a)) -> Pony a ()
  rewriteAll = opT rewrite
  
  -- rewriteAllM :: Functor f => (Mu f -> m (Maybe (Mu f))) -> Mu f -> Pony a ()
  -- rewriteAllM = opM rewriteM
  
  evalPony :: Pony a r -> Mu a -> r
  evalPony p item = evalState p (PE item def)
  
  evalPonyPretty :: (PrettyAlg a) => Pony a r -> Mu a -> Doc e
  evalPonyPretty p = prettyPrint <<< evalPony (p >> gets topLevel)
    
  evalPonyPath :: Pony C99 a -> FilePath -> IO (Doc e)
  evalPonyPath op path = do
    result <- preprocessAndParse preprocessedC path def
    either (fail . show) (return . evalPonyPretty op) result
  