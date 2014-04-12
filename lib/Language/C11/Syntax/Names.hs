module Language.C11.Syntax.Names where 
  
  import Language.Pony.Overture
  import Data.String
  import Data.ByteString.Lens
  import Control.Lens
  import Data.Comp.Derive
  
  newtype Name = Name ByteString
    deriving (Show, Eq, Monoid, IsString, IsByteString)
  
  derive [ makeWrapped ] [''Name]
  