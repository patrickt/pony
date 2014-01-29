module Language.C11.Syntax.Functions where
  
  import Language.Pony.Overture
  import Control.Lens
  import Data.Comp.Derive
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Function a = Function 
    { _fReturning :: a
    , _fName :: ByteString
    , _fArguments :: [a]
    , _fAttributes :: [a]
    } deriving (Show, Eq)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , makePrisms
         , makeLenses
         , smartConstructors] [ ''Function
                              ]
  
  instance HasType Function where typ = fReturning
  instance TravName Function where nameT = fName
  instance HasName Function where name = fName
  instance HasArguments Function where arguments = fArguments
  instance HasBody Function where body = fAttributes
  
  