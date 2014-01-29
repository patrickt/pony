module Language.C11.Syntax.Lens where
  
  import Language.Pony.Overture
  import Control.Lens
  import StringTable.Atom
  
  class HasType f where
    typ :: Lens' (f a) a
  
  class HasName f where
    name :: Lens' (f a) ByteString
  
  class HasArguments f where
    arguments :: Lens' (f a) [a]
  
  class HasBody f where
    body :: Lens' (f a) [a]
  
  class HasTarget f where
    target :: Lens' (f a) a
    
  class HasValue f v | f -> v where
    value :: Lens' (f a) v
    
  class HasBase f where
    base :: Lens' (f a) Int
  