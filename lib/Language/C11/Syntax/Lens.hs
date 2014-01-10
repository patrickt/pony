module Language.C11.Syntax.Lens where
  
  import Control.Lens
  import StringTable.Atom
  
  class HasType f where
    typ :: Lens' (f a) a
  
  class HasName f where
    name :: Lens' (f a) Atom
  
  class HasArguments f where
    arguments :: Lens' (f a) [a]
  
  class HasBody f where
    body :: Lens' (f a) [a]
  
  class HasTarget f where
    target :: Lens' (f a) a
  