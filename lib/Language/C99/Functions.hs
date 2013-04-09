module Language.C99.Functions where
  
  import Data.List
  import Language.Pony.Overture
  import Language.C99.Specifiers
  import Language.C99.Parser
  import Language.C99.Internal
  import Language.C99.Declarations
  import Language.C99.Statements
  import Language.C99.Syntax
  import Data.Functor.Fix
  
  functionDefinition :: Parser CSyn
  functionDefinition = do
    -- save the old typedefs, since you can declare new typedefs within a function that go out of scope
    oldState <- getState
    
    (functionWrapper, returnType, fname, args) <- functionSignature
    
    -- get the body
    body <- compoundStmt <?> "function body"
    
    attrs <- many
    let names = concat $ nub [ n | (CAttribute n) <- attrs ]
    let builder = if null names then id else Fix <$> Attributed names
    
    -- build the function 
    let func = builder $ Fix $ Function { name = fname, typ = returnType, args = args, body = body }
    
    -- restore the state
    putState oldState
    return $ functionWrapper func