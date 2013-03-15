module Language.C99.Functions where
  
  import Language.Pony.Overture
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Declarations
  import Language.C99.Statements
  import Language.C99.Syntax
  import Data.Functor.Fix
  
  functionDefinition :: Parser CSyn
  functionDefinition = do
    -- save the old typedefs, since you can declare new typedefs within a function that go out of scope
    oldState <- getState
    
    (functionWrapper, returnType) <- functionSignature
    
    signature <- declarator
    
    -- compute its name
    -- TODO: skipping arguments here
    name <- maybe (fail "expected function name") (pure . name') (declName signature)
    
    -- get the body
    body <- compoundStmt <?> "function body"
    
    -- build the function 
    let func = Fix $ Function { name = name, typ = returnType, args = nil', body = body }
    
    -- restore the state
    putState oldState
    return $ functionWrapper func