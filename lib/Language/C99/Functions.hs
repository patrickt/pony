module Language.C99.Functions where
  
  import Language.Pony.Overture
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Specifiers
  import Language.C99.Declarations
  import Language.C99.Statements
  import Language.C99.Syntax
  import Data.Functor.Fix hiding (foldl)
  
  import Data.List (sort, partition)
  import Semantics.C.Reifiable
  
  specifierBelongsToFunction :: CSpecifier -> Bool
  specifierBelongsToFunction (SSpec CStatic) = True
  specifierBelongsToFunction (SSpec CExtern) = True
  specifierBelongsToFunction (TQual CInline) = True
  specifierBelongsToFunction _ = False
  
  functionDefinition :: Parser CSyn
  functionDefinition = do
    -- save the old typedefs, since you can declare new typedefs within a function that go out of scope
    oldState <- getState
    
    -- slurp in a bunch of specifiers, sort them, then partition them into two groups, one consisting 
    -- of those that modify the function and the other of those that modify its return type.
    (funcSpecs, returnTypeSpecs) <- partition specifierBelongsToFunction <$> sort <$> some specifier
    
    -- TODO: we're gonna make this prettier
    -- build the function's return type
    let returnType = typeFromSpecifiers returnTypeSpecs
    
    -- parse the declarator that specifies its name and its arguments
    -- TODO: safety checks for name and function modifier
    signature <- declarator
    
    -- compute its name
    -- TODO: skipping arguments here
    name <- maybe (fail "expected function name") (pure . name') (declName signature)
    
    -- get the body
    body <- compoundStmt <?> "function body"
    
    -- build the function 
    let func = Fix $ Function { fname = name, ftype = returnType, fargs = nil', fbody = body }
    
    -- now attach the specifiers, if any, that modified the function (e.g. extern, static, inline)
    let functionModifiers = builderFromSpecifier <$> funcSpecs
    let attributedFunc = foldl (.) id functionModifiers func
    
    -- restore the state
    putState oldState
    return attributedFunc