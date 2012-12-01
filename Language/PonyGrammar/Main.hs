module Main where
  import Language.PonyGrammar.Parser
  
  
  main :: IO ()
  main = do
    let c = "Grammar test { test ::= test. }" 
    putStrLn . show $ lexer c
    putStrLn . show $ parseGrammar' c