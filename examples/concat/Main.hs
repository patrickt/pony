{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where
  import Language.Pony

  strlen l = call' "strlen" [l]
  malloc l s = binary' l "=" $ call' "alloca" [s]
  strlcpy to from size = call' "strlcpy" [to, from, size]
  strlcat to from size = call' "strlcat" [to, from, size]
  
  convertConcatenation :: CSyn -> CSyn
  convertConcatenation (µ -> Group stmts) = group' $ concatMap go stmts
    where go (µ -> Binary a "=" (µ -> Binary l "<+>" r)) =
            [ Fix $ Variable { typ = typedef' "size_t" int'
                             , name = "needed_size"
                             , value = Just (strlen l) }
            , malloc a "needed_size"
            , strlcpy a l "needed_size"
            , strlcat a r "needed_size"
            ]
          go other = [other]
  convertConcatenation other = other
  
  allOps :: [Operator]
  allOps = defaultOperators ++ [ cautiousBinaryOp "<+>" 9 ]

  
  main :: IO ()
  main = runPony allOps (bottomUp convertConcatenation)