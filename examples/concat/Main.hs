{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Main where
  import Language.Pony

  strlen l = call' "strlen" [l]
  malloc l s = binary' l "=" $ call' "alloca" [s]
  strlcpy to from size = call' "strlcpy" [to, from, size]
  strlcat to from size = call' "strlcat" [to, from, size]

  convertConcatenation :: CSyn -> C99 CSyn
  convertConcatenation (µ -> Group stmts) = Group $ concatMap go stmts
    where go (µ -> Binary a "=" (µ -> Binary l "<+>" r)) =
            [ Fix $ Variable { typ = Fix $ Typedef { name = nil', typ = "size_t" }
                             , name = "needed_size"
                             , value = Nothing }
            , malloc a "needed_size"
            , strlcpy a l "needed_size"
            , strlcat a r "needed_size"
            ]
          go other = [other]
  convertConcatenation other = out other
  
  main :: IO ()
  main = run $ def { binaryOperators = defaultOperators ++ [ cautiousBinaryOp "<+>" 9 ]
                   , anamorphisms = [convertConcatenation] }
