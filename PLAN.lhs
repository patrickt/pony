Pony's highest-level description follows.
This is a Literate Haskell file, though it's not meant to be executed for anything but illustrative purposes.
> {-# LANGUAGE EmptyDataDecls, RankNTypes #-}

Given a stream kind,
> data S a
a token kind,
> data Token a
and an opaque type for a byte,
> data Byte
then a Tokenizer is a kind that converts a byte stream to a token.
> type Tokenizer t = S Byte -> Token t

When combined with a data type that represents how to properly combine a list of tokens,
> data TokenizationRules 
then a token stream is a stream of tokens from a list of tokens.
> type TokenStream b = forall a. FilePath -> [Tokenizer a] -> TokenPrecedence -> S (Token b)

A syntax transformer maps a stream of tokens to some semantic information.
> type Transformer t n = S t -> Semantic n

Given some semantic precedence,
> data SemanticPrecedence
a compiler is a kind that combines transformers according to that precedence.
> type Compiler t n = TokenStream t -> [Transformer t n] -> SemanticPrecedence t n -> S n

Given a notion of a C99 node and some FoFCode type that can be compiled to Filet-O-Fish,
> data FoFCode
> data C99

PonyC is a compiler that generates Filet-o-Fish code from C99-like nodes.
> ponyC :: Compiler C99 FoFCode
> ponyC = undefined

