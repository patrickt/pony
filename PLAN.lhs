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
a compiler is a kind that combines transformers according to a given precedence.
> type Compiler t n = TokenStream t -> [Transformer t (Semantic n)] -> SemanticPrecedence -> S n

Given a notion of a C99 node and some FoFCode type that can be compiled to Filet-O-Fish,
> data FoFCode
> data C99

PonyC is a compiler that generates Filet-o-Fish code from C99-like nodes.
> ponyC :: Compiler C99 FoFCode
> ponyC = undefined

I need to learn how monad transformers work.
Why? I need to be able to describe at the type-level that a nodelike type - say PyNode - has behavior with respect to some other type a, and that at some point there will exist a type a such that PyNode can be converted into an a and made into a CNode.

Notes on type, newtype, data, family, and instance:
data x = Y <Constant> :: world-effectful. declares type 'x' and constructor Y :: Constant.

type x = (y :: type) :: type-effectful. declares type 'x' as a synonym for y