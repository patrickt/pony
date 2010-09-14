%if false  
  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%include polycode.fmt

%if false

> module Compile where

> import Language.FiletOFish.Constructs
> import Language.FiletOFish.Constructs
> import Language.FiletOFish.PureExpressions
> import Expressions

> import Language.FiletOFish.IL.FoF.FoF
> import Language.FiletOFish.IL.FoF.Compile

> import IL.Paka.Paka
> import IL.Paka.Syntax
> import IL.Paka.Optimizer

%endif

\chapter*{The Filet-O-Fish Compiler(s)}
\label{chap:fof_comp}
\epigraph{I'm French!\\
          Why do think I have this outrageous accent, you silly king-a?!}%
         {Monty Python}


The Filet-o-Fish to C compiler is major component of
Filet-o-Fish. Major in the sense that it is a big chunk of code,
which correctness is critical. So, when playing with this part of the
code, better be cautious. The high-level specification of the compiler
is straightforward: given a Filet-o-Fish code, it should translate it
into a semantically equivalent C code. Well, it is a compiler, after
all.

However, from a usability point of view, it is vital to be able to
understand what the generated code is doing: think of a debugging
session that needs to go through some code generated by
Filet-o-Fish. Hence, we have implemented some so-called
\emph{optimizations} that tidy up the generated code. In order to ease
the implementation of these optimizations we rely on two standard
compiler techniques: first, we define a bunch of intermediate
languages (IL) to tackle a specific optimization issue, second we
implement the optimizer as a data-flow analysis solver. The current
state of affair is not as idyllic and the reader is referred to
Chapter~\ref{chap:future_work} to get an overview of my dreams.

Let us sketch the compilation process. 

> compile :: Semantics FoFConst PureExpr -> PakaCode
> compile sem =
>     optimizePaka $!
>     compileFoFtoPaka $!
>     compileSemtoFoF sem

First of all, The compiler is provided a value of type 
|Semantics FoFConst PureExpr|, built by the
operators of Chapter~\ref{chap:fof_operators}. While this structure
has a nice functional definition, making it convenient for
interpretation, it is bothersome to navigate on it. Therefore, the
first pass of the compiler is to reify this data-structure, as
explained in Chapter~\ref{chap:il_fof}. 

At the end of this compilation pass, the initial input has been
translated into an (hopefully) equivalent one in the FoF intermediate
language. In order to remove unnecessary variable assignments, a
second pass of the compiler translate the FoF code into Paka code. In
a nutshell, the Paka language only captures variable assignments,
ignoring the computational parts of statements. Hence, seeking and
simplifying redundant assignments is made easy: it corresponds to an
optimization phase applied to the resulting Paka code.

Because different optimizations will focus on different aspects of the
code, one could imagine several intermediate languages and refinements
between them. FoF and Paka are just an example of what could be
done. The name Paka comes from a retired hurricane: to pursue that
tradition, you can look up the list of retired hurricane
names~\cite{hurricane-names}. There is fair amount of ILs to be
implemented.
