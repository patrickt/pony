Hi there. Pony is a source-to-source transpiler for C programs. It aims to be small, fast, elegant, extensible, and verifiable (to the greatest extent possible). 

It is TOTALLY alpha and is in the process of being entirely rewritten. Do not use it. But please check it out (specifically the experimental branch; master is very old).

Installation
============

Pony has been tested under GHC 7.6.3 on Mac OS 10.9. Theoretically, it should work on any POSIX platform. If you do not already have a working installation of GHC and Cabal, I recommend that you install the Haskell Platform and use Cabal sandboxing.

There are six built-in transformations that you can play with. They are currently being translated to use Pony's new library mechanism.

* `StringConcat` - introduces `<+>`, a string concatenation operator.
* `HelloWorld` - converts the identifier 'hello' into the archetypal hello-world printf.
* `LogicalShift` - introduces the `>>>` operator for right logical shift, as in Java.
* `CheckMalloc` - ensures that all calls to `malloc(3)` are checked for NULL.
* `SeparateDeclarations` - moves all variable declarations in a given function to the top of the function, as in old-style C.
* `PreciseGC` - adds shadow-stack garbage collection. The provided example is somewhat of a work in progress and currently only works on the `examples/gc/dlist.pony.c` example.

Pony is copyright (c) 2009, George Washington University. Please read the LICENSE file for the terms under which Pony is distributed, and please read the CREDITS file to read acknowledgments. 

Miscellaneous Information
=========================

The module for parsing C (`Language.C99`) is so named in order to distinguish it in nomenclature from Benedikt Huber's `Language.C` module (which was an invaluable resource for Pony development).

--
Patrick Thomson
04/29/2011
