Hello. Welcome to Pony.

I wrote Pony in order to help make C programming suck less. Real-world C code, especially code written for embedded systems, usually focuses on irrelevances and minutae; with Pony's power to abstract over both the syntax and semantics of C, you can bring an end to boilerplate and irrelevant code. Pony's power goes even beyond automatically generating code; you can also transform your code arbitrarily, perhaps to implement garbage collection or dynamic dispatch. There's very little you can't do with Pony.

Installation
============

Pony has been tested under GHC 7.4.2 on Mac OS 10.8. Theoretically, it should work on any POSIX platform. If you do not already have a working installation of GHC and Cabal, I recommend that you install the Haskell Platform. You'll also need [cabal-dev](https://github.com/creswick/cabal-dev), installable via cabal.

To compile and run Pony, perform the following steps.

    cabal-dev configure
    cabal-dev install
    
To run the unit tests, pass `--enable-tests` to `cabal-dev configure` and then run `cabal-dev test`. To generate documentation, run `cabal-dev haddock`.

There are six built-in transformations that you can play with. They are currently being translated to use Pony's new library mechanism.

* `StringConcat` - introduces `<+>`, a string concatenation operator.
* `HelloWorld` - converts the identifier 'hello' into the archetypal hello-world printf.
* `LogicalShift` - introduces the `>>>` operator for right logical shift, as in Java.
* `CheckMalloc` - ensures that all calls to `malloc(3)` are checked for NULL.
* `SeparateDeclarations` - moves all variable declarations in a given function to the top of the function, as in old-style C.
* `PreciseGC` - adds shadow-stack garbage collection. The provided example is somewhat of a work in progress and currently only works on the `examples/gc/dlist.pony.c` example.

Please read the LICENSE file for the terms under which Pony is distributed, and please read the CREDITS file to read acknowledgments. 

Miscellaneous Information
=========================

The module for parsing C (`Language.C99`) is so named in order to distinguish it in nomenclature from Benedikt Huber's `Language.C` module (which was an invaluable resource for Pony development).

Debugging
=========

If you find a bug, please file it on the bug tracker. If you're having trouble triaging a bug, run `cabal-dev ghci` in the Pony root folder; it will load all of Pony's components into a ghci instance.

--
Patrick Thomson
04/29/2011
