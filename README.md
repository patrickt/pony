Hello. Welcome to Pony.

I wrote Pony in order to help make C programming suck less. Real-world C code, especially code written for embedded systems, usually focuses on irrelevances and minutae; with Pony's power to abstract over both the syntax and semantics of C, you can bring an end to boilerplate and irrelevant code. Pony's power goes even beyond automatically generating code; you can also transform your code arbitrarily, perhaps to implement garbage collection or dynamic dispatch. There's very little you can't do with Pony.

Installation
============

Pony has been tested under GHC 7.0.3 on Mac OS 10.6. Theoretically, it should work on any POSIX platform. If you do not already have a working installation of GHC and Cabal, I recommend that you install the Haskell Platform. 

To compile and run Pony, perform the following steps.

    cabal install --only-dependencies # only needs to be run once
    cabal configure
    cabal build
    ./runpony --ponyproj=<your .ponyproj> --trans=<a transformation> --output=<output file> [filename]
    
To run the unit tests, please run the `./tests` shell script. To generate documentation, run `cabal haddock`.

There are six built-in transformations that you can play with. Pass these names to Pony with the --trans option:

* `StringConcat` - introduces `<+>`, a string concatenation operator. (Be sure to use `examples/example.ponyproj` to ensure that the new operator is added.)
* `HelloWorld` - converts the identifier 'hello' into the archetypal hello-world printf.
* `LogicalShift` - introduces the `>>>` operator for right logical shift, as in Java.
* `CheckMalloc` - ensures that all calls to `malloc(3)` are checked for NULL.
* `SeparateDeclarations` - moves all variable declarations in a given function to the top of the function, as in old-style C.
* `PreciseGC` - adds shadow-stack garbage collection. The provided example is somewhat of a work in progress and currently only works on the `examples/gc/dlist.pony.c` example.

Please read the LICENSE file for the terms under which Pony is distributed, and please read the CREDITS file to read acknowledgments. 

Debugging
=========

If you find a bug, please file it on the bug tracker. If you're having trouble triaging a bug, I recommend loading up Repl.hs, which provides access to Pony's subcomponents, in GHCi.

--
Patrick Thomson
04/29/2011