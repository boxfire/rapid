# LLVM codegen and native runtime for Idris 2

This is a (work in progress) backend to generate native executables from Idris code,
using the LLVM compiler infrastructure. Code is generated via [LLVM
IR](https://llvm.org/docs/LangRef.html) (i.e. not via generated C source code).

The source folder `rts/` contains a primitive runtime system with a relocating
semi-space garbage collector.

## Prerequisites

LLVM 11 needs to be installed and the binaries available in your `$PATH`. To
check if that is the case, try the following command:

    $ opt --version
    LLVM (http://llvm.org/):
      LLVM version 11.1.0

      Optimized build.
      Default target: x86_64-pc-linux-gnu
      Host CPU: znver2

## Compiling

    $ make
    $ make test

## Using

    # compile the included "Hello world" example
    ./build/exec/rapidc --cg llvm -o hello samples/Hello.idr
    # run the compiled binary
    ./build/exec/hello
