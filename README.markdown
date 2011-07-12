This library provides a compile-time abstraction of writing a bounded number
of bytes to memory; i.e., all functions in this module are intended to be
inlined at compile time. The main use of this /write abstraction/ is to
share the implementation of encodings of Haskell values where the maximal
number of bytes to be written per value is statically known.

This library only provides types and combinators for defining encodings. A
number of actual encodings is provided by the @encoding-encodings@ library. They
are used together with the @Builder@ type from the @bytestring@ library to
provide builders for a variety of encodings in the @encoding-builders@
library.

A word of caution: if you just need to serialize some data in some standard
encoding format, then use the builders provided in the @encoding-builders@
library. They provide an easy to use IO-free interface, that gives you very
good performance for most use cases. If you need to write your own encoding,
then be aware that

 1. compile-time abstractions can result in rather inefficient code, if used
    the wrong way, and

 2. you are writing code with /all saftey belts off/; i.e., 
    /this is the code that makes your application vulnerable to buffer-overflow
    attacks!/
.
Note that if your encoding is not yet supported by @encoding-builders@, but
it is standardized, then I'm very happy to accept patches for the
@encoding-builders@ library.
