Testing and Instrumentation
===========================

Test Harness for Bitcode functions - N.B. only Int32 -> Int32 functions are currently supported:

    ./cabal-dev/bin/reptile --bitcode=../hllvm/examples/myfib.bc --function=fib --fnarg=10 --iterations=1

TODO
----
I will expand this to bvectors in due course
