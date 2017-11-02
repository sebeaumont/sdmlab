SDMLAB - Sparse Distributed Memory Lab 
======================================

The main artefact is the sdm runtime library which offers a high
performance sparse distributed memory or semantic vector database
targeted at a number of platforms.

The sdm library is currently written in c++ making extensive use of
boost::multiprocessing template library to provide memory management
tools to allow the supporting data structures to be allocated from a
memory mapped file, thus providing a database and RTL/API to act on it.

The application is high performance associative memory to further
research and development of the capabilities of a general model of
hyper-dimensional sparse space. This model has been extensively
applied to and indeed was incubated in the domain of data/text
mining.

There is a common c library under active development currently driven
by the need of the Haskell FFI required to support the following..


Abstract Machine Learning
-------------------------

We are also developing a DSL and compiler for ML based on SDM (as an
instance of a vectorspace) using the following tools:

- ghc   (Haskell compiler)
- idris (dependant type language)
- llvm  (compiler toolchain)

The library can be easily built for Linux/iOS/MacOS and other
posix/linux/bsd like platforms would be easy ports. Once the compiler
and RTL is complete it will support any LLVM target and will be hosted
on any platform that supports LLVM and Haskell (or Idris).

[Further Reading](hsdm/README.md)

Project contact: [Simon Beaumont](
mailto:s@molemind.net) 
_______________________
Copyright (c) 2012-2017 Simon Beaumont. All Rights Reserved.


