SDMLAB - Sparse Distributed Memory Lab Project 
==============================================

This project is the Sparse Distributed Memory Research and Development
Hub.  The main artefact is the sdm runtime library which offers a high
performance database targeted at a number of platforms.

We now fully support iOS 9.3 and OSX/Linux/BSD x86_64 using the
following tools:

- clang on Darwin based systems, gcc on Linux based systems
- Swift on iOS
- boost

The application is high performance associative memory to further
research and development of the capabilities of a general model
of hyper-dimensional sparse space.

We are also developing a DSL and compiler for ML based on SDM using the following
toolchain and libraries:

- ghc
- idris (dependant type language)
- llvm

There is an additional ios framework (sdmi) which wraps the c++
implementation with an objective c++ API suitable for importing into
Swift. 

There is a common c library under construction.


Project contact: [Simon Beaumont](mailto:s@molemind.net) 
_______________________
Copyright (c) 2012-2016 Simon Beaumont. All Rights Reserved.


