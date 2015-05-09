# runtime master build
project(geckortl)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# modern c++ please
add_definitions("-std=c++11")
add_definitions("-Wall")

###
# YMMV so this where we tune for our target/platform
#
# see below for current compiler nightmares...  The only platform we a
# good on as of 9 May 2015 is linux with gcc4.9 and boost 1.58 I might
# rewrite it all in fucking Haskell or get a life.. three years of
# effort in the balance I hate this fucking industry - I should stick
# to maths...
###

# XXX this blows up clang3.6 or at least clang3.6's code blows up
# but its too slow without it! Like an order of magnitude.
# add_definitions("-O3") 

# XXX this blows up gcc4.9 on os x -- not sure what assembler is used!
# add_definitions("-mavx") 

# these are clang specific
# add_definitions("-funroll-loops")
# add_definitions("-fvectorize")
# add_definitions("-fslp-vectorize")
# add_definitions("-fslp-vectorize-aggressive")

# runtime library
add_subdirectory(rtl)

# command line tool
add_subdirectory(repl)

# tests
add_subdirectory(test)





