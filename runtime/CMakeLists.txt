# runtime master build
project(geckortl)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# modern c++ please
add_definitions("-std=c++14")
add_definitions("-Wall")

###
# YMMV so this where we tune for our target/platform
#
# SNAFU
# see below for current compiler nightmares...  The only platform we are
# good on as of 9 May 2015 is linux with gcc4.9 and boost 1.58
# 
###

# XXX clang3.6's code blows up with any optimizations
# but its too slow without it! Like an order of magnitude.

#add_definitions("-O3")
add_definitions("-O0")

#add_definitions("-Rpass=.*") # have clang show optimizations
#add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!

# XXX this blows up gcc4.9 on os x -- not sure what assembler is used!
# but it cant grok the AVX instructions...
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





