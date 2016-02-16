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
# good on as of
# 9 May 2015 is linux with gcc4.5 and boost 1.58
# aug 2015 gcc5.2 boost 1.58 os x + linux

# XXX clang >= 3.6 code blows up with any optimizations

add_definitions("-O3")
add_definitions("-march=native")

#add_definitions("-O0")

#add_definitions("-Rpass=.*") # have clang show optimizations
#add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!

# this gets gcc to pass -q to the assembler which will use the clang assembler which groks the avx instructions
IF ((${CMAKE_SYSTEM_NAME} MATCHES "Darwin") AND (${CMAKE_CXX_COMPILER_ID} MATCHES "GNU"))
  add_definitions("-Wa,-q") # todo make this conditional on gcc and darwin platform
ENDIF()

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





