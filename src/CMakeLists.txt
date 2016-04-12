# c++ shared library and tools master build
project(sdmlab)
cmake_minimum_required (VERSION 3.0)
enable_testing()

##################
# c++ compiler

# modern c++ please
add_definitions("-std=c++14")
add_definitions("-Wall")

# is namespace wizardy allowed?
# YES: add_definitions("-Wc++1z-extensions")

# YMMV so this where we tune for our target/platform

add_definitions("-O3")
add_definitions("-march=native")


# this gets gcc to pass -q to the assembler which will use the clang
# assembler which groks the avx instructions - yeah don't ask there's
# history here...
IF ((${CMAKE_SYSTEM_NAME} MATCHES "Darwin") AND (${CMAKE_CXX_COMPILER_ID} MATCHES "GNU"))
  add_definitions("-Wa,-q") 
ENDIF()

# TODO for clang these are clang specific
# add_definitions("-Rpass=.*") # have clang show optimizations
# add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!

# add_definitions("-funroll-loops")
# add_definitions("-fvectorize")
# add_definitions("-fslp-vectorize")
# add_definitions("-fslp-vectorize-aggressive")


##################
# runtime library
add_subdirectory(rtl)

# command line tool
# add_subdirectory(repl)

########
# tests
add_subdirectory(test)






