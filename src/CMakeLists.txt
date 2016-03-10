# runtime master build
project(sdmlab)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# modern c++ please
add_definitions("-std=c++14")
add_definitions("-Wall")
# is namespace wizardy allowed
add_definitions("-Wc++1z-extensions")

# YMMV so this where we tune for our target/platform

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






