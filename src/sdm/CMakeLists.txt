# runtime master build
project(sdmlib)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# oh yes please
add_definitions("-Wall")

# tunin'
add_definitions("-O3")
add_definitions("-march=native")
#add_definitions("-mavx")

# CLANG options
IF (${CMAKE_C_COMPILER_ID} MATCHES "Clang")

  # diagnostics
  #add_definitions("-Rpass-missed=loop-vectorize")
  add_definitions("-Rpass-analysis=loop-vectorize")
  
  #add_definitions("-Rpass=.*") # have clang show optimizations
  #add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!
  # optimizations and code generation
  #add_definitions("-funroll-loops")
  #add_definitions("-fvectorize")
  #add_definitions("-fslp-vectorize")
  #add_definitions("-fslp-vectorize-aggressive")
  #add_definitions("-emit-llvm")
  # llvm options
  #add_definitions("-mllvm")
  #add_definitions("-unroll-count=16")
ENDIF()

# vectorspace library
add_subdirectory(lib)

# command line tool
#add_subdirectory(repl)

# tests
add_subdirectory(test)

