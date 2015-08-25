# runtime master build
project(hazel)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# oh yes please
add_definitions("-Wall")

# tunin'
add_definitions("-O3")
add_definitions("-march=native")
#add_definitions("-mavx")

#add_definitions("-Rpass=.*") # have clang show optimizations
#add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!

# these are clang specific
#add_definitions("-funroll-loops")
#add_definitions("-fvectorize")
#add_definitions("-fslp-vectorize")
#add_definitions("-fslp-vectorize-aggressive")
#add_definitions("-emit-llvm")
#add_definitions("-mllvm")
#add_definitions("-unroll-count=16")

# vectorspace library
add_subdirectory(rtl)

# command line tool
#add_subdirectory(repl)

# tests
add_subdirectory(test)

