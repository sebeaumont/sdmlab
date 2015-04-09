# runtime master build
project(geckortl)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# modern c++ please
add_definitions("-std=c++11")

# YMMV so this where we tune for our target/platform
add_definitions("-O3")
add_definitions("-mavx")
add_definitions("-funroll-loops")
add_definitions("-fvectorize")
add_definitions("-fslp-vectorize")
add_definitions("-fslp-vectorize-aggressive")

# runtime library
add_subdirectory(rtl)

# command line tool
add_subdirectory(repl)

# tests
add_subdirectory(test)





