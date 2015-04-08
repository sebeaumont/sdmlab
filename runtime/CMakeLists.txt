project(geckoruntime)

cmake_minimum_required (VERSION 3.0)

include_directories(vspace)
include_directories(.)

#find_package(Boost REQUIRED COMPONENTS program_options filesystem system)
find_package(Boost) # REQUIRED COMPONENTS program_options) #unit_test_framework
include_directories (${Boost_INCLUDE_DIR})

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
add_library(geckort runtime.cpp)

# command line test tool
#add_executable (gecko main.cpp)
#target_link_libraries(gecko ${Boost_LIBRARIES})

# command line tool
add_subdirectory(repl)

# unit tests
add_subdirectory(test)
enable_testing()
add_test(NAME unitest COMMAND unitest --log_level=all)




