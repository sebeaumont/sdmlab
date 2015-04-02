cmake_minimum_required (VERSION 3.0)

include_directories(vspace)
include_directories(.)

#find_package(Boost REQUIRED COMPONENTS program_options filesystem system)
find_package(Boost REQUIRED COMPONENTS program_options)
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

# todo runtime library so/dylib and llvm bitcode output
add_library(geckort runtime.cpp)

# command line test tool
add_executable (gecko main.cpp)
target_link_libraries(gecko ${Boost_LIBRARIES})

# new command line test tool
add_executable (gecko2 main2.cpp)
target_link_libraries(gecko2 ${Boost_LIBRARIES} geckort)




