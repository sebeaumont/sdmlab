cmake_minimum_required (VERSION 3.0)

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

add_executable (gecko main.cpp)
target_link_libraries(gecko ${Boost_LIBRARIES})



