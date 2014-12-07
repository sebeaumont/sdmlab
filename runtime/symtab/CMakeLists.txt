cmake_minimum_required (VERSION 3.0)

find_package(Boost REQUIRED)
include_directories (${Boost_INCLUDE_DIR})

# modern c++ please
add_definitions("-std=c++11")
# YMMV so this where we tune for our target/platform
add_definitions("-O3")

#add_definitions("-mavx")

add_executable (loadtest main.cpp)
add_executable (bimaptest bimap.cpp)
add_executable (miitest multiindex.cpp)

#target_link_libraries(test)
