# build c++ shared library 
# header only boost

find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})


# runtime library
add_library(sdm SHARED database.cpp)










