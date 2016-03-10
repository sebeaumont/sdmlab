# build runtime library and ffi wrappers
#include_directories(../vspace)
#include_directories(.)

# header only boost
find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})


# runtime library
add_library(sdm database.cpp)

# linux need threads
find_package(Threads)

target_link_libraries(sdm ${CMAKE_THREAD_LIBS_INIT})
