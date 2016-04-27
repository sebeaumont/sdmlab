# c++ python bindings
project(pysdm)
cmake_minimum_required (VERSION 3.0)


##################
# c++ compiler

add_definitions("-std=c++14")
add_definitions("-Wall")

add_definitions("-O3")
add_definitions("-march=native")

###
include_directories(../src)
###
find_package(PythonLibs 3 REQUIRED)
include_directories(${PYTHON_INCLUDE_DIR})

find_package(Boost COMPONENTS python REQUIRED)
include_directories(${Boost_INCLUDE_DIR})

# UC

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/../src/rtl")
find_package(SDM REQUIRED)

# shared lib target for python module
add_library(pysdm SHARED pysdm.cpp)
set_target_properties(pysdm PROPERTIES PREFIX "" SUFFIX ".so")
target_link_libraries(pysdm ${Boost_LIBRARIES} ${SDM_LIBRARY} ${PYTHON_LIBRARY})



