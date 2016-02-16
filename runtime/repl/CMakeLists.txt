# command line test tool for rtl
include_directories(../rtl)

find_package(Boost REQUIRED COMPONENTS program_options)
include_directories (${Boost_INCLUDE_DIR})

add_executable (gecko main.cpp)
target_link_libraries(gecko ${Boost_LIBRARIES} geckort)
