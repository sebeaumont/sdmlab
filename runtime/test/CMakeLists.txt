# test programs for rtl
include_directories(../rtl)

find_package(Boost REQUIRED COMPONENTS program_options)
include_directories (${Boost_INCLUDE_DIR})

add_executable (unitest runtime_tests.cpp)
target_link_libraries(unitest geckort)

add_test(NAME unitest COMMAND unitest --log_level=all)
