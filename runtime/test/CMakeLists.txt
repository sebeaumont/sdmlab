# test programs for rtl
include_directories(../rtl ../vspace)

find_package(Boost REQUIRED COMPONENTS program_options)
include_directories (${Boost_INCLUDE_DIR})

add_executable (unitest runtime_tests.cpp)
target_link_libraries(unitest geckort)

add_executable (vspace-0 vspace-0.cpp)


add_test(NAME vspace-0 COMMAND vspace-0 --log_level=all)

add_test(NAME unitest COMMAND unitest --log_level=all)
