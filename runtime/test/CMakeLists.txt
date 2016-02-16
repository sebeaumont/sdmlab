# test programs for rtl
include_directories(../rtl ../vspace)

find_package(Boost REQUIRED COMPONENTS program_options)
include_directories (${Boost_INCLUDE_DIR})

find_package(Threads)

add_executable (rtl_load_space rtl_load_space.cpp)
target_link_libraries(rtl_load_space geckort)

# low level space test 
add_executable (vspace_0 vspace_0.cpp)
target_link_libraries(vspace_0 ${CMAKE_THREAD_LIBS_INIT})

add_test(NAME vspace_0 COMMAND vspace_0 --log_level=all)

add_test(NAME rtl_load_space COMMAND rtl_load_space --log_level=all)
