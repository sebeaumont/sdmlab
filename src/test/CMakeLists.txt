# test programs for rtl
include_directories(..)

find_package(Boost REQUIRED)

include_directories (${Boost_INCLUDE_DIR})

message("${CPU_FEATURES}")

add_executable (rtl_load_space rtl_load_space.cpp)
target_link_libraries(rtl_load_space sdm)
target_link_libraries(rtl_load_space ${CMAKE_EXE_LINKER_FLAGS})


# low level space test 
add_executable (mms_0 mms_0.cpp)
target_link_libraries(mms_0 ${CMAKE_EXE_LINKER_FLAGS})

add_test(NAME mms_0 COMMAND mms_0 --log_level=all)

add_test(NAME rtl_load_space COMMAND rtl_load_space --log_level=all)


# c library test
add_executable (csdm_unit_tests ctest.c)
target_link_libraries(csdm_unit_tests csdm)
target_link_libraries(csdm_unit_tests ${CMAKE_EXE_LINKER_FLAGS})

add_test(NAME csdm_unit_tests COMMAND csdm_unit_tests --log_level=all) 


# c++ frametrain utility
# todo move this to own tools module

find_package(Boost REQUIRED COMPONENTS program_options)
add_executable (frametrainer frametrain.cpp)
target_link_libraries (frametrainer sdm ${Boost_LIBRARIES})

install (TARGETS frametrainer RUNTIME DESTINATION bin)


# c++ topology utility
# todo move this to own tools module

find_package(Boost REQUIRED COMPONENTS program_options)
find_package(ZeroMQ REQUIRED)

add_executable (topology topology.cpp)
target_link_libraries (topology
  sdm
  ${Boost_LIBRARIES}
  ${ZeroMQ_LIBRARIES})

install (TARGETS topology RUNTIME DESTINATION bin)
