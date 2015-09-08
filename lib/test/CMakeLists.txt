# test programs 
include_directories(../vspace ../rtl)

add_executable (test_vectors test_vectors.c)
#target_link_libraries(vspace)

# low level space test
add_executable (test_api test_api.c)
#target_link_libraries()

# library test
add_executable (test_runtime test_runtime.c)
target_link_libraries(test_runtime sdm)

# test cases
add_test(NAME vectors COMMAND test_vectors 8)

add_test(NAME api COMMAND test_api)

add_test(NAME runtime COMMAND test_runtime)
