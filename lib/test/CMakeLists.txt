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


add_test(NAME vectors COMMAND test_vectors --log_level=all)

add_test(NAME api COMMAND test_api --log_level=all)

add_test(NAME runtime COMMAND test_runtime --log_level=all)
