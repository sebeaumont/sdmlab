# test programs 
include_directories(../vspace ../rtl)

add_executable (test_vectors test_vectors.c)
#target_link_libraries(vspace)

# low level space test
add_executable (test_api test_api.c)
#target_link_libraries()

add_test(NAME vectors COMMAND test_vectors --log_level=all)

add_test(NAME api COMMAND test_api --log_level=all)
