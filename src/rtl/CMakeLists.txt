# build c++ shared library 
# header only boost

find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})

# runtime library
add_library(sdm SHARED database.cpp)

set_target_properties(sdm PROPERTIES VERSION ${SDM_VERSION_MAJOR}.${SDM_VERSION_MINOR}
                                     SOVERSION ${SDM_VERSION_MAJOR})

# install lib and headers
install (TARGETS sdm LIBRARY DESTINATION lib)
install (FILES database.hpp DESTINATION include/sdm)






