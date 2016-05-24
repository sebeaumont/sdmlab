# build c++ shared library
include_directories(..)

# header only boost
find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})

# c++ and c libraries
add_library(sdm SHARED database.cpp)

set_target_properties(sdm PROPERTIES
  VERSION ${SDM_VERSION_MAJOR}.${SDM_VERSION_MINOR}
  SOVERSION ${SDM_VERSION_MAJOR})

# c bindings 
add_library(csdm SHARED csdm.cpp)

set_target_properties(csdm PROPERTIES
  VERSION ${SDM_VERSION_MAJOR}.${SDM_VERSION_MINOR}
  SOVERSION ${SDM_VERSION_MAJOR})

target_link_libraries(csdm sdm)


# install lib and headers
install (TARGETS sdm LIBRARY DESTINATION lib)
install (TARGETS csdm LIBRARY DESTINATION lib)

#install (FILES database.hpp DESTINATION include/sdm)
install (FILES csdm.h DESTINATION include/sdm)





