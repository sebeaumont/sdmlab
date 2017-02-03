# build c shared library
include_directories(..)

# header only boost
find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})

# c++ and c libraries
add_library(sdm SHARED sdmlib.cpp database.cpp)

set_target_properties(sdm PROPERTIES
  VERSION ${SDM_VERSION_MAJOR}.${SDM_VERSION_MINOR}
  SOVERSION ${SDM_VERSION_MAJOR})


# install lib and headers
install (TARGETS sdm LIBRARY DESTINATION lib)







