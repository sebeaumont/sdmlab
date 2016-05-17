# Find Molemind SDM library...
#  SDM_FOUND 
#  SDM_INCLUDE_DIRS 
#  SDM_LIBRARIES

find_path(SDM_INCLUDE_DIR sdm/database.hpp
  HINTS /usr/local/include /opt/molemind/include)

find_library(SDM_LIBRARY NAMES sdm
  HINTS /usr/local/lib /opt/molemind/lib)

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set SDM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(SDM  DEFAULT_MSG
                                  SDM_LIBRARY SDM_INCLUDE_DIR)

mark_as_advanced(SDM_INCLUDE_DIR SDM_LIBRARY)

set(SDM_LIBRARIES ${SDM_LIBRARY})
set(SDM_INCLUDE_DIRS ${SDM_INCLUDE_DIR})
