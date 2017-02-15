# c++ shared library and tools master build
# copyright simon beaumont 2012-2016 - all rights reserved

project(sdmlab)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# project version

set (SDM_VERSION_MAJOR 8)
set (SDM_VERSION_MINOR 0)

set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/../cmake/Modules/")
# install paths

set (CMAKE_INSTALL_PREFIX "/opt/molemind")
set (CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")


#########################################
# project global c/c++ compiler settings
#########################################

# common to both c & c++ flags
set(SDM_ALL_FLAGS "-Wall -O3 -march=native")

# c++ only
set(SDM_CXX_FLAGS "-std=c++14")

# for max compatability c lib
set(SDM_C_FLAGS "-std=c90")


# is future namespace wizardy allowed?
# YES: set(SDM_CXX_FLAGS "-Wc++1z-extensions")


############################################
# compiler specific project wide defintions


# this gets gcc to pass -q to the assembler which will use the clang
# assembler which groks the avx instructions - yeah don't ask there's
# history here...
#IF ((${CMAKE_SYSTEM_NAME} MATCHES "Darwin") AND (${CMAKE_CXX_COMPILER_ID} MATCHES "GNU"))
#  set(SDM_CXX_FLAGS "-Wa,-q") 
#ENDIF()


if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # TODO for clang these are clang specific
  # set(SDM_CXX_FLAGS "-Rpass=.*") # have clang show optimizations
  # set(SDM_CXX_FLAGS "-fsanitize=address") doesn't work with apple 3.6 compiler!
  set(SDM_ALL_FLAGS "${SDM_ALL_FLAGS} -funroll-loops -fvectorize -fslp-vectorize -fslp-vectorize-aggressive")
endif()


######################################################################
# we use think different threading backends on Darwin derived systems
# also we probe what cpu features we can exploit...

set(CPU_FEATURES)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  # OS X/XNU specific configuration - should work for iOS?
  message(STATUS "Will compile for Blocks/GCD and other Apple sweetness...")
  set(HAVE_DISPATCH 1)
  set(SDM_CXX_FLAGS "${SDM_CXX_FLAGS} -stdlib=libc++")
  set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} "-stdlib=libc++")
  exec_program("sysctl -n machdep.cpu.features" OUTPUT_VARIABLE CPU_FEATURES)

else()

  # Use openmp on non OSX Systems (linux)
  
  find_package(OpenMP REQUIRED)
  if (OPENMP_FOUND)

    message(STATUS "OpenMP is configured")
    set(HAVE_OPENMP 1)
    set(SDM_ALL_FLAGS "${SDM_ALL_FLAGS} -fopenmp")
    set(SDM_C_FLAGS "${SDM_C_FLAGS} ${OpenMP_C_FLAGS}")
    set(SDM_CXX_FLAGS "${SDM_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS})
    
    find_package(Threads REQUIRED)

    if (CMAKE_USE_PTHREADS_INIT)
      set(SDM_CXX_FLAGS "${SDM_CXX_FLAGS} -pthread")
    endif()

    set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_THREAD_LIBS_INIT})
    
    file(READ "/proc/cpuinfo" _cpuinfo)
    string(REGEX REPLACE ".*flags[ \t]*:[ \t]+([a-zA-Z0-9_ ]+).*" "\\1" CPU_FEATURES "${_cpuinfo}")
  endif()

endif()

set(CMAKE_CXX_FLAGS "${SDM_CXX_FLAGS} ${SDM_ALL_FLAGS}")
set(CMAKE_C_FLAGS "${SDM_C_FLAGS} ${SDM_ALL_FLAGS}")

message(${CMAKE_CXX_FLAGS})
message(${CMAKE_C_FLAGS})

# normalise features before searching
string(TOUPPER ${CPU_FEATURES} CPU_FEATURES)


##################
# runtime library
add_subdirectory(rtl)


########
# tests
add_subdirectory(test)





