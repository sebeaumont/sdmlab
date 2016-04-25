# c++ shared library and tools master build
project(sdmlab)
cmake_minimum_required (VERSION 3.0)
enable_testing()

# project version

set (SDM_VERSION_MAJOR 6)
set (SDM_VERSION_MINOR 0)

# install paths

set (CMAKE_INSTALL_PREFIX "/usr/local")
set (CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set (CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")


##################
# c++ compiler

# modern c++ please
add_definitions("-std=c++14")
add_definitions("-Wall")

# is future namespace wizardy allowed?
# YES: add_definitions("-Wc++1z-extensions")

add_definitions("-O3")
add_definitions("-march=native")

############################################
# compiler specific project wide defintions


# this gets gcc to pass -q to the assembler which will use the clang
# assembler which groks the avx instructions - yeah don't ask there's
# history here...

IF ((${CMAKE_SYSTEM_NAME} MATCHES "Darwin") AND (${CMAKE_CXX_COMPILER_ID} MATCHES "GNU"))
  add_definitions("-Wa,-q") 
ENDIF()


if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # TODO for clang these are clang specific
  # add_definitions("-Rpass=.*") # have clang show optimizations
  # add_definitions("-fsanitize=address") doesn't work with apple 3.6 compiler!
  add_definitions("-funroll-loops")
  add_definitions("-fvectorize")
  add_definitions("-fslp-vectorize")
  add_definitions("-fslp-vectorize-aggressive")
endif()


######################################################################
# we use think different threading backends on Darwin derived systems
# also we probe what cpu features we can exploit...

set(CPU_FEATURES)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  # OS X/XNU specific configuration - should work for iOS?
  message(STATUS "Will compile for Blocks/GCD and other Apple sweetness...")
  set(HAVE_DISPATCH 1)
  add_definitions("-stdlib=libc++")
  set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} "-stdlib=libc++")
  exec_program("sysctl -n machdep.cpu.features" OUTPUT_VARIABLE CPU_FEATURES)

else()

  # Use openmp on non OSX Systems (linux)
  
  find_package(OpenMP REQUIRED)
  if (OPENMP_FOUND)

    message(STATUS "OpenMP is configured")
    set(HAVE_OPENMP 1)
    add_definitions("-fopenmp")
    set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS})
    set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS})
    set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS})
    
    find_package(Threads REQUIRED)

    if (CMAKE_USE_PTHREADS_INIT)
      add_definitions("-pthread")
    endif()

    set(CMAKE_EXE_LINKER_FLAGS ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_THREAD_LIBS_INIT})
    
    file(READ "/proc/cpuinfo" _cpuinfo)
    string(REGEX REPLACE ".*flags[ \t]*:[ \t]+([a-zA-Z0-9_ ]+).*" "\\1" CPU_FEATURES "${_cpuinfo}")
  endif()

endif()


# normalise features before searching
string(TOUPPER ${CPU_FEATURES} CPU_FEATURES)


##################
# runtime library
add_subdirectory(rtl)


########
# tests
add_subdirectory(test)






