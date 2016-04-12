# build c++ shared library 
# header only boost

find_package(Boost) 
include_directories (${Boost_INCLUDE_DIR})

# we use think different threading backends on Darwin derived systems
# also we probe what cpu features we can exploit...

set(CPU_FEATURES)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  # OS X/XNU specific configuration - should work for iOS?
  message(STATUS "Will compile for Blocks/GCD and other Apple sweetness...")
  set(HAVE_DISPATCH 1)
  add_definitions("-stdlib=libc++")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++")
  exec_program("sysctl -n machdep.cpu.features" OUTPUT_VARIABLE CPU_FEATURES)

else()
  # Use openmp on non OSX Systems
  find_package(OpenMP REQUIRED)
   if (OPENMP_FOUND)
     message(STATUS "OpenMP is configured")
     set(HAVE_OPENMP 1)
     set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
     set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
     set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
     file(READ "/proc/cpuinfo" _cpuinfo)
     string(REGEX REPLACE ".*flags[ \t]*:[ \t]+([a-zA-Z0-9_ ]+).*" "\\1" CPU_FEATURES "${_cpuinfo}")
  endif()
endif()


# normalise features before searching
string(TOUPPER ${CPU_FEATURES} CPU_FEATURES)
message("${CPU_FEATURES}")

# linux need threads... 
# find_package(Threads)
# target_link_libraries(sdm ${CMAKE_THREAD_LIBS_INIT})


# runtime library
add_library(sdm database.cpp)


