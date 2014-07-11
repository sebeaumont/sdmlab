/*
 * fixup for bogus unistd must be included before anything else 
 */

#ifdef __block
  #undef __block
  #include <unistd.h>
  #define __block __attribute__((__blocks__(byref)))
#else
  #include <unistd.h>
#endif
