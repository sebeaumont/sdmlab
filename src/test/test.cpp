// unit tests for runtime class

#include "runtime.hpp"

// test
int main(int argc, char** argv) {

  using namespace gecko;

  // sizing
  const std::size_t ini_size = 700 * 1024 * 1024;
  const std::size_t max_size = 700 * 1024 * 1024;
  
  // 1. create runtime system
  runtime rts(ini_size, max_size, "testheap.img");

    
  return 1;
}
