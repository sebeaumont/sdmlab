// unit tests for runtime class
#include "runtime.hpp"

// test
int main(int argc, char** argv) {

  using namespace gecko;

  // sizing
  const std::size_t heap_size = 700 * 1024 * 1024;
  
  // 1. create runtime system
  runtime rts(heap_size, heap_size, "testheap.dat");

  // 2. load some named vectors
  std::ifstream ins("testvectors.txt");
        
  if (ins.good()) {
    std::string fline;
    int n = 0;

    while(std::getline(ins, fline)) {
      boost::trim(fline);
      //mytable.insert(fline);
      n++;
    }
  }

  // 3. lookup said vectors
  
  return 1;
}
