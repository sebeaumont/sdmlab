// unit tests for api lib

#include "../rtl/csdm.h"
#include <iostream>

// declare error handlers

void die(error_t e) {
  std::cerr << "ERROR:" << e.code << "@" << e.message << "?" << e.reason  << std::endl;
  std::abort();
}


// test

int main(int argc, char** argv) {
  
  std::cout << "open db...";
  const sdm_database db = guard(sdm_open_database("test.sdm", 700, 1400), die);
  
  // don't really like how we can unwrap willy nilly here...
  std::cout << db.either.right << std::endl;
  
}
