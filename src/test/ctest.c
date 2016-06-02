// c unit tests for api lib

#include "../rtl/csdm.h"
#include <stdio.h>

// declare error handlers

void die(error_t e) {
  // 
  //std::cerr << "ERROR:" << e.code << "@" << e.message << "?" << e.reason  << std::endl;
  //std::abort();
  printf("ERROR:%d %s %s\n", e.code, e.message, e.reason);
  // exit()/abort
}


// test

int main(int argc, char** argv) {
  
  printf("open db...");
  const sdm_database db = sdm_database_guard(sdm_open_database("/test.sdm", 700, 1400), die);
  
  // don't really like how we can unwrap willy nilly here...
  printf("%p\n", db.value.right);

}
