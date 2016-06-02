// c unit tests for api lib

#include "../rtl/csdm.h"
#include <stdio.h>

// declare error handlers

void die(error_t e) {
  // 
  //std::cerr << "ERROR:" << e.code << "@" << e.message << "?" << e.reason  << std::endl;
  //std::abort();
  printf("ERROR:%d %s %s\n", e.code, e.message, e.reason);
  exit(e.code);
}


// test

int main(int argc, char** argv) {
  
  printf("open db...");
  const sdm_database db = sdm_database_guard(sdm_open_database("test.sdm", 700, 1400), die);
  
  // don't like how we can unwrap willy nilly here...
  // e.g.
  printf("%p\n", db.value.right);
  
  either n = sdm_guard(sdm_space_cardinality(db, "People"), die);
  printf("%d\n", n.value.right.value.as_nat);
}
