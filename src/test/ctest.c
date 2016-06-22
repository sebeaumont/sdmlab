// c unit tests for api lib

#include "../rtl/csdm.h"
#include <stdio.h>
#include <stdlib.h>

// declare error handlers

void die(sdm_error_t e) {
  // 
  //std::cerr << "ERROR:" << e.code << "@" << e.message << "?" << e.reason  << std::endl;
  //std::abort();
  printf("FATAL:%d %s %s\n", e.code, e.message, e.reason);
  exit(e.code);
}

void warning(sdm_error_t e) {
  printf("ERROR:%d %s %s\n", e.code, e.message, e.reason);
}

// test

int main(int argc, char** argv) {
  
  printf("open db...");
  const sdm_database db = sdm_database_guard(sdm_open_database("test.sdm", 700, 1400), die);
 
  //////
  // don't like how we can unwrap willy nilly here...
  // e.g.
  printf("%p\n", db.value.right);
  
  // make this a nat_t as if space nex we get 0
  either n = sdm_guard(sdm_space_cardinality(db, "Test"), die);
  printf("%d\n", n.value.right.value.as_nat);
  
  either t = sdm_guard(sdm_neighbourhood(db, "foo", "Test", 0.0, 1.0, 20), warning);
  topology_t top = t.value.right.value.as_topo;
  printf("%d\n", top.card);
  
}
