#include "csdm.h"
#include "database.hpp"
#include <iostream>

using namespace molemind;

// unclean unwrap


const sdm_database sdm_open_database(string_t name, nat_t init_size, nat_t max_size) {
  /*
  */
  sdm_database r;

  try {
    auto db = new sdm::database(init_size, max_size, name);
    r.type = RIGHT_T;
    r.value.right = db;
    
  } catch (const std::exception& e) {
    r.type = LEFT_T;
    r.value.left = error("opening database", e.what(), 0);
  }
  
  return r;
}

/*
 try {
 auto db = new sdm::database(init_size, max_size, name);
 } catch (const std::exception& e) {
 // XXX under construction!!!
 return error("opening database", e.what(), 0);
 }
 */

// all these functions need to carry the aditional database parameter in order to dispatch
// to c++ methods


const either sdm_space_cardinality(sdm_database db, string_t name) {
  nat_t n = 0; // STUB
  return right(nat(n));
}

const either sdm_neighbourhood(string_t name,
                                 string_t spacename,
                                 real_t similarity_lower_bound,
                                 real_t density_upper_bound,
                                 nat_t card_upper_bound) {
  // 
  topology_t t;                 // XXX 
  t.card = card_upper_bound;    // XXX
  t.pointset = 0;               // XXX 
  return right(topology(t));
}

