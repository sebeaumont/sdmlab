#include "csdm.h"
#include "database.hpp"

using namespace molemind;

const result_t sdm_open_database(string_t name, nat_t init_size, nat_t max_size) {
  return database(new sdm::database(init_size, max_size, name));
}

const result_t sdm_space_cardinality(string_t name) {
  nat_t n = 0; // TODO call 
  return nat(n);
}

const result_t sdm_neighbourhood(string_t name,
                                 string_t spacename,
                                 real_t similarity_lower_bound,
                                 real_t density_upper_bound,
                                 nat_t card_upper_bound) {
  // 
  topology_t t;                 // XXX 
  t.card = card_upper_bound;    // XXX
  t.pointset = 0;               // XXX 
  return topology(t);
}

