#include "csdm.h"

const result_t dsm_get_neighbourhood(string_t name, string_t space, real_t similarity_lower_bound, real_t density_upper_bound, nat_t cardinality_upper_bound) {
  topology_t t;                     // XXX go figure...
  t.card = cardinality_upper_bound; // XXX
  t.pointset = 0;                   // XXX never!
  return topology(t);
}
