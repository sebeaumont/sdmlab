#include <stdio.h>
#include "sdm.h"
#include "timer.h"

////////////////////////////////////////////////////
/// sdm library api use case development and testing



int main(int argc, char** argv) {

  timer_init(); // init high resolution timer
  
  uint64_t t1 = absolute_time();
  int prng = sdm_rtl_init_prng();
  uint64_t t2 = absolute_time();
  
  printf("--------------------------------------------\n");
  printf("rtl prng initialize: %d\n", prng);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  t1 = absolute_time();
  vector_space vs = sdm_vspace();
  t2 = absolute_time();

  
  printf("--------------------------------------------\n");
  printf("vspace constructor\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  t1 = absolute_time();
  const size_t n = sdm_vspace_allocate(vs, 8);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_allocate: %zu\n", n);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  t1 = absolute_time();
  const vector v = sdm_vspace_vector(vs, 7);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_vector\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  // XXX  
  t1 = absolute_time();
  const topo_t m = sdm_vspace_neighbourhood(vs, v, 0.5, 1., 20);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_topology: %zu\n", m.n_points);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  t1 = absolute_time();
  const topo_t l = sdm_vspace_neighbourhood(vs, v, 0.5, 1., 20);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_topology: %zu\n", l.n_points);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  t1 = absolute_time();
  const size_t o = sdm_vspace_capacity(vs);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_capacity: %zu\n", o);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  t1 = absolute_time();
  const vector u = sdm_vspace_vector(vs, 17);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_vector (get)\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  assert (!(u == v)); // remember we restrict vectors N.B. they must not overlap either!  

  // just use distance for semantic equality u - v
  t1 = absolute_time();
  const size_t d = sdm_vector_distance(u, v);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vector_distance:\t %zu\n", d);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  // ones vector
  t1 = absolute_time();
  sdm_vector_ones(u);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vector_ones:\t %zu\n", sdm_vector_norm(u));
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  // zero vector
  t1 = absolute_time();
  sdm_vector_zero(u);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vector_zero:\t %zu\n", sdm_vector_norm(u));
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  // random vector -- N.B. relies on previous zero
  t1 = absolute_time();
  sdm_vector_random(u, 0.001);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vector_random:\t %f\t%zu\n", sdm_vector_density(u), sdm_vector_norm(u));
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  return 0;
}
