#include <stdio.h>
#include "sdm.h"
#include "timer.h"

////////////////////////////////////////////////////
/// sdm library api use case development and testing


#define SAFE_CALL(_fn) { apit_t r = (_fn); if (r.type == Fail || r.type == Error) perror("(_fn)"), abort(); else r } 

int main(int argc, char** argv) {

  timer_init(); // init high resolution timer
  

  uint64_t t1 = absolute_time();
  api_t r = sdm_vspace();
  uint64_t t2 = absolute_time();
  
  printf("--------------------------------------------\n");
  printf("vspace constructor\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  vector_space vs = r.value.vector_space_v;

  t1 = absolute_time();
  r = sdm_vspace_allocate(vs, 8);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_allocate\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  t1 = absolute_time();
  r = sdm_vspace_get_vector(vs, 7);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_get_vector\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  const vector v = r.value.vector_v;
  
  t1 = absolute_time();
  r = sdm_vspace_neighbourhood(vs, v, 0.5, 1., 20);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_topology: %zu\n", r.value.size_v);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  t1 = absolute_time();
  r = sdm_vspace_neighbourhood(vs, v, 0.5, 1., 20);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_topology: %zu\n", r.value.size_v);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  t1 = absolute_time();
  r = sdm_vspace_capacity(vs);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_capacity: %zu\n", r.value.size_v);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  t1 = absolute_time();
  r = sdm_vspace_get_vector(vs, 17);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vspace_get_vector\n");
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  const vector u = r.value.vector_v;
  
  assert (!(u == v)); // remeber we restrict vectors they must not overlap either!  

  // just use distance for semantic equality u - v
  t1 = absolute_time();
  r = sdm_vector_distance(u, v);
  t2 = absolute_time();
  printf("--------------------------------------------\n");
  printf("vector_distance:\t %zu\n", r.value.size_v);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  

  
  return 0;
}
