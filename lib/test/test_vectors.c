#include <stdio.h>
#include "vectors.h"
#include "vectorspace.h"
#include "timer.h"

/* test vector space allocator */

int main(int argc, char** argv) {

  vector_space_t* vs = vector_space_init();
  size_t req_segments = VS_MAX_SEGMENTS;

  timer_init(); // init high resolution timer
  
  if (argc > 1) req_segments = (size_t) atol(argv[1]);

  uint64_t t1 = absolute_time();
  const size_t n = vector_space_allocate(vs, req_segments);
  uint64_t t2 = absolute_time();
  
  printf("--------------------------------------------\n");
  printf("requested: %zu segments: got: %zu\n", req_segments, n);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  printf("--------------------------------------------\n");
  printf("vector space write test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  
  for (size_t i=0; i < n; i++) {
    // write all vectors
    for (size_t s=0; s < VS_SEGMENT_SIZE; s++) {
      for (size_t j=0; j < V_ELEMENTS; j++) {
        vs->segments[i]->vectors[s].els[j] = 28091957;
      }
    }
  }
  t2 = absolute_time();
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  printf("--------------------------------------------\n");
  printf("vector space read  test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  for (size_t i=0; i < n; i++) {
    // read all vectors
    for (size_t s=0; s < VS_SEGMENT_SIZE; s++) {
      for (size_t j=0; j < V_ELEMENTS; j++) {
        if (vs->segments[i]->vectors[s].els[j] != 28091957) {
          printf("read test failed: segment: %zu, vector: %zu, element: %zu\n", i, s, j);
          return 1;
        }
      }
    }
  }
  t2 = absolute_time();
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  // WRITE/COUNT
  printf("--------------------------------------------\n");
  printf("vector space write/modulus  test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  size_t gt = 0;
  for (size_t i=0; i < vector_space_capacity(vs); i++) {
    //vector v = _VECTOR(vs, i);
    vector v = get_vector(vs, i);
    //printf("v:%p\n", v);
    vector_ones(v);
    gt += vector_count(v);
  }
  t2 = absolute_time();
  printf("count: %zu\n", gt);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  // TOPOLOGY
  printf("--------------------------------------------\n");
  printf("vspace neighbourhood test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  const scores_t scores = neighbourhood(vs, get_vector(vs, 0), 0.5, 1., 20);
  t2 = absolute_time();
  printf("hits: %zu\n", scores.n_scores);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);

  
  // SCORES
  printf("--------------------------------------------\n");
  printf("vpsace scores test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  float density = 0.0;
  for (size_t i=0; i < vector_space_capacity(vs); ++i) {
    density += scores.scores[i*2];
  }
  t2 = absolute_time();
  printf("density: %E\n", density);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);


  // SCAN
  printf("--------------------------------------------\n");
  printf("vector space scan  test:%zu\n", vector_space_capacity(vs));
  t1 = absolute_time();
  float td = 0.0;
  for (size_t i=0; i < vector_space_capacity(vs); i++) {
    //vector v = _VECTOR(vs, i);
    vector v = get_vector(vs, i);
    //printf("v:%p\n", v);
    td += vector_density(v);
  }
  t2 = absolute_time();
  printf("density: %E\n", td);
  printf("elapsed time (ns): %E\n", (double)(t2 - t1) * conversion_factor);
  
  return 0;
}
