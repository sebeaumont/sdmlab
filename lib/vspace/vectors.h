/* vectors.h */

#ifndef __VECTORS_H__
#define __VECTORS_H__
#include <stdlib.h>
#include <assert.h>

// XXX architecture dependent -- TODO: set by build */
#define VELEMENT_64
#define HAVE_DISPATCH
// XXX

/* vector implementation constants */
#define V_SIZE_BYTES 4096                      // vector size in bytes
#define V_SIZE_BITS (V_SIZE_BYTES * 8)         // vector size in bits


#ifdef VELEMENT_64
typedef unsigned long long vector_element_t;   // vector element type
#else
typedef unsigned vector_element_t;   // vector element type
#endif

/* actual array number of elements */
#define V_ELEMENTS (V_SIZE_BYTES/sizeof(vector_element_t))

/* vector type */
typedef struct {
  vector_element_t els[V_ELEMENTS];
} vector_t;

typedef vector_t* vector;


///////////////////////


/* basic operations on vectors - we want to be sure of unrolling loops here */
static inline void vector_superpose(vector u, vector v) {
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] |= v->els[i];
  }
}

/* count for binary vectors */
static inline size_t vector_count(vector u) {
  size_t count = 0;
  #pragma unroll
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    #ifdef VELEMENT_64
    count += __builtin_popcountll(u->els[i]);
    #else
    count += __builtin_popcount(u->els[i]);
    #endif
  }
  return count;
}

/* set all bits */

static inline void vector_ones(vector u) {
  #pragma unroll
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] = -1; 
  }
}

/* clear all bits */

static inline void vector_zeros(vector u) {
 for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] = 0; 
  }
}

static inline float vector_similarity(const vector restrict u, vector restrict v) {
  size_t count = 0;
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    vector_element_t r = u->els[i] ^ v->els[i]; 
    #ifdef VELEMENT_64
    count += __builtin_popcountll(r);
    #else
    count += __builtin_popcount(r);
    #endif
  }
  return 1.0 - count/V_SIZE_BITS;
}

static inline float vector_density(const vector u) {
  return (float) vector_count(u)/V_SIZE_BITS;
}

/* TODO: futrher basic ops and metrics */

#endif /* __VECTORS_H__ */
