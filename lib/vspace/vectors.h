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


/////////////////////////////////
// basic operations on vectors //
/////////////////////////////////

/* add or superpose */

static inline void vector_superpose(const vector restrict u, const vector restrict v) {
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] |= v->els[i];
  }
}

/* set all bits */

static inline void vector_ones(const vector u) {
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] = -1; 
  }
}

/* clear all bits */

static inline void vector_zeros(const vector u) {
 #pragma unroll
 #pragma clang loop vectorize(enable) interleave(enable)
 for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] = 0; 
  }
}

/* subtract v from u */

static inline void vector_subtract(const vector restrict u, const vector restrict v) {
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] &= ~v->els[i];
  }
}

static inline void vector_multiply(const vector restrict u, const vector restrict v) {
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    u->els[i] ^= v->els[i];
  }
}

////////////////////
// vector metrics //
////////////////////

/* count for binary vectors */

static inline const size_t vector_count(const vector u) {
  size_t count = 0;

  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    #ifdef VELEMENT_64
    count += __builtin_popcountll(u->els[i]);
    #else
    count += __builtin_popcount(u->els[i]);
    #endif
  }
  return count;
}

static inline size_t vector_distance(const vector restrict u, const vector restrict v) {
  // compute the distance between vectors: u,v --> |u-v|
  // that is count the number of bits where they differ
  size_t distance = 0;

  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    vector_element_t r = u->els[i] ^ v->els[i]; 
    #ifdef VELEMENT_64
    distance += __builtin_popcountll(r);
    #else
    distance += __builtin_popcount(r);
    #endif
  }
  return distance;
}


static inline size_t vector_inner(const vector restrict u, const vector restrict v) {
  size_t count = 0;
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    vector_element_t r = u->els[i] & v->els[i]; 
    #ifdef VELEMENT_64
    count += __builtin_popcountll(r);
    #else
    count += __builtin_popcount(r);
    #endif
  }
  return count;
}


static inline size_t vector_countsum(const vector restrict u, const vector restrict v) {
  size_t count = 0;
  #pragma unroll
  #pragma clang loop vectorize(enable) interleave(enable)
  for (size_t i=0; i < V_ELEMENTS; ++i) {
    vector_element_t r = u->els[i] | v->els[i]; 
    #ifdef VELEMENT_64
    count += __builtin_popcountll(r);
    #else
    count += __builtin_popcount(r);
    #endif
  }
  return count;
}


  
static inline float vector_similarity(const vector restrict u, const vector restrict v) {
  // inverse of the normalized distance
  return 1.0 - vector_distance(u,v)/V_SIZE_BITS;
}

static inline float vector_density(const vector u) {
  return (float) vector_count(u)/V_SIZE_BITS;
}



#endif /* __VECTORS_H__ */
