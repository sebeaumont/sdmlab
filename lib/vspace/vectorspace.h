/* vectorspace.h - Copyright (c) 2105 Simon Beaumont - All Rights Reserved. See LICENSE for details. */

#ifndef __VECTORSPACE_H__
#define __VECTORSPACE_H__

/* operations on vectorspace as a disributed SIMD array */

#include "vectors.h"

#ifdef HAVE_DISPATCH
#include <dispatch/dispatch.h>
#endif


/* vector space - a segmented array of vectors */
// xxx address arithmetic might benefit from powers of 2 for these xxx
#define VS_SEGMENT_SIZE 131072
#define VS_MAX_SEGMENTS 8

typedef struct {
  vector_t vectors[VS_SEGMENT_SIZE];
} vector_space_segment_t;

typedef struct {
  vector_space_segment_t* segments[VS_MAX_SEGMENTS];
  size_t n_segments;
  //size_t n_vectors;
} vector_space_t;

typedef vector_space_t* vector_space;


/////////////////////////
/// memory management ///
/////////////////////////


/* segment allocator -- simple malloc version */
static inline vector_space_segment_t* segment_allocate(const vector_space vs) {
  return malloc(sizeof(vector_space_segment_t)); 
}

/* segment deallocate */
static inline void segment_deallocate(vector_space_segment_t* seg) {
  free(seg);
}

/* allocate vector_space segment header */
static inline vector_space vector_space_init() {
  vector_space vs =  malloc(sizeof(vector_space_t));
  if (vs != NULL) {
    vs->n_segments = 0;
    //vs->n_vectors = 0;
  }
  return vs;
}

/* incremental allocator 1-segment is smallest unit of allocation */
static inline size_t vector_space_allocate(vector_space vs, const size_t n_segments) {
  // allocate up to n_segments
  #pragma nounroll
  #pragma clang loop vectorize(disable) 
  for (size_t i = 0; i < n_segments && vs->n_segments < VS_MAX_SEGMENTS; ++vs->n_segments, ++i) {
    vector_space_segment_t* vseg = segment_allocate(vs);
    if (vseg == NULL) return i;
    else vs->segments[vs->n_segments] = vseg;
  }
  return n_segments;
}

/* free a vectorspace i.e. deallocate all segments */
static inline void vector_space_free(vector_space vs) {
  #pragma nounroll
  #pragma clang loop vectorize(disable) 
  for (size_t i = 0; i < vs->n_segments; ++i) {
    segment_deallocate(vs->segments[i]);
  }
}


///////////////////
/// vectorspace ///
///////////////////

/* vector_space properties */
static inline size_t vector_space_capacity(const vector_space vs) {
  return vs->n_segments * VS_SEGMENT_SIZE;
}

/* unchecked vector access */
static inline vector get_vector(const vector_space vs, const size_t i) {
  // is this address arithmetic fast? I have a fast mod for division by powers of 2 somewhere
  return &vs->segments[i/VS_SEGMENT_SIZE]->vectors[i%VS_SEGMENT_SIZE];
}


/* 
   WIP: parallelised SIMD operations on entire vectorspace
   1. distribute by segments (n_cores) on cpu (treat as separate arrays on gpu?) 
   2. accumulate number of matching targets in parallel scan
   3. allocate smallest set of scores and sort in main thread 
*/

typedef struct {
  float similarity;
  float density;
  size_t vid;
} score_t;

typedef struct {
  //score_t* scores;
  float* scores; // XXX testing hack...
  size_t n_scores;
} scores_t;


/* compute neighbourhood of a vector */

static inline const scores_t neighbourhood(const vector_space vs,
                                           const vector u,
                                           const float p,
                                           const float d,
                                           const size_t n) {

  // 1. allocate working memory
  const size_t m = vector_space_capacity(vs);
  // could be very large -- do we keep track of used vectors? should we heap allocate?
  float *work = malloc(sizeof(float)*2*m);
  //float work[2*m];
  assert(work != NULL); // XXX ooh er missus
  
  // TODO keep track of global count of vectors meeting p, d thresholds if this is can be non-divergent
  //      this will ease memory allocation for scores
  
  //// parallel block ////
  
#ifdef HAVE_DISPATCH
  
  dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
  dispatch_apply(m, queue, ^(size_t i) {
      const vector v = get_vector(vs, i);
      const float rho = vector_density(v);
      const float mu = vector_similarity(u, v);
      work[i*2] = rho;
      work[i*2+1] = mu;
    });
  dispatch_release(queue);

#else

#pragma omp parallel for
  for (size_t i=0; i < m; ++i) {
    const vector v = get_vector(vs, i);
    work[i*2] = vector_density(v);
    work[i*2+1] = vector_similarity(v, u);
  }
#endif
  //// end parallel block ////

  // TODO parallel loop reduction?
  size_t targets = 0;
  #pragma unroll
  for (unsigned i = 0; i < m; ++i)
    targets += (work[i*2] < d  && work[i*2+1] > p) ? 1 : 0;
  
  //XXX...
  //std::vector<score<T>> scores;
  //scores.reserve(m); // TODO BM
  
  // filter amd sort
  /*
  for (std::size_t i=0; i < m; ++i) {
    double rho = work[i*2];
    double sim = work[i*2+1]
    if (rho <= d && mu >= p) {
      score<T> score;
      score.ptr = vectors.get_link(i);
      score.density = rho; //work[i*2];
      score.similarity = sim; //work[i*2+1];
      scores.push_back(score);
    }
  }
  
  delete[] work;
  // sort the scores in similarity order
  sort(scores.begin(), scores.end());
  const std::size_t ns = scores.size();
  
  // chop off (long) tail before serialising -- is it worth it?
  scores.erase(scores.begin() + ((n < ns) ? n : ns), scores.end()); // TODO benchmark this
  return scores;
  */
  scores_t scores;
  scores.n_scores = targets;
  scores.scores = work;
  return scores;
}

#endif // __VECTORSPACE_H__
