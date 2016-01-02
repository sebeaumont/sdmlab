/* vectors.c */

#include "sdm.h"

int sdm_rtl_init_prng(void) {
  return init_prng();
}

/* unboxed sdm api */

const vector_space sdm_vspace(void) {
  return vector_space_init();
}

void sdm_vspace_free(const vector_space vs) {
  vector_space_free(vs);
}

const size_t sdm_vspace_allocate(const vector_space vs, const size_t n_segments) {
  return vector_space_allocate(vs, n_segments);
}

const vector sdm_vspace_vector(const vector_space vs, const size_t id) {
  return (id > vector_space_capacity(vs)) ? NULL : get_vector(vs, id);
}

// fix this now
const size_t sdm_vspace_neighbourhood(const vector_space vs, const vector u, const float p, const float d, const size_t n) {
  scores_t scores = neighbourhood(vs, u, p, d, n);
  // XXX not fully implemented...
  return scores.n_scores;
}

const size_t sdm_vspace_capacity(const vector_space vs) {
  return vector_space_capacity(vs);
}


/* destructive vector operations */

void sdm_vector_superpose(const vector restrict u, const vector restrict v) {
  vector_superpose(u, v);
}

void sdm_vector_ones(const vector u) {
  vector_ones(u);
}

void sdm_vector_zero(const vector u) {
  vector_zeros(u);
}

void sdm_vector_random(const vector u, const float p) {
  vector_random(u, p);
}

void sdm_vector_subtract(const vector restrict u, const vector restrict v) {
  vector_subtract(u, v);
}

void sdm_vector_multiply(const vector restrict u, const vector restrict v) {
  vector_multiply(u, v);
}

/* vector functions  */

const size_t sdm_vector_norm(const vector u) {
  return vector_count(u);
}

const size_t sdm_vector_distance(const vector restrict u, const vector restrict v) {
  return vector_distance(u, v);
}

const size_t sdm_vector_inner(const vector restrict u, const vector restrict v) {
  return vector_inner(u, v);
}

const size_t sdm_vector_countsum(const vector restrict u, const vector restrict v) {
  return vector_countsum(u, v);
}


const float sdm_vector_similarity(const vector restrict u, const vector restrict v) {
  return vector_similarity(u, v);
}

const float sdm_vector_density(const vector u) {
  return vector_density(u);
}


