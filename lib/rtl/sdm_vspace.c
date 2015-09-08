/* vectors.c */

#include "sdm.h"

/* vectorspace api/module */
// TODO boxed types on api calls?


api_t sdm_vspace(void) {
  api_t ret;

  vector_space_t* vs = vector_space_init();
  if (vs == NULL) {
    ret.type = Fail;
    ret.value.errno_v = errno;
  } else {
    ret.type = VectorSpace;
    ret.value.vector_space_v = vs;
  }
  return ret;
}

api_t sdm_vspace_free(const vector_space vs) {
  api_t ret;
  vector_space_free(vs);
  ret.type = Void;
  ret.value.uint_v = 0;
  return ret;
}

api_t sdm_vspace_allocate(const vector_space vs, const size_t n_segments) {
  api_t ret;
  const size_t n = vector_space_allocate(vs, n_segments);
  ret.type = Size;
  ret.value.size_v = n;
  return ret;
}

api_t sdm_vspace_get_vector(const vector_space vs, const size_t id) {
  api_t ret;
  if (id > vector_space_capacity(vs)) {
    ret.type = Fail;
    ret.value.errno_v = OUT_OF_BOUNDS;
  } else {
    ret.type = Vector;
    ret.value.vector_v = get_vector(vs, id);
  }
  return ret;
}

api_t sdm_vspace_neighbourhood(const vector_space vs, const vector u, const float p, const float d, const size_t n) {
  api_t ret;
  scores_t scores = neighbourhood(vs, u, p, d, n);
  // XXX not fully implemented...
  ret.type = Size;
  ret.value.size_v = scores.n_scores;
  return ret;
}

api_t sdm_vspace_capacity(const vector_space vs) {
  api_t ret;
  size_t n = vector_space_capacity(vs);
  ret.type = Size;
  ret.value.size_v = n;
  return ret;
}

api_t sdm_vector_distance(const vector restrict u, const vector restrict v) {
  api_t ret;
  size_t n = vector_distance(u, v);
  ret.type = Size;
  ret.value.size_v = n;
  return ret;
}
