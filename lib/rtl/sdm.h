/* sdm.h */
#ifndef __SDM_H__
#define __SDM_H__

#include "vectorspace.h"

///////////////////
// rtl functions //
///////////////////

int sdm_rtl_init_prng(void);


/////////////////
// vectorspace //
/////////////////

const vector_space sdm_vspace(void);
const size_t sdm_vspace_allocate(const vector_space, const size_t);
const vector sdm_vspace_vector(const vector_space, const size_t);
void sdm_vspace_free(const vector_space);

// XXX need a proper neighbourhood type for this and where is it
// (de-)allocated can use an opaque pointer if we have some kind of gc hooks in ffi
const size_t sdm_vspace_neighbourhood(const vector_space, const vector, const float, const float, const size_t);

const size_t sdm_vspace_capacity(const vector_space);


/////////////
// vectors //
/////////////

/* destructive vector operations */

void sdm_vector_ones(const vector u);
void sdm_vector_zero(const vector u);
void sdm_vector_random(const vector u, float p);

void sdm_vector_superpose(const vector restrict u, const vector restrict v);
void sdm_vector_subtract(const vector restrict u, const vector restrict v);
void sdm_vector_multiply(const vector restrict u, const vector restrict v);

/* vector functions  */

const size_t sdm_vector_norm(const vector u);
const size_t sdm_vector_distance(const vector restrict u, const vector restrict v);
const size_t sdm_vector_inner(const vector restrict u, const vector restrict v);
const size_t sdm_vector_countsum(const vector restrict u, const vector restrict v);
const float sdm_vector_similarity(const vector restrict u, const vector restrict v);
const float sdm_vector_density(const vector u);


#endif // __SDM_H__

