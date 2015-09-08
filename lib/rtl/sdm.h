/* sdm.h */
#ifndef __SDM_H__
#define __SDM_H__

#include "api.h"

// vectorspace
api_t sdm_vspace(void);

api_t sdm_vspace_free(const vector_space);

api_t sdm_vspace_allocate(const vector_space, const size_t);

api_t sdm_vspace_get_vector(const vector_space, const size_t);

api_t sdm_vspace_neighbourhood(const vector_space, const vector, const float, const float, const size_t);

api_t sdm_vspace_capacity(const vector_space);

// vectors
api_t sdm_vector_distance(const vector restrict u, const vector restrict v);

#endif // __SDM_H__

