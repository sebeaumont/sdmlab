/* api.h */
#ifndef __API_H__
#define __API_H__

#include <sys/errno.h>
#include "vectorspace.h"

/* api error codes */
typedef enum {
  OK = 0,
  OUT_OF_BOUNDS,
  ALL_FUCKED_UP
} error_t;


/* api return types */
typedef enum {
  Void = 0,
  Fail,
  Error,
  Int,
  Unsigned,
  Double,
  Float,
  Size,
  Vector,
  VectorSpace
} type_t;


/* union of atomic values we can return from api calls corresponding to types */ 

typedef struct {
  union {
    int int_v;
    unsigned int uint_v;
    float float_v;
    double double_v;
    errno_t errno_v;
    vector vector_v;
    size_t size_v;
    vector_space vector_space_v;
    error_t error_v;
  };
} value_t;


/* boxed values we return from api calls */

typedef struct {
  type_t type;
  value_t value;
} api_t;

#endif // __API_H__

