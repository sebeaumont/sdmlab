/**
 * Copyright (c) Simon Beaumont 2013-2016 - All Rights Reserved.
 * 
 * This file defines the c interface to dsm for all api users
 */

#pragma once

#ifndef SDM_VERSION_MAJOR
#define SDM_VERSION_MAJOR 6
#define SDM_VERSION_MINOR 1
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 Under construction
 ------------------ 
 */

/************************
 * Data types in the API
 ************************/

typedef const char* string_t;

typedef unsigned int nat_t;

typedef double real_t;

typedef int integer_t;

// product types

typedef struct { string_t name; real_t distance; real_t density; } point_t;

typedef struct { point_t* pointset; nat_t card; } topology_t;

typedef struct { string_t message; string_t reason; integer_t code; } error_t;

typedef const void* database_t;

// type tags

typedef enum {
  REAL_T,
  NAT_T,
  TOPOLOGY_T,
  DATABASE_T,
  VOID_T,
  ERROR_T
} tag_t;


///////////////////////////////////
// value_t sum type of value types

typedef struct {
  tag_t type;
  union {
    real_t as_real;
    nat_t as_nat;
    topology_t as_topo;
    database_t as_database;
  } value;
} value_t;


// simple value_t constructors

static inline value_t real(double x) {
  value_t r;
  r.type = REAL_T;
  r.value.as_real = x;
  return r;
}

static inline value_t nat(nat_t x) {
  value_t r;
  r.type = NAT_T;
  r.value.as_nat = x;
  return r;
}

static inline value_t topology(topology_t x) {
  value_t r;
  r.type = TOPOLOGY_T;
  r.value.as_topo = x;
  return r;
}

static inline value_t database(database_t x) {
  value_t r;
  r.type = DATABASE_T;
  r.value.as_database = x;
  return r;
}


// error type

static inline error_t error(string_t m, string_t r, nat_t c) {
  error_t e;
  e.message = m;
  e.reason = r;
  e.code = c;
  return e;
}

// result unwrappers?

// either

typedef enum {
  LEFT_T,
  RIGHT_T
} either_t;

// either: sum type of value or error
// type either = value_t | error_t
  
typedef struct {
  either_t type;
  union {
    value_t right;
    error_t left;
  } value;
} either;


static inline either left(error_t x) {
  either e;
  e.type = LEFT_T;
  e.value.left = x;
  return e;
}

static inline either right(value_t x) {
  either e;
  e.type = RIGHT_T;
  e.value.right = x;
  return e;
}


// alternatively we need several Either types
// most evolved
// with specific error handling code...

typedef struct {
  either_t type;
  union {
    error_t left;
    database_t right;
  } either;
} sdm_database;


// e.g.

static inline sdm_database database2(database_t x) {
  sdm_database r;
  if (x == nullptr) {
    r.type = LEFT_T;
    r.either.left = error("database2", "null", 10);
  } else {
    r.type = RIGHT_T;
    r.either.right = x;
  }
  return r;
}

// vs. generic
static inline either database3(database_t x) {
  if (x == nullptr) {
    return left(error("database3", "null", 20));
  } else {
    return right(database(x));
  }
}


static inline const either guard(either e) {
  if (e.type == LEFT_T) {
      // raise error
  }
  return e;
}
  
typedef void (*error_handler)(error_t e);
  
static inline const sdm_database guard(sdm_database d, error_handler fail) {
  if (d.type == LEFT_T) fail(d.either.left);
  return d;
}

  


/***********************
 * Functions in the API
 ***********************/


inline const either sdm_major_version() { return right(nat(SDM_VERSION_MAJOR)); }
inline const either sdm_minor_version() { return right(nat(SDM_VERSION_MINOR)); }
  
const sdm_database sdm_open_database(string_t name, nat_t initial_size, nat_t max_size);
const either sdm_space_cardinality(sdm_database& db, string_t name);
  
const either sdm_get_neighbourhood(string_t name,
                                  string_t space,
                                  real_t similarity_lower_bound,
                                  real_t density_upper_bound,
                                  nat_t cardinality_upper_bound);
  
  

#ifdef __cplusplus
}
#endif // cplusplus

//#endif // __DSMLIB.H__
