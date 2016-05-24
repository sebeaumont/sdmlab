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

/*
 Under construction
 ------------------ 
 */

/************************
 * Data types in the API
 ************************/

typedef char* string_t;

typedef unsigned int nat_t;

typedef double real_t;

typedef struct { string_t name; real_t distance; real_t density; } point_t;

typedef struct { point_t* pointset; nat_t card; } topology_t;

typedef struct {} error_t;

typedef void* database_t;

// tagged union to represent sum type for return values of api functions

typedef enum {
  REAL_T,
  NAT_T,
  TOPOLOGY_T,
  DATABASE_T,
  VOID_T,
  ERROR_T
} tag_t;



// no no no not one great big sum type we need several Either types

typedef struct {
  tag_t type;
  union {
    real_t as_real;
    nat_t as_nat;
    topology_t as_topo;
    database_t as_database;
  } value;
} result_t;


// type constructors

static inline result_t real(double x) {
  result_t r;
  r.type = REAL_T;
  r.value.as_real = x;
  return r;
}

static inline result_t nat(nat_t x) {
  result_t r;
  r.type = NAT_T;
  r.value.as_nat = x;
  return r;
}

static inline result_t topology(topology_t x) {
  result_t r;
  r.type = TOPOLOGY_T;
  r.value.as_topo = x;
  return r;
}

static inline result_t database(database_t x) {
  result_t r;
  r.type = DATABASE_T;
  r.value.as_database = x;
  return r;
}

// result unwrappers?

// either

typedef enum {
  LEFT_T,
  RIGHT_T
} either_t;

// API type

typedef struct {
  either_t type;
  union {
    result_t right;
    error_t left;
  } value;
} sdm_t;


static inline sdm_t left(error_t x) {
  sdm_t e;
  e.type = LEFT_T;
  e.value.left = x;
  return e;
}

static inline sdm_t right(result_t x) {
  sdm_t e;
  e.type = RIGHT_T;
  e.value.right = x;
  return e;
}


/***********************
 * Functions in the API
 ***********************/

#ifdef __cplusplus
extern "C" {
#endif

  inline const sdm_t dsm_major_version() { return right(nat(SDM_VERSION_MAJOR)); }
  inline const sdm_t dsm_minor_version() { return right(nat(SDM_VERSION_MINOR)); }
  
  const sdm_t dsm_open_database(string_t name, nat_t initial_size, nat_t max_size);
  
  const sdm_t dsm_get_neighbourhood(string_t name,
                                       string_t space,
                                       real_t similarity_lower_bound,
                                       real_t density_upper_bound,
                                       nat_t cardinality_upper_bound);
  
  

#ifdef __cplusplus
}
#endif // cplusplus

//#endif // __DSMLIB.H__
