#pragma once

#include <stdlib.h>
#include "sdmconfig.h"
#include "sdmstatus.h"

/**
 * dual purpose header for c library
 */


typedef void* database_t;
typedef void* space_t;
typedef void* vector_t;
typedef void* symbol_t;
typedef void* topology_t;
typedef SDM_VECTOR_ELEMENT_TYPE vectordata_t[SDM_VECTOR_ELEMS];
//typedef void* vectordata_t; // make this fixed array?

#ifdef __cplusplus
extern "C" {
#endif
  
const status_t sdm_open_database(const char* filename, size_t size, size_t maxsize, database_t*);
const status_t sdm_close_database(const database_t db);

const status_t sdm_get_space(const database_t, const char* spacename, space_t*);
const status_t sdm_ensure_space(const database_t, const char* spacename, space_t*);

  //const status_t sdm_get_symbol(const space_t, const char* symbolname, symbol_t*); 
const status_t sdm_ensure_symbol(const space_t, const char* symbolname, symbol_t*);

const status_t sdm_get_vector(const space_t, const char* vectorname, vector_t*); 

const status_t sdm_read_vector(const vector_t, vectordata_t* vdata);
const status_t sdm_write_vector(const vector_t, vectordata_t vdata);

const status_t sdm_get_topology(const space_t, const vectordata_t, topology_t* topo);


#ifdef __cplusplus
}
#endif
