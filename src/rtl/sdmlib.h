#ifndef __SDMLIB_H__
#define __SDMLIB_H__

/**
 * dual purpose c/"c in c++" header for sdmlib library C API
 * UC: make this c99/language-c friendly!
 */

#include "sdmconfig.h"
#include "sdmstatus.h"

// caveat: these pointer types are opaque to the capi so as not to
// provide a dependency on complex c++ classes in the implmentation:

// N.B. they are not currently validated by the library adapter
// except via static_cast on input.

typedef void* database_t;
typedef void* space_t;
typedef void* vector_t;
typedef void const * symbol_t;

// concrete types that are marshalled into caller space

typedef struct {
  const char* symbol;
  double metric;
  double density;
} point_t;

typedef SDM_VECTOR_ELEMENT_TYPE vectordata_t;
typedef size_t basis_t; //XXX rename basis_t? 
typedef size_t card_t;


#ifdef __cplusplus
extern "C" {
#endif
  /**
   * ... -> database_t 
   */
  const status_t sdm_database(const char* filename,
                              size_t size,
                              size_t maxsize,
                              database_t*);

  /**
   * database_t -> ...
   */
  
  const status_t sdm_database_close(const database_t db);

  const status_t sdm_database_get_space(const database_t,
                                        const char* spacename,
                                        space_t*);

  const status_t sdm_database_ensure_space(const database_t,
                                           const char* spacename,
                                           space_t*);
  
  /* convenience fns which may afford some internal optimizations for
     interpreters and similar clients however these always incur a
     spacename lookup which may be trivial: certainly O(1) but will entail
     space creation and symbol creation and insertion at first
     occurence, high performance applications are encouraged to use
     more direct api's below. */
  
  const status_t sdm_database_ensure_space_symbol(const database_t,
                                                  const char* spacename,
                                                  const char* symbolname,
                                                  symbol_t* symbol);

  const status_t sdm_database_ensure_symbol(const database_t,
                                            const space_t,
                                            const char* symbolname,
                                            symbol_t* symbol);
  
  // TODO high level training api  or this in client?  

  /**
   * space_t -> ...
   */
  
  const status_t sdm_space_get_vector(const space_t,
                                      const char* vectorname,
                                      vector_t*);
  
  const status_t sdm_space_get_symbol(const space_t,
                                      const char* symbolname,
                                      symbol_t*); 

  const status_t sdm_space_get_symbols(const space_t,
                                       const char* prefix,
                                       const char** names);


  const card_t sdm_space_get_topology(const space_t s,
                                      const vectordata_t* v,
                                      const double metric_lb,
                                      const double density_ub,
                                      const unsigned card_ub,
                                      point_t* t);


  /**
   * symbol_t ->
   */

  const status_t sdm_symbol_get_basis(const symbol_t symbol,
                                      basis_t* basis);
  

  /**
   * vector_t -> ...
   */

  const status_t sdm_vector_load(const vector_t,
                                 vectordata_t* vdata);
  
  const status_t sdm_vector_store(const vector_t,
                                  vectordata_t vdata);



#ifdef __cplusplus
}
#endif
#endif // __SDMLIB_H__
