// implementation of sdmlib c interface library
#include "sdmlib.h"
#include "database.hpp"

using namespace molemind::sdm;

const status_t sdm_open_database(const char* filename, size_t size, size_t maxsize, database_t* db) {
  try {
    *db = new database::database(size, maxsize, std::string(filename));
    return AOK;
  } catch (const std::exception& e) {
    fprintf(stderr, "RUNTIME EXCEPTION: %s\n", e.what());
    return ERUNTIME;
  }
}

const status_t sdm_close_database(const database_t db) {
  delete static_cast<database::database*>(db);
  return AOK;
}

const status_t sdm_get_space(const database_t db, const char* spacename, space_t* space) {
  database::space* sp = static_cast<database::database*>(db)->
    get_space_by_name(std::string(spacename));
  if (sp == nullptr) return ESPACE;
  *space = sp;
  return AOK;
}

const status_t sdm_ensure_space(const database_t db, const char* spacename, space_t* space) {
  try {
    database::space* sp = static_cast<database::database*>(db)->
      ensure_space_by_name(std::string(spacename));
    if (sp == nullptr) return ERUNTIME; //?
    *space = sp;
    return AOK;
  } catch (const std::bad_alloc& e) {
    return EMEMORY;
  } catch (...) {
    return ERUNTIME;
  }
}


const status_t sdm_ensure_space_symbol(const database_t db,
                                       const char* spacename,
                                       const char* symbolname,
                                       symbol_t* sym) {
  auto dp = static_cast<database::database*>(db);
    // may create a space
  auto space = dp->ensure_space_by_name(spacename);
  if (!space) return ERUNTIME;
  // already exists?
  auto res = space->get_symbol_by_name(symbolname);

  if (res) {
    *sym = &(*res);
    return AOLD; // existing
  }
  // else
  try {
    // use insert to create a new symbol with elemental "fingerprint"
    database::space::inserted_t p = space->insert(symbolname, dp->randomidx().shuffle());
    
    if (p.second) {
      // insert successful:
      // N.B. the returned symbol reference is to an *immutable* entry in the index
      // i.e. const database::space::symbol& s = *(p.first);
      *sym = &(*(p.first));
      return ANEW; // created
      // do we need a new error for this?
    } else return ERUNTIME; // something in the index stopped us inserting! 
    
  } catch (boost::interprocess::bad_alloc& e) {
    // XXX: here is where we can try and grow the heap
    return EMEMORY; // 'cos we ran out of memory!
  } catch (const std::exception& e) {
    fprintf(stderr, "RUNTIME EXCEPTION: %s\n", e.what());
    return ERUNTIME;
  }
}


//
// ensure a symbol exists in space - may therefore entail creation and
// insertion of a new symbol
// 
const status_t sdm_ensure_symbol(const database_t db,
                                 const space_t space,
                                 const char* symbolname,
                                 symbol_t* sym) {

  auto dp = static_cast<database::database*>(db);
  auto sp = static_cast<database::database::space*>(space);
    

  auto res = sp->get_symbol_by_name(symbolname);
  // see if symbolname already exists
  if (res) {
    *sym = &(*res);
    return AOLD; // existing
  }
  // else
  try {
    // use insert to create a new symbol with elemental "fingerprint"
    database::space::inserted_t p = sp->insert(symbolname, dp->randomidx().shuffle());
    
    if (p.second) {
      // insert successful:
      // N.B. the returned symbol reference is to an *immutable* entry in the index
      // i.e. const database::space::symbol& s = *(p.first);
      *sym = &(*(p.first));
      return ANEW; // created
      // do we need a new error for this?
    } else return ERUNTIME; // something in the index stopped us inserting! 
    
  } catch (boost::interprocess::bad_alloc& e) {
    // XXX: here is where we can try and grow the heap
    return EMEMORY; // 'cos we ran out of memory!
  } catch (const std::exception& e) {
    fprintf(stderr, "RUNTIME EXCEPTION: %s\n", e.what());
    return ERUNTIME;
  }
}

//
// retreive a vector reference from space
//
const status_t sdm_get_vector(const space_t space,
                              const char* symbolname,
                              vector_t* vec) {
  
  auto maybe_vector = static_cast<database::database::space*>(space)->
    get_vector_by_name(std::string(symbolname));
  if (!maybe_vector) return ESYMBOL;
  *vec = &(*maybe_vector); // deref boost optional
  return AOK;
}

//
// retrieve a symbol reference from space
//
const status_t sdm_get_symbol(const space_t space,
                              const char* symbolname,
                              symbol_t* sym) {
  // lost in space...
  auto sp = static_cast<database::database::space*>(space);
    
  // already exists?
  auto maybe_symbol = sp->get_symbol_by_name(symbolname);

  if (maybe_symbol) {
    *sym = &(*maybe_symbol);
    return AOLD; 
  } else {
    return ESYMBOL;
  }
}

// TODO
const status_t sdm_get_symbols(const space_t space,
                               const char* prefix,
                               const char** names) {
  return EUNIMPLEMENTED;
}

// TODO xxx next xxx
const status_t sdm_get_basis(const symbol_t symbol,
                             basis_t const ** basis) {
  // get underlying array data from symbol
  auto sp = static_cast<const database::space::symbol*>(symbol);
  *basis = sp->basis().data();
  return AOK;
}

const status_t sdm_get_topology(const space_t s, const vectordata_t, topology_t* topo) {
  return EUNIMPLEMENTED;
}
  
// xxx UC xxx
const status_t sdm_load_vector(const vector_t v,
                               vectordata_t* vdata) {
  auto vector = static_cast<database::database::space::vector*>(v);
  vector->copy_me(vdata);
  return AOK;
}


const status_t sdm_store_vector(const vector_t v, vectordata_t vdata) {
  return EUNIMPLEMENTED;
}

