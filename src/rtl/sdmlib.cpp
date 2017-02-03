// implementation of sdmlib c interface library
#include "sdmlib.h"
#include "database.hpp"

using namespace molemind::sdm;

const status_t sdm_open_database(const char* filename, size_t size, size_t maxsize, database_t* db) {
  try {
    *db = new database::database(size, maxsize, std::string(filename));
    return AOK;
  } catch (...) {
    return ERUNTIME;
  }
}

const status_t sdm_close_database(const database_t db) {
  delete static_cast<database::database*>(db);
  return AOK;
}

const status_t sdm_get_space(const database_t db, const char* spacename, space_t* space) {
  database::space* sp = static_cast<database::database*>(db)->get_space_by_name(std::string(spacename));
  if (sp == nullptr) return ESPACE;
  *space = sp;
  return AOK;
}

const status_t sdm_ensure_space(const database_t db, const char* spacename, space_t* space) {
  try {
    database::space* sp = static_cast<database::database*>(db)->ensure_space_by_name(std::string(spacename));
    if (sp == nullptr) return ERUNTIME; //?
    *space = sp;
    return AOK;
  } catch (const std::bad_alloc& e) {
    return EMEMORY;
  } catch (...) {
    return ERUNTIME;
  }
}

/* refactor database...
const status_t sdm_ensure_symbol(const database_t db, const char* spacename, const char* symbolname, symbol_t* sym) {

}
*/


const status_t sdm_get_vector(const space_t space, const char* symbolname, vector_t* vec) {
  auto vector = static_cast<database::database::space*>(space)->get_vector_by_name(std::string(symbolname));
  if (!vector) return ESYMBOL;
  *vec = &(*vector); // boost optional
  return AOK;
}
