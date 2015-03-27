// implement runtime methods
#include "runtime.hpp"
/*
#include <boost/algorithm/string.hpp>

// helper functions
inline std::vector<std::string> parse_qualified_name(std::string& s) {
  std::vector<std::string> strs;
  boost::split(strs, s, boost::is_any_of("\t "));
  return strs;
}
*/

namespace gecko {

  // constructor to initialize heap
  runtime::runtime(const std::size_t initial_size, const std::size_t max_size, const char* mmf) :
    heap(bip::open_or_create, mmf, initial_size) {}

  ///////////////////
  // named vectors //
  ///////////////////
  
  // get named vector
  boost::optional<const runtime::space::vector&> runtime::get_vector(const std::string& sn, const std::string& vn) {
    return ensure_space_by_name(sn)->get(vn);
  }
  
  // find by prefix
  
  std::pair<runtime::space::vector_iterator, runtime::space::vector_iterator> runtime::search_vectors(const std::string& sn, const std::string& vp) {
    return ensure_space_by_name(sn)->search(vp);
  }
  
  // create new vector
  void runtime::add_vector(const std::string& sn, const std::string& vn) {
    ensure_space_by_name(sn)->insert(vn);
  }
  
  // properties

  // operations
  
  void runtime::superpose(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::vector&> v = ensure_space_by_name(snv)->get(vn);
    boost::optional<const space::vector&> u = ensure_space_by_name(snu)->get(un);
    // TODO if v and u superpose else throw a notfound exception
  }

  // measurement
  float runtime::similarity(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::vector&> v = ensure_space_by_name(snv)->get(vn);
    boost::optional<const space::vector&> u = ensure_space_by_name(snu)->get(un);
    // TODO 
    return 0.0;
  }

  // neighbourhood


  // deletion

  
  // gc, heap management

  // create and manage named vectors by name -- space constructor does find_or_construct on segment
  // runtime memoizes pointers to spaces to optimize vector resolution 
  
  inline runtime::space* runtime::ensure_space_by_name(const std::string& name) {
    // lookup in cache
    auto it = spaces.find(name);
    
    if (it == spaces.end()) {
      // need to delegate find_or_construct to heap
      space* sp = new space(name, heap);
      spaces[name] = sp;
      return sp;
      
    } else {
      // used cached space
      return it->second;
    }
  }

}
