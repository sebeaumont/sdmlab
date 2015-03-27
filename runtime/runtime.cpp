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
  
  ////////////////////////////////////////////
  // runtime constructor to initialize heap //
  ////////////////////////////////////////////

  runtime::runtime(const std::size_t initial_size, const std::size_t max_size, const char* mmf) :
    heap(bip::open_or_create, mmf, initial_size), heapimage(mmf) {}

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

  //////////////////////
  /// space management
  //////////////////////
  
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

  // lookup a space by name

  std::pair<runtime::space*, std::size_t> runtime::get_space_by_name(const std::string& name) {
    return heap.find<space>(name.c_str());
  }

  // destroy it permanently
  
  bool runtime::destroy_space(const std::string& name) {
    return heap.destroy<space>(name.c_str());
  }

  // lookup all spaces in the segment manager
  std::vector<std::string> runtime::get_named_spaces() {
    std::vector<std::string> names;
    
    typedef segment_t::const_named_iterator const_named_it;
    const_named_it named_beg = heap.named_begin();
    const_named_it named_end = heap.named_end();

    for(; named_beg != named_end; ++named_beg){
      const segment_t::char_type *name = named_beg->name();
      std::size_t name_len = named_beg->name_length();
      names.push_back(std::string(name, name_len));
      // constant void pointer to the named object
      //const void *value = named_beg->value();
    }
    return names;
  }

  
  ////////////////////////
  // gc, heap management
  ////////////////////////

  bool runtime::grow_heap_by(const std::size_t& extra_bytes) {
    // mapped_file grow
    return heap.grow(heapimage, extra_bytes);
  }

  bool runtime::compactify_heap() {
    // mapped_file shrink_to_fit -- compact
    return heap.shrink_to_fit(heapimage);
  }

  
}
