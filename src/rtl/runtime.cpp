// Copyright (c) 2015 Simon Beaumont - All Rights Reserved

// implement runtime methods - API under construction as we really haven't got failure semantics nailed yet!
// be nice if we could not use exceptions and use maybe/boost::optional but really we need Either/...

#include "runtime.hpp"

namespace sdm {
  
  ////////////////////////////////////////////
  // runtime constructor to initialize heap //
  ////////////////////////////////////////////

  runtime::runtime(const std::size_t initial_size, const std::size_t max_size, const std::string& mmf) :
    heap(bip::open_or_create, mmf.c_str(), initial_size), heapimage(mmf) {
      // new idea to pre-cache spaces (and workaroud some weirdness)
      for (std::string spacename: get_named_spaces())
        ensure_space_by_name(spacename);
    }

  
  ///////////////////
  // named vectors //
  ///////////////////

  // properties

  float
  runtime::density(const std::string& sn, const std::string& vn) {
    boost::optional<const space::vector&> v = ensure_space_by_name(sn)->get(vn);
    return 0.0; //v.density();
  }

  // get named vector

  boost::optional<const runtime::space::vector&>
  runtime::get_vector(const std::string& sn, const std::string& vn) {
    return get_space_by_name(sn)->get(vn);
  }
  
  // find by prefix
  
  std::pair<runtime::space::vector_iterator, runtime::space::vector_iterator>
  runtime::search_vectors(const std::string& sn, const std::string& vp) {
    return get_space_by_name(sn)->search(vp);
  }
  
  // all vectors
  /*
  runtime::get_vectors(const std::string& sn) {
    std::vector<space::vector> vectors;
    for (spacev :get_space_by_name(sn)
  
  }
   */
  // create new vector

  void
  runtime::add_vector(const std::string& sn, const std::string& vn) {
    ensure_space_by_name(sn)->insert(vn);
  }
  
  // operations
  
  void
  runtime::superpose(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::vector&> v = ensure_space_by_name(snv)->get(vn);
    // rhs must exist
    boost::optional<const space::vector&> u = get_vector(snu, un);
    // TODO if v and u superpose else throw a notfound exception
  }

  // measurement
  float
  runtime::similarity(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::vector&> v = get_vector(snv, vn);
    boost::optional<const space::vector&> u = get_vector(snu, un);
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
  
  inline runtime::space*
  runtime::ensure_space_by_name(const std::string& name) {
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
  /* 
  this has weird behaviour -- hangs or throws assersion errors
  so I'm doing a workaround and cache all spaces at rts start up via ensure space_by_name
  probably be quicker...
   
  std::pair<runtime::space*, std::size_t>
  runtime::get_space_by_name(const std::string& name) {
    return heap.find<space>(name.c_str());
  }
  */
  runtime::space*
  runtime::get_space_by_name(const std::string& name) {
    auto it = spaces.find(name);
    if (it == spaces.end())
      throw space_not_found(name);
    else
      return it->second;
  }
  
  // destroy space permanently
  
  bool runtime::destroy_space(const std::string& name) {
    return heap.destroy<space>(name.c_str());
  }

  // lookup all spaces in the heap/segment manager
  
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
    return heap.grow(heapimage.c_str(), extra_bytes);
  }

  bool runtime::compactify_heap() {
    // mapped_file shrink_to_fit -- compact
    return heap.shrink_to_fit(heapimage.c_str());
  }

  
}
