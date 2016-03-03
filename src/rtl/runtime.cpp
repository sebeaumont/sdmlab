// Copyright (c) 2015, 2016 Simon Beaumont - All Rights Reserved

// implement runtime methods - API under construction as we really haven't got failure semantics nailed yet!
// be nice if we could not use exceptions and use maybe/boost::optional but really we need Either/...

#include "runtime.hpp"

namespace sdm {
  
  ////////////////////////////////////////////
  // runtime constructor to initialize heap //
  ////////////////////////////////////////////

  runtime::runtime(const std::size_t initial_size, const std::size_t max_size, const std::string& mmf) :
    heap(bip::open_or_create, mmf.c_str(), initial_size),
    heapimage(mmf),
    irand(random::index_randomizer(space::vector::dimensions)) {
    
    // pre-cache spaces (and workaroud some weirdness)
      for (std::string spacename: get_named_spaces())
          ensure_space_by_name(spacename);
    }

  
  ///////////////////
  // named symbols //
  ///////////////////

  // properties

  float runtime::density(const std::string& sn, const std::string& vn) {
    boost::optional<const space::symbol&> v = ensure_space_by_name(sn)->get(vn);
    return 0.0; //v.density();
  }

  // get named symbol

  boost::optional<const runtime::space::symbol&>
  runtime::get_symbol(const std::string& sn, const std::string& vn) {
    return get_space_by_name(sn)->get(vn);
  }

  boost::optional<runtime::space::vector&> runtime::get_vector(const std::string& sn, const std::string& vn) {
    return get_space_by_name(sn)->get_vector(vn);
  }

  // find by prefix
  
  std::pair<runtime::space::symbol_iterator, runtime::space::symbol_iterator>
  runtime::search_symbols(const std::string& sn, const std::string& vp) {
    return get_space_by_name(sn)->search(vp);
  }
  
  // all symbols
  /*
  runtime::get_symbols(const std::string& sn) {
    std::symbol<space::symbol> symbols;
    for (spacev :get_space_by_name(sn)
  
  }
   */
  // create new symbol

  boost::optional<const std::size_t> runtime::add_symbol(const std::string& sn, const std::string& vn) {
    return ensure_space_by_name(sn)->insert(vn);
  }

  
  // operations
  
  void
  runtime::superpose(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::symbol&> v = ensure_space_by_name(snv)->get(vn);
    // rhs must exist
    boost::optional<const space::symbol&> u = get_symbol(snu, un);
    // TODO if v and u superpose else throw a notfound exception
  }

  // measurement
  float
  runtime::similarity(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
    boost::optional<const space::symbol&> v = get_symbol(snv, vn);
    boost::optional<const space::symbol&> u = get_symbol(snu, un);
    // TODO 
    return 0.0;
  }

  // neighbourhood


  // deletion


  // low level randomize
  void runtime::randomize_vector(boost::optional<space::vector&> v, float p) {
    std::size_t n = floor(p * space::vector::dimensions);
    std::vector<unsigned>& ilist = irand.shuffle();
    // xxx can we avoid this copy? maybe pass the iterator instead?
    std::vector<unsigned> bitlist(ilist.begin(), ilist.begin() + n);
    v->setbits(bitlist);
  }
  
  //////////////////////
  /// space management
  //////////////////////
  
  // create and manage named symbols by name -- space constructor does find_or_construct on segment
  // runtime memoizes pointers to spaces to speed up symbol resolution 
  
  inline runtime::space* runtime::ensure_space_by_name(const std::string& name) {
    // lookup in cache
    auto it = spaces.find(name);
    
    if (it == spaces.end()) {
      // delegate find_or_construct to symbol_space
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
  
  runtime::space* runtime::get_space_by_name(const std::string& name) {
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
      if (name[0] != '_')
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
