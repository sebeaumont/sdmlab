// Copyright (c) 2015, 2016 Simon Beaumont - All Rights Reserved

/// Public interface to the C++ API

#include "database.hpp"

namespace molemind {

  namespace sdm {
  
    /// constructor to initialize database

    database::database(const std::size_t initial_size,
                       const std::size_t max_size,
                       const std::string& mmf)
      // init
      : heap(bip::open_or_create, mmf.c_str(), initial_size),
        heapimage(mmf),
        irand(random::index_randomizer(space::vector::dimensions)) {
      // pre-load space cache (and workaroud some weirdness)
      for (std::string spacename: get_named_spaces())
        ensure_space_by_name(spacename);
    }
    
    
    /// destructor flushes the segment iff sane
    
    database::~database() {
      if (check_heap_sanity()) {
        heap.flush();
        std::cout << "flushed:" << heapimage << std::endl;
      } 
    }
    
        
    ///////////////////
    /// named vectors // 
    ///////////////////
 
    // get named vector
    
    boost::optional<database::space::vector&> database::get_vector(const std::string& sn, const std::string& vn) {
      return get_space_by_name(sn)->get_vector_by_name(vn);
    }
    
    
    // XXX optional
    double database::density(const std::string& sn, const std::string& vn) {
      boost::optional<space::vector&> v = ensure_space_by_name(sn)->get_vector_by_name(vn);
      return v->density();
    }
    
    
    ///////////////////
    /// named symbols //
    ///////////////////
    
    
    // get named symbol
    
    boost::optional<const database::space::symbol&> database::get_symbol(const std::string& sn, const std::string& vn) {
      return get_space_by_name(sn)->get_symbol_by_name(vn);
    }
    
 
    // find symbols by prefix
    
    typedef std::pair<database::space::symbol_iterator, database::space::symbol_iterator> symbol_list;
    
    symbol_list database::search_symbols(const std::string& sn, const std::string& vp) {
      return get_space_by_name(sn)->search(vp);
    }
    
    // all symbols
    /*
      database::get_symbols(const std::string& sn) {
      for (v : get_space_by_name(sn)) ;
      }
    */
    
    // create new symbol

    boost::optional<const std::size_t> database::add_symbol(const std::string& sn, const std::string& vn) {
      return ensure_space_by_name(sn)->insert(vn);
    }
    
    
    // operations
    
    void database::superpose(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
      boost::optional<space::vector&> v = ensure_space_by_name(snv)->get_vector_by_name(vn);
      boost::optional<space::vector&> u = ensure_space_by_name(snu)->get_vector_by_name(un);
      // optional guards? 
      v->superpose(*u);
      
    }
    
    // measurement
    
    double database::similarity(const std::string& snv, const std::string& vn, const std::string& snu, const std::string& un) {
      boost::optional<const space::symbol&> v = get_symbol(snv, vn);
      boost::optional<const space::symbol&> u = get_symbol(snu, un);
      // TODO 
      return 0.0;
    }

    
    // neighbourhood
    database::space::topology database::neighbourhood(const::std::string& sn,
                                                      const std::string& snv, const std::string& vn,
                                                      double p, double d, std::size_t n) {
      //
      auto v = get_space_by_name(snv)->get_vector_by_name(vn);
      return get_space_by_name(sn)->neighbourhood(*v, p, d, n);
    }
    
    // deletion


    // low level randomize a vector -- writes p * d random bits
    void database::randomize_vector(boost::optional<space::vector&> v, float p) {
      std::size_t n = floor(p * space::vector::dimensions);
      std::vector<unsigned>& ilist = irand.shuffle();
      v->setbits(ilist.begin(), ilist.begin() + n);
    }
    
    
    
    
    //////////////////////
    /// space management
    //////////////////////
    
    // create and manage named symbols by name -- space constructor does find_or_construct on segment
    // database memoizes pointers to spaces to speed up symbol resolution 
    
    inline database::space* database::ensure_space_by_name(const std::string& name) {
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
       
       std::pair<database::space*, std::size_t>
       database::get_space_by_name(const std::string& name) {
       return heap.find<space>(name.c_str());
       }
    */
    
    database::space* database::get_space_by_name(const std::string& name) {
      auto it = spaces.find(name);
      if (it == spaces.end())
        throw space_not_found(name); // no exceptions?
      else
        return it->second;
    }
    
    // destroy space permanently
    
    bool database::destroy_space(const std::string& name) {
      return heap.destroy<space>(name.c_str());
    }
    
    // lookup all spaces in the heap/segment manager
    
    std::vector<std::string> database::get_named_spaces() {
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
    
    bool database::grow_heap_by(const std::size_t& extra_bytes) {
      // mapped_file grow
      return heap.grow(heapimage.c_str(), extra_bytes);
    }
    
    bool database::compactify_heap() {
      // mapped_file shrink_to_fit -- compact
      return heap.shrink_to_fit(heapimage.c_str());
    }
    
  }
}

