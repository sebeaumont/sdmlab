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
      : inisize(initial_size),
        maxheap(max_size),
        heap(bip::open_or_create, mmf.c_str(), initial_size),
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
      } 
    }
    
        
    ////////////////////
    /// named vectors // 
    ////////////////////
 
    /// get named vector
    
    boost::optional<database::space::vector&> database::get_vector(const std::string& sn, const std::string& vn) noexcept {
      auto sp = get_space_by_name(sn);
      return (sp == nullptr) ? boost::none : sp->get_vector_by_name(vn);
    }
    
    
    // fully guarded
    boost::optional<const double> database::density(const std::string& sn, const std::string& vn) noexcept {
      auto sp = get_space_by_name(sn);
      if (sp == nullptr) {
        return boost::none;
      } else {
        auto v = sp->get_vector_by_name(vn);
        if (v) return v->density(); else return boost::none;
      }
    }
    
    
    ////////////////////
    /// named symbols //
    ////////////////////
    
    
    /// get named symbol
    
    boost::optional<const database::space::symbol&> database::get_symbol(const std::string& sn, const std::string& vn) noexcept {
      return get_space_by_name(sn)->get_symbol_by_name(vn);
    }
    
 
    /// find symbols by prefix
    
    boost::optional<database::symbol_list> database::search_symbols(const std::string& sn, const std::string& vp) noexcept {
      auto sp = get_space_by_name(sn);
      if (sp) return sp->search(vp);
      else return boost::none;
    }
    
    // all symbols
    /*
      database::get_symbols(const std::string& sn) {
      for (v : get_space_by_name(sn)) ;
      }
    */
    
    /// create new symbol -- this can cause bad alloc

    boost::optional<const bool> database::add_symbol(const std::string& sn, const std::string& vn) noexcept {
      try {
        // N.B. may side-effect the creation of a space and a symbol/vector
        auto ov = ensure_space_by_name(sn)->insert(vn);
        if (ov) return true;
        else return false;
        
      } catch (boost::interprocess::bad_alloc& e) {
        return boost::none;
      }
    }
    
    
    // operations
    
    void database::superpose(const std::string& ts, const std::string& tn,
                             const std::string& ss, const std::string& sn) noexcept {
      // TODO source and target vectors must exist...
      boost::optional<space::vector&> v = ensure_space_by_name(ts)->get_vector_by_name(tn);
      boost::optional<space::vector&> u = ensure_space_by_name(ss)->get_vector_by_name(sn);
      // optional guards? 
      v->superpose(*u);
    }
    
        
    
    // measurement
    
    boost::optional<double> database::similarity(const std::string& snv, const std::string& vn,
                                                 const std::string& snu, const std::string& un) noexcept {
      boost::optional<const space::symbol&> v = get_symbol(snv, vn);
      boost::optional<const space::symbol&> u = get_symbol(snu, un);
      // TODO 
      return 0.0;
    }

    
    // neighbourhood
    boost::optional<database::space::topology> database::neighbourhood(const::std::string& ts,
                                                                       const std::string& ss, const std::string& sv,
                                                                       double p, double d, std::size_t n) noexcept {
      // all parts must exist
      auto s = get_space_by_name(ss);
      if (s) {
        boost::optional<space::vector&> v = s->get_vector_by_name(sv);
        if (v) return s->neighbourhood(*v, p, d, n);
        else return boost::none;
      } else return boost::none;
    }
        
    // TODO deletion


    // low level randomize a vector -- writes p * d random bits
    void database::randomize_vector(boost::optional<space::vector&> v, double p) noexcept {
      if (v) {
        std::size_t n = floor(p * space::vector::dimensions);
        std::vector<unsigned>& ilist = irand.shuffle();
        v->setbits(ilist.begin(), ilist.begin() + n);
      }
    }
    
    void database::ones_vector(boost::optional<space::vector&> v) noexcept {
      if (v) v->ones();
    }
      
    void database::zeros_vector(boost::optional<space::vector&> v) noexcept {
      if (v) v->zeros();
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
    
    // this is meant to be fast so no optional's here -- we could inline this.
    database::space* database::get_space_by_name(const std::string& name) {
      auto it = spaces.find(name);
      if (it == spaces.end())
        return nullptr;
      else
        return it->second;
    }
    
    // destroy space permanently
    
    bool database::destroy_space(const std::string& name) noexcept {
      return heap.destroy<space>(name.c_str());
    }
    
    // lookup all spaces in the heap/segment manager
    
    std::vector<std::string> database::get_named_spaces() noexcept {
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
    

    boost::optional<std::size_t> database::get_space_cardinality(const std::string& sn) noexcept {
      auto sp = get_space_by_name(sn);
      if (sp) return sp->entries();
      else return boost::none;
    }
    
    ////////////////////////
    // gc, heap management
    ////////////////////////
    
    bool database::grow_heap_by(const std::size_t& extra_bytes) noexcept {
      // mapped_file grow
      // todo unmap heap
      //
      if (heap.grow(heapimage.c_str(), extra_bytes)) {
        // remap
        heap = segment_t(bip::open_only, heapimage.c_str());
        std::cout << "free: " << free_heap() << " max: " << maxheap << " init:" << inisize << " heap:" << heap_size() << std::endl;
        if (check_heap_sanity())
          return true;
      }
      return false;
    }
    
    bool database::compactify_heap() noexcept {
      // mapped_file shrink_to_fit -- compact
      return heap.shrink_to_fit(heapimage.c_str());
    }
    
  }
}

