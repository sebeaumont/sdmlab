// Copyright (c) 2015, 2016 Simon Beaumont - All Rights Reserved

/// Public interface to the C++ API

#include "database.hpp"

namespace molemind {

  namespace sdm {
  
    /// constructor to initialize database

    database::database(const std::size_t initial_size,
                       const std::size_t max_size,
                       const std::string& mmf)
      // init slots
      : inisize(initial_size),      // initial size of heap in bytes
        maxheap(max_size),          // maximum size of heap in bytes
        // construct the memory mapped segment for database
        heap(bip::open_or_create, mmf.c_str(), initial_size),
        heapimage(mmf),             // diskimage path
        // initialize PRNG
        irand(random::index_randomizer(space::vector::dimensions)) {
      
      // pre-load space cache (and workaroud some weirdness)
      for (std::string spacename: get_named_spaces())
        ensure_space_by_name(spacename);
    }
    
    
    /// destructor flushes the segment iff sane
    
    database::~database() {
      if (check_heap_sanity()) heap.flush();
    }
    
    
    
    ////////////////////
    /// named vectors // 
    ////////////////////
 
   
    
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
    
    
    /// find symbols by prefix
    
    boost::optional<database::symbol_list> database::prefix_search(const std::string& sn, const std::string& vp) noexcept {
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
    
    /// get existing or create new symbol -- this can cause bad alloc
    /// snd may side-effect the creation of a space and a symbol+vector within it
    
    boost::optional<const bool> database::ensure_symbol(const std::string& sn, const std::string& vn) noexcept {
      auto space = ensure_space_by_name(sn);
      auto sym = space->get_symbol_by_name(vn);
      if (sym) return true; // found
  
      // try and create symbol
      else try {
        // UC... need a random list of indexes
        //std::vector<std::size_t> fp = irand.shuffle();
        database::space::inserted_t p = space->insert(vn, irand.shuffle());
        if (p.second) {
          //const database::space::symbol& s = *(p.first);
          //
          return false;
        } else return boost::none;
        
      } catch (boost::interprocess::bad_alloc& e) {
        return boost::none;
      }
    }
    
    
    // operations
    
    void database::superpose(const std::string& ts, const std::string& tn,
                             const std::string& ss, const std::string& sn) noexcept {
      
      // create spaces if required
      auto tsp = ensure_space_by_name(ts);
      auto ssp = ensure_space_by_name(ss);
      
      // if not found insert and retry!
      boost::optional<space::vector&> v = tsp->get_vector_by_name(tn);
      if (!v) {
        tsp->insert(tn);
        v = tsp->get_vector_by_name(tn);
        assert(v);
      }
      
      // if not found insert and retry!
      boost::optional<space::vector&> u = ssp->get_vector_by_name(sn);
      if (!u) {
        ssp->insert(tn);
        u = tsp->get_vector_by_name(tn);
        assert(u);
      }
  
      v->superpose(*u);
    }
    
        
    
    // measurement
    
    boost::optional<double> database::similarity(const std::string& snv, const std::string& vn,
                                                 const std::string& snu, const std::string& un) noexcept {
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
    

    // init symbol --- needs randomizer
    
    // low level randomize a vector -- writes p * d random bits
    
    void database::randomize_vector(boost::optional<space::vector&> v, double p) noexcept {
      if (v) {
        std::size_t n = floor(p * space::vector::dimensions);
        std::vector<std::size_t>& ilist = irand.shuffle();
        v->setbits(ilist.begin(), ilist.begin() + n);
      }
    }
    
    void database::unit_vector(boost::optional<space::vector&> v) noexcept {
      if (v) v->ones();
    }
      
    void database::zero_vector(boost::optional<space::vector&> v) noexcept {
      if (v) v->zeros();
    }
    
    
    /* END API */
    
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

