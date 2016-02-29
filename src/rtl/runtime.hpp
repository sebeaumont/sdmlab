// Copyright (c) 2015 Simon Beaumont - All Rights Reserved
// runtime.hpp - runtime api interface

#pragma once
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <map>

#include "../mms/symbol_space.hpp"
#include "runtime_exceptions.hpp"

namespace sdm {

  // space and heap memory implementation types
  using namespace sdm::mms;
  typedef bip::managed_mapped_file segment_t;
  
  class runtime {

  public:

    // mms symbol_space implementation definition
    typedef symbol_space<unsigned long long, 265, 16, segment_t> space;

    // constructor to initialize file mapped heap 
    runtime(const std::size_t initial_size, const std::size_t max_size, const std::string& mmf);

    // no copy or move semantics
    runtime() = delete;
    runtime(const runtime&) = delete;
    runtime(runtime&&) = delete;
    const runtime& operator=(const runtime&) = delete;
    const runtime& operator=(runtime&&) = delete;

    ///////////////////
    // named vectors //
    ///////////////////

    // get named symbol
    boost::optional<const space::symbol&> get_symbol(const std::string&, const std::string&);

    // get named vector
    boost::optional<space::vector&> get_vector(const std::string& sn, const std::string& vn);


    // find by prefix
    std::pair<space::symbol_iterator, space::symbol_iterator> search_symbols(const std::string&, const std::string&);

    // create new symbol
    boost::optional<const std::size_t> add_symbol(const std::string&, const std::string&);
    
    // properties
    float density(const std::string&, const std::string&);


    // binary operations
    void superpose(const std::string&, const std::string&, const std::string&, const std::string&);
  
    // measurement
    float similarity(const std::string&, const std::string&, const std::string&, const std::string&);
    
    // neighbourhood
    

    // deletion
    
    //////////////////////
    // space operations //
    //////////////////////

    space* get_space_by_name(const std::string&); // XXX make private and expose space properties?
    
    bool destroy_space(const std::string&);

    std::vector<std::string> get_named_spaces();
    
    ///////////////////////////
    // gc, heap utilities etc.
    ///////////////////////////

    bool grow_heap_by(const std::size_t&);

    bool compactify_heap();

    // heap metrics
    inline std::size_t heap_size() const { return heap.get_size(); }
    inline std::size_t free_heap()  const { return heap.get_free_memory(); }
    inline bool check_heap_sanity() { return heap.check_sanity(); }

    
  private:
    
    // runtime memoizes pointers to named spaces to optimize symbol resolution 
    space* ensure_space_by_name(const std::string&); 

    ////////////////////
    // lifetime state //
    ////////////////////
    
    // constructed
    segment_t heap;
    const std::string heapimage;
    // read through cache
    std::map<const std::string, space*> spaces; // used space cache

  };
}
