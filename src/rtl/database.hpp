// Copyright (c) 2015 Simon Beaumont - All Rights Reserved
// runtime.hpp - runtime api interface

#pragma once
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <map>

#include "../mms/symbol_space.hpp"
#include "../util/fast_random.hpp"
#include "runtime_exceptions.hpp"

#include <iostream>

namespace molemind { namespace sdm {

  namespace bip = boost::interprocess;
  using namespace molemind;
  
  class database {

    typedef bip::managed_mapped_file segment_t;
    
  public:

    /////////////////////////////////
    /// mms symbol_space definition
    /////////////////////////////////

    /// type of space implementation -- todo make this platform independant...
    
    typedef sdm::mms::symbol_space<unsigned long long, 265, 16, segment_t> space;

    
    /// constructor to initialize file mapped heap
    
    explicit database(const std::size_t initial_size, const std::size_t max_size, const std::string& mmf);

    // no copy or move semantics

    database() = delete;
    database(const database&) = delete;
    database(database&&) = delete;
    const database& operator=(const database&) = delete;
    const database& operator=(database&&) = delete;

    /// destructor sanely syncs file mapped heap
    
    ~database();
    

    /// get named symbol
    boost::optional<const space::symbol&> get_symbol(const std::string&, const std::string&);

    /// get named vector
    boost::optional<space::vector&> get_vector(const std::string& sn, const std::string& vn);


    /// find by prefix
    std::pair<space::symbol_iterator, space::symbol_iterator> search_symbols(const std::string&, const std::string&);

    /// create new symbol
    boost::optional<const std::size_t> add_symbol(const std::string&, const std::string&);
    
    /// vector properties
    double density(const std::string&, const std::string&);


    /// vector binary operations
    void superpose(const std::string&, const std::string&, const std::string&, const std::string&);
  
    /// vector measurement
    double similarity(const std::string&, const std::string&, const std::string&, const std::string&);
    
    /// neighbourhood
    database::space::topology neighbourhood(const std::string& sn, const std::string& snv, const std::string& vn, double p, double d, std::size_t n);    

    /// deletion
    
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

    
    // randomise a vector 
    void randomize_vector(boost::optional<space::vector&>, float);

    // 
    
  private:
    
    // database memoizes pointers to named spaces to optimize symbol resolution 
    space* ensure_space_by_name(const std::string&); 
  
    ////////////////////
    // lifetime state //
    ////////////////////
    
    // constructed
    segment_t heap;
    const std::string heapimage;
    // read through cache
    std::map<const std::string, space*> spaces; // used space cache
    // randomizer
    random::index_randomizer irand;
  };
  }}
