// Copyright (c) 2015 Simon Beaumont - All Rights Reserved
// runtime.hpp - runtime api interface

#pragma once
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <map>

#include "../mms/symbol_space.hpp"
#include "../util/fast_random.hpp"
//#include "runtime_exceptions.hpp"

#include <iostream>

namespace molemind { namespace sdm {

  namespace bip = boost::interprocess;
  using namespace molemind;
  
  /***********************************************************************
   ** Database type provides the API for the SDM implementation
   **
   ** The failure model says that no exceptions (except system failure)
   ** should leak out of here. We use boost optional to indicate failure 
   ** where it should be handled by the caller, this is mainly concerned 
   ** with symbol lookups but memory outages can occur in a number of 
   ** functions... <tbc>
   ***********************************************************************/
  
  class database {

    /// memory manager for spaces within the database
    typedef bip::managed_mapped_file segment_t;
    
  public:

    /////////////////////////////////
    /// mms symbol_space definition
    /////////////////////////////////

    /// type of space implementation determines the type and number of elements and sparsity
    // XXX all of which can be portably calculated or re-templated from more abstract notions
    
    typedef sdm::mms::symbol_space<unsigned long long, 265, 16, segment_t> space;

    
    /// constructor to initialize file mapped heap
    
    explicit database(const std::size_t initial_size, const std::size_t max_size, const std::string& filepath);

    /// currently no copy or move semantics

    database() = delete;
    database(const database&) = delete;
    database(database&&) = delete;
    const database& operator=(const database&) = delete;
    const database& operator=(database&&) = delete;

    /// destructor will tidy up sanely - fear not
    
    ~database();
    
    /// create new symbol
    /// return tristate: failed, false->existed, true->added (be careful unwrapping) see:
    /// http://www.boost.org/doc/libs/1_60_0/libs/optional/doc/html/boost_optional/a_note_about_optional_bool_.html

    boost::optional<const bool> add_symbol(const std::string& space_name, const std::string& symbol_name) noexcept;

    /// search for symbols starting with prefix
    
    std::pair<space::symbol_iterator, space::symbol_iterator> search_symbols(const std::string& space_name, const std::string& symbol_prefix) noexcept;
    
    
    //////////////////////
    /// heap utilities ///
    //////////////////////

    bool grow_heap_by(const std::size_t&) noexcept;

    bool compactify_heap() noexcept;

    /// heap metrics
    
    inline std::size_t heap_size() noexcept { return heap.get_size(); }
    inline std::size_t free_heap() noexcept { return heap.get_free_memory(); }
    inline bool check_heap_sanity() noexcept { return heap.check_sanity(); }
    inline bool can_grow_heap() noexcept { return (heap.get_size() < maxheap); }
    
 
    
    /////////////////////////
    /// vector properties ///
    /////////////////////////
    
    /// get vector density
    boost::optional<const double> density(const std::string& space_name, const std::string& vector_name) noexcept;


    /////////////////////////////////////////////////////
    /// effectful learning operations on target vectors
    /////////////////////////////////////////////////////
    
    /// add or superpose
    void superpose(const std::string& ts, const std::string& tn,
                   const std::string& ss, const std::string& sn) noexcept;
  
    /// subtract
    void subtract(const std::string& ts, const std::string& tn,
                  const std::string& ss, const std::string& sn) noexcept;
    
    /// multiply
    void multiply(const std::string& ts, const std::string& tn,
                  const std::string& ss, const std::string& sn) noexcept;
    
    /// TODO exponents
    
    /// TODO shifts and other permutations of bases
    
    
    ////////////////////////
    /// vector measurement
    ////////////////////////
    
    /// simlilarity (unit distance)
    boost::optional<double> similarity(const std::string&, const std::string&,
                                       const std::string&, const std::string&) noexcept;

    /// inner product
    boost::optional<double> inner(const std::string&, const std::string&,
                                  const std::string&, const std::string&) noexcept;

    
    /// toplogy of n nearest neighbours satisfying p, d constraints
    boost::optional<database::space::topology> neighbourhood(const std::string& target_space,
                                                             const std::string& source_space,
                                                             const std::string& source_name,
                                                             double similarity_lower_bound,
                                                             double density_upper_bound,
                                                             std::size_t cardinality_upper_bound) noexcept;
    /// TODO negation
    
    /// TODO query algebra
    
    
    ////////////////////////
    /// space operations ///
    ////////////////////////
    
    bool destroy_space(const std::string&) noexcept;

    std::vector<std::string> get_named_spaces() noexcept;
    
    boost::optional<std::size_t> get_space_cardinality(const std::string&) noexcept;
    
       
    
  protected:
    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // xxx not sure to expose these as is yet but handy and efficient for wrappers and dsls however
    // xxx no doubt these will go away once efficient portable serialisaions have been wrought.
    
    /// get named symbol
    boost::optional<const space::symbol&> get_symbol(const std::string& space_name, const std::string& symbol_name) noexcept;

    /// get named vector
    boost::optional<space::vector&> get_vector(const std::string& space_name, const std::string& vector_name) noexcept;


    /// randomise a vector
    void randomize_vector(boost::optional<space::vector&> vector, double p) noexcept;

    /// ones
    void ones_vector(boost::optional<space::vector&> v) noexcept;
    
    /// zeros
    void zeros_vector(boost::optional<space::vector&> v) noexcept;

    // get space
    space* get_space_by_name(const std::string&); 

    /// database memoizes pointers to named spaces to optimize symbol resolution 
    space* ensure_space_by_name(const std::string&); 
    

    
  private:    
   
    ////////////////////
    // lifetime state //
    ////////////////////

    // constructed
    std::size_t inisize;
    std::size_t maxheap;
    segment_t heap;
    const std::string heapimage;
    // read through cache
    std::map<const std::string, space*> spaces; // used space cache
    // randomizer
    random::index_randomizer irand;
 
  };
  }}
