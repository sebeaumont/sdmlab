#pragma once
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <map>

#include <feature_space.hpp>

namespace gecko {

  using namespace gecko::vspace;
  typedef bip::managed_mapped_file segment_t;
  
  class runtime {

  public:

    // this is vspace implementation (fully featured)
    typedef feature_space<unsigned long, 256, 16, segment_t> space;

    // constructor to initialize heap
    runtime(const std::size_t initial_size, const std::size_t max_size, const char* mmf);

    // no copy or move semantics
    runtime(const runtime&) = delete;
    runtime(runtime&&) = delete;
    const runtime& operator=(const runtime&) = delete;
    const runtime& operator=(runtime&&) = delete;

    ///////////////////
    // named vectors //
    ///////////////////

    // get named vector
    boost::optional<const space::vector&> get_vector(const std::string&, const std::string&);

    // find by prefix
    std::pair<space::vector_iterator, space::vector_iterator> search_vectors(const std::string&, const std::string&);

    // create new vector
    void add_vector(const std::string&, const std::string&);
    
    // properties

    // binary operations
    void superpose(const std::string&, const std::string&, const std::string&, const std::string&);
  
    // measurement
    float similarity(const std::string&, const std::string&, const std::string&, const std::string&);
    
    // neighbourhood
    

    // deletion
    
    //////////////////////
    // space operations //
    //////////////////////

    std::pair<space*, std::size_t> get_space_by_name(const std::string&);

    bool destroy_space(const std::string&);

    std::vector<std::string> get_named_spaces();
    
    ///////////////////////////
    // gc, heap utilities etc.
    ///////////////////////////

    bool grow_heap_by(const std::size_t&);

    bool compactify_heap();

    
  private:
    // runtime memoizes pointers to named spaces to optimize vector resolution 
    space* ensure_space_by_name(const std::string&); 

    ////////////////////
    // lifetime state //
    ////////////////////
    
    // constructed
    segment_t heap;
    const char* heapimage;
    // read through cache
    std::map<const std::string, space*> spaces; // used space cache

  };
}
