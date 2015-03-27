#pragma once
#include <boost/interprocess/managed_mapped_file.hpp>
#include <boost/optional.hpp>
#include <feature_space.hpp>
#include <map>

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

    ////////////////////////
    // gc, heap management
    ///////////////////////
    
  private:
    // runtime memoizes pointers to named spaces to optimize vector resolution 
    space* ensure_space_by_name(const std::string&); 
    
    // lifetime data 
    segment_t heap;
    std::map<const std::string, space*> spaces; // space pointer
  };
}
