#pragma once

// vector_space.hpp - Copyright (c) 2016 Simon Beaumont. All Rights Reserved.

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/containers/vector.hpp>


namespace sdm {
  namespace mms {
    
    namespace bip = boost::interprocess;

    // TODO rewrite this as a bip::vector of vectors of ElementType and NElems
    // currently a vector of VectorType
    
    template <typename VectorElementType, std::size_t NElems, typename Allocator>

    //struct vector_space : public vector_vector_t {
    struct vector_space {

      // how to forward reference this?
      typedef VectorElementType element_t;
      typedef Allocator memory_manager_t;
      
      // vector of vectors
      typedef bip::allocator<void, memory_manager_t> void_allocator_t;

      typedef bip::allocator<element_t, memory_manager_t> element_allocator_t;
      
      typedef bip::vector<element_t, element_allocator_t> vector_t;
      typedef bip::allocator<vector_t, memory_manager_t> vector_allocator_t;
      
      typedef bip::vector<vector_t, vector_allocator_t> vector_vector_t;

      
     
      // c'tor
      vector_space(const void_allocator_t& a) : vector_vector_t(a) {}

      // c'tor with reservation size
      vector_space(const::size_t n, const void_allocator_t& a) : vector_vector_t(a) {
        this->reserve(n);
      }

      const std::size_t push_new() {
        vector_t v;
        this->push_back(v);
        return -1;
      }

      //vector_vector_t vectors;
    };
  }
}
