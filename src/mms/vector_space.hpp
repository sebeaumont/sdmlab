#pragma once

// vector_space.hpp - Copyright (c) 2016 Simon Beaumont. All Rights Reserved.

#include <boost/interprocess/allocators/allocator.hpp>
#include <boost/interprocess/containers/vector.hpp>


namespace sdm {
  namespace mms {
    
    namespace bip = boost::interprocess;
    
    template <typename T, typename M>
    struct vector_space : public bip::vector<T, bip::allocator<T, M>> {
    
      //
      typedef bip::vector<T, bip::allocator<T, M>> vector_array_t;
      typedef bip::allocator<bip::vector<T, bip::allocator<T, M>>, M> vector_array_allocator_t;
      typedef typename bip::allocator<void, M> void_allocator_t;
      
      // c'tor
      vector_space(const void_allocator_t& a) : vector_array_t(a) {}
    };
  }
}
