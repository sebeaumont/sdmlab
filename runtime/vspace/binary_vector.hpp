#pragma once

#include <boost/interprocess/containers/vector.hpp>

namespace gecko {

  namespace vspace {

    namespace bip = boost::interprocess;

    template <typename T, typename M>
    struct binary_vector : public bip::vector<T, bip::allocator<T, M>> {

      // bit vector types
      typedef bip::vector<T, bip::allocator<T, M>> vector_t;
      typedef bip::allocator<bip::vector<T, bip::allocator<T, M>>, M> bitv_vector_allocator_t;
      typedef typename bip::allocator<void, M> void_allocator_t;

      binary_vector(std::size_t s, const T& i, const void_allocator_t& a) : vector_t(s, i, a) {}
    };
  }
}
