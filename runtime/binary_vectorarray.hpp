#ifndef __BINARY_VECTORARRY_H__
#define __BINARY_VECTORARRY_H__

#include "vectorarray.hpp"

namespace dsm {
  
  typedef vectorarray<unsigned, 1024> base_vectorarray;
  
  class binary_vectorarray : public base_vectorarray {
  public:

    /* stream printer */
    friend std::ostream& operator<< (std::ostream& stream, const binary_vectorarray& vs) {
      return stream << "<vectorarray n: " << vs.n_vectors
                    << " vsiz: " << vs.vector_size
                    << " dims: " << vs.dimensions()
                    << " name: " << vs.region_name
                    << " size: " << MB(vs.region_size) << " MB>";
    }

    /* constructors */
    binary_vectorarray() : base_vectorarray() {};
    binary_vectorarray(const char* file) : base_vectorarray(file) {};
    binary_vectorarray(const char* file, const std::size_t n) : base_vectorarray(file, n) {};

    /* dimensions are number of bits in vector */
    inline const std::size_t dimensions() const { return vector_els * sizeof(vector_element_t) * 8; }

  };
}
#endif
