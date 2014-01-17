//
//  valloc.hpp
//  alligator
//
//  Created by Informatics on 11/03/2013.
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//

#ifndef alligator_valloc_hpp
#define alligator_valloc_hpp
#include <vector>
#include "exceptions.hpp"

namespace dsm {

  /* global array of vectors */
  template<typename V, typename P>
  struct varray final {
    
    varray(std::size_t n) {
      vectors.reserve(n);
      owners.reserve(n);
    }
    
    ~varray() {
      std::cout << "varray destroyed" << std::endl;
    }
    
    inline std::size_t size() const { return vectors.size(); }
    
    // we deal in pointers to avoid any copying which would be expensive
    inline V* get_vector(const std::size_t& i) {
      return &vectors[i];
    }

    inline const P* get_link(const std::size_t& i) {
      return owners[i];
    }
    
    // vector allocation
    inline V* next_vector(P* p) {
      // relies on the container calling the copy constructor
      // but hopefully ensures that vectors are laid out contiguously
      V v;
      vectors.push_back(v);
      owners.push_back(p);
      return get_vector(size()-1);
    }
    
    // no copy
    varray(const varray& h) = delete;
    varray& operator=(const varray& h) = delete;

    // no move
    varray(varray&& v) = delete;
    varray& operator=(const varray&& v) = delete;
    
  private:
    std::vector<V> vectors;
    std::vector<P*> owners;
  };
}
#endif
