//
//  space.hpp
//  alligator
//
//  Created by Informatics on 09/03/2013.
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//

#ifndef alligator_space_hpp
#define alligator_space_hpp
#include <vector>

#ifdef HAVE_DISPATCH
#include <dispatch/dispatch.h>
#endif

#include "entropy.hpp"
#include "symbol_table.hpp"
#include "binary_vector.hpp"
#include "valloc.hpp"
#include "score.hpp"


namespace dsm {
  
 
  template <typename T, std::size_t M>
  struct vectorspace final {

    typedef binary_vector<unsigned int, M> base_vector_t;
    typedef typename base_vector_t::vector_element_t base_vector_element_t;
    
    vectorspace(const std::size_t& n) : vectors(n) {}
    
    // no copy
    vectorspace(const vectorspace& h) = delete;
    vectorspace& operator=(const vectorspace& h) = delete;
    
    // no move
    vectorspace(vectorspace&& v) = delete;
    vectorspace& operator=(const vectorspace&& v) = delete;
    
    // get a vector from the space
    inline base_vector_t* new_vector(T* rp) {
      base_vector_t* v = vectors.next_vector(rp);
      return v;
    }

    inline const std::size_t next_vector_id() {
      return vectors.size();
    }
    
    inline base_vector_t* get_vector(const std::size_t id) {
      return vectors.get_vector(id);
    }

    inline const std::size_t size() {
      return vectors.size();
    }
     
    // randomly set n bits in a vector
     
    inline void randomize_vector(base_vector_t* u, std::size_t n) {
     
      // this is all quite expensive but gives good distribution
      std::vector<std::size_t> indexes = randomizer.indexes();
     
      for (std::size_t i = 0; i < n; ++i) {
        std::size_t b = randomizer.uniform_distribution(sizeof(base_vector_element_t)* 8);
        u->elements[indexes[i%M]] |= (0x1 << b);
      }
    }
     
     
    // randomize bits with probability p
     
    inline void randomize_vector(base_vector_t* u, const float p) {
      randomize_vector(u, (std::size_t)floor(sizeof(base_vector_element_t) * M * 8 * p));
    }
    

    // the neighbourhood of a vector

    inline const std::vector<score<T>> neighbourhood(const base_vector_t* u, float p, float d, std::size_t n) {

      const std::size_t m = vectors.size();
      // allocate working memory - TODO try this on stack
      auto work = new double[m*2];
      
      //// parallel block ////
      #ifdef HAVE_DISPATCH
      dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
      dispatch_apply(m, queue, ^(size_t i) {
        const base_vector_t* vp = vectors.get_vector(i);
        work[i*2] = vp->density();
        work[i*2+1] = vp->similarity(u);
      });
      dispatch_release(queue);

      #else
      #pragma omp parallel for 
      for (std::size_t i=0; i < m; ++i) {
        const base_vector_t* vp = vectors.get_vector(i);
        work[i*2] = vp->density();
        work[i*2+1] = vp->similarity(u);
      }
      #endif
      //// end parallel block ////
      
      std::vector<score<T>> scores;
      scores.reserve(m); // TODO BM
      
      // filter amd sort
      for (std::size_t i=0; i < m; ++i) {
        double rho = work[i*2];
        double sim = work[i*2+1];
        if (rho < d && sim > p) {
          score<T> score;
          score.ptr = vectors.get_link(i);
          score.density = rho; //work[i*2];
          score.similarity = sim; //work[i*2+1];
          scores.push_back(score);
        }
      }

      delete[] work;
      sort(scores.begin(), scores.end());
      const std::size_t ns = scores.size();
      
      scores.erase(scores.begin() + ((n < ns) ? n : ns), scores.end()); // TODO benchmark this
      return scores;
    }

    private:
      utl::random::vrand<utl::random::cmr_adapter,M> randomizer;
      varray<base_vector_t,T> vectors;  
         
  };
  
}
#endif
