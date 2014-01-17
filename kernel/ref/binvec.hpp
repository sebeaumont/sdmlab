//
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//  Internal Research Use Only
//

#include <cstddef>

/* 
The idea here is to provide naive and simple implemetations to see
how our compilers and other otpmizations perform and to make this
basic set of operations portable and JIT compilable
*/

namespace dsm {

  template <typename T, std::size_t N>
  struct binary_vector final {

    // defined types
    typedef T vector_element_t;
    typedef vector_element_t* const vector;
    typedef binary_vector* const bvector;

    // storage
    alignas(128) vector_element_t elements[N];
    


    /* vector popcount */
    
    inline const unsigned int count() {
      unsigned int count = 0;
      for (unsigned int i=0; i < N; ++i)
        count += __builtin_popcount(elements[i]);
      return count;
    }

    //////////////////////////////////////////////////////////////////////
    //
    // vector ops N.B. it is required that u =! v in all of these
    // operations i.e. at least if u == v then they are undefined due to
    // the restriction of the respective pointers.
    //
    //////////////////////////////////////////////////////////////////////

    /* unitize vector */
    
    void unit() {
      for (unsigned int i=0; i < N; ++i) {
        elements[i] = -1;
      }
    }
    

    /* zero vector */
    
    void zero(bvector v) {
      for (unsigned int i=0; i < N; ++i) {
        elements[i] = 0;
      }
    }

    /* TODO: randomize */



    ///////////////////////
    // binary operations //
    ///////////////////////

    
    /* add accumulate (u |= v) in bvectorspace is or */
    
    void adda(const bvector __restrict__ u) {
      for (unsigned int i=0; i < N; ++i) {
        elements[i] |= u->elements[i];
      }
    }
    
    /* subtraction  u = u & ~v */
    
    void suba(const bvector __restrict__ u) {
      for (unsigned int i=0; i < N; ++i) {
        elements[i] &= ~u->elements[i];
      }
    }
   
    /* multiplication u = u ^ v */
    
    void mula(const bvector __restrict__ u) {
      for (unsigned int i=0; i < N; ++i) {
        elements[i] ^= u->elements[i];
      }
    }
    

    //////////////////
    // vector metrics
    //////////////////

    /* vector density is weight/capacity or hamming_weight by dimensions in this case */
    
    const double density() {
      return count() / (double) (N * sizeof(vector_element_t) * 8);
    }

    
    /* distance for binary vectorspace: count(u^v) */

    const unsigned int distance(const bvector __restrict__ u) {
      
      vector_element_t tmp[N];
      unsigned int count = 0;
  
      for (unsigned int i=0; i < N; ++i) {
        tmp[i] = elements[i] ^ u->elements[i];
        count += __builtin_popcount(tmp[i]);
      }  
      return count;
    }
    

    /* inner for binary vectorspace: count(u&v) */
    
    const unsigned int inner(const bvector __restrict__ u) {
      
      vector_element_t tmp[N];
      unsigned int count = 0;
  
      for (register unsigned int i=0; i < N; ++i) {
        tmp[i] = elements[i] & u->elements[i];
        count += __builtin_popcount(tmp[i]);
      }
      return count;
    }


    /* countsum for binary vectorspace: count(u|v) */
    
    const unsigned int countsum(const bvector __restrict__ u) {
  
      vector_element_t tmp[N];
      unsigned int count = 0;
      
      for (unsigned int i=0; i < N; ++i) {
        tmp[i] = elements[i] | u->elements[i];
        count += __builtin_popcount(tmp[i]);
      }
      return count;
    }

    
    /* similarity */
    
    const double similarity(const bvector __restrict__ u) {
      unsigned int dims = N * sizeof(vector_element_t) * 8;
      return 1.0 - (distance(u) / (double) dims);
    }
    
  };
}

