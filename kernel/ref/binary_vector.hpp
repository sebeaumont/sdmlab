//
//  binary_vector.hpp
//  alligator
//
//  Created by Datalligator on 12/03/2013.
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//

#ifndef alligator_binary_vector_hpp
#define alligator_binary_vector_hpp

//#include <tmmintrin.h> // SSSE3
#include <pmmintrin.h> // SSE3
#include <popcntintrin.h>

namespace dsm {
  
  
  template <typename T, std::size_t N>
  struct binary_vector final {
    
    /* defined types */
    typedef T vector_element_t;
    typedef vector_element_t* vector_t;
    
    /* storage */
    alignas(128) vector_element_t elements[N];
    
    /* c'tor*/
    binary_vector() {}
    
    /* gang of five */
    ~binary_vector() {}
    // no copy
    //binary_vector(const binary_vector& v) = delete;
    //binary_vector& operator=(const binary_vector& v) = delete;
    // no move
    //binary_vector(binary_vector&& v) = delete;
    //binary_vector& operator=(const binary_vector&& v) = delete;
    
    
    /* vector ops */
    
    /* add accumulate (u |= v) in binary_vectorspace is or */
    
    inline void adda(const binary_vector* v) {
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        
        __m128 a = _mm_load_ps((float*) &elements[i]);
        __m128 b = _mm_load_ps((float*) &v->elements[i]);
        
        __m128 c = _mm_or_ps(a, b);
        
        // since we are not going to fetch this anytime soon we avoid cache line pollution with store
        _mm_stream_ps((float*)&elements[i], c);
      }
    }
    
    /* subtraction  u = u & ~v */
    
    inline void suba(const binary_vector* v) {
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        __m128i b = _mm_lddqu_si128((const __m128i*)&elements[i]);
        __m128i a = _mm_lddqu_si128((const __m128i*)&v->elements[i]);
        
        /* Computes the bitwise AND of the 128-bit value in b and the bitwise NOT of the 128- bit value in a. */
        __m128i c = _mm_andnot_si128(a, b);
        
        // since we are not going to fetch this anytime soon we avoid cache line pollution with store
        _mm_stream_si128((__m128i*)&elements[i], c);
      }
    }
   
    /* multiplication u = u ^ v */
    
    inline void mula(const binary_vector* v) {
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        __m128 a = _mm_load_ps((float*) &elements[i]);
        __m128 b = _mm_load_ps((float*) &(v->elements[i]));
      
        __m128 c = _mm_xor_ps(a, b);
      
        // not needed yet so stream
        _mm_stream_ps((float*) &elements[i], c);
      }
    }
    
    
    /* unitize vector - todo use instrinsics */
    
    inline void unit() {
      for (std::size_t i=0; i < N; ++i) {
        elements[i] = -1;
      }
    }
    
    /* zero vector */
    
    inline void zero() {
      for (std::size_t i=0; i < N; ++i) {
        elements[i] = 0;
      }
    }
    
    
    /* distance for binary vectorspace: count(u^v) */
    
    inline const std::size_t distance(const binary_vector* v) const {
      
      vector_element_t tmp[N];
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        
        __m128 a = _mm_load_ps((float*) &elements[i]);
        __m128 b = _mm_load_ps((float*) &(v->elements[i]));
        
        __m128 c = _mm_xor_ps(a, b);
        
        // we will compute popcount on this so store. XXX benchmark this
        _mm_store_ps((float*) &tmp[i], c);
      }
      
      // now compute the popcount of the temp (inline) TODO: unroll
      return popcount(tmp);
    }
    
    /* inner for binary vectorspace: count(u&v) */
    
    inline const std::size_t inner(const binary_vector* v) const {
      
      vector_element_t tmp[N];
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        
        __m128 a = _mm_load_ps((float*) &elements[i]);
        __m128 b = _mm_load_ps((float*) &(v->elements[i]));
        
        __m128 c = _mm_and_ps(a, b);
        
        // we will compute popcount on this so store. XXX benchmark this
        _mm_store_ps((float*) &tmp[i], c);
      }
      
      // now compute the popcount of the temp (inline) TODO: unroll
      return popcount(tmp);
    }

    /* countsum for binary vectorspace: count(u|v) */
    
    inline const std::size_t countsum(const binary_vector* v) const {
      
      vector_element_t tmp[N];
      
      for (register unsigned int i=0; i < N; i += (sizeof(__m128)/sizeof(vector_element_t))) {
        
        __m128 a = _mm_load_ps((float*) &elements[i]);
        __m128 b = _mm_load_ps((float*) &(v->elements[i]));
        
        __m128 c = _mm_or_ps(a, b);
        
        // we will compute popcount on this so store. XXX benchmark this
        _mm_store_ps((float*) &tmp[i], c);
      }
      
      // now compute the popcount of the temp (inline) TODO: unroll
      return popcount(tmp);
    }

    
    /* hamming weight is popcount for a binary vector */
    
    inline const std::size_t count() const {
      return popcount((vector_element_t*)elements);
    }
    
    
    /* density is weight/capacity or hamming_weight by dimensions in this case */
    
    inline const double density(void) const {
      return count() / (double) (N * sizeof(vector_element_t) * 8);
    }
    
    
    /* similarity */
    
    inline const double similarity(const binary_vector* v) const {
      std::size_t dims = N * sizeof(vector_element_t) * 8;
      return 1.0 - (distance(v) / (double) dims);
    }
    
    /* popcnt */
    /* why does this not work?
    inline const std::size_t popcount(vector_element_t* v) const {
      std:size_t count = 0;
      for (unsigned int i=0; i < N; i += sizeof(__int64_t)/sizeof(vector_element_t))
        count += _mm_popcnt_u64(v[i]); // try a cast to uint64_t here
      return count;
    }
    */
    
    inline const std::size_t popcount(vector_element_t* v) const {
      std::size_t count = 0;
      for (unsigned int i=0; i < N; ++i)
        count += _mm_popcnt_u32(v[i]);
      return count;
    }
    
    // experimental
    /*
    inline std::vector<vector_element_t> vector_vector () {
      std::vector<vector_element_t> els;
      for (std::size_t i=0; i < N; ++i) {
        els.push_back(elements[i]);
      }
      return els;
    }
    */
  };
  
}

#endif
