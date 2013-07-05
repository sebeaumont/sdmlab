//
//  Created by Datalligator on 12/03/2013.
//  Copyright (c) 2013 Simon Beaumont. All rights reserved.
//  Internal Research Use Only
//

/* 
The idea here is to provide naive and simple implemetations to see
how our compilers and other otpmizations perform and to make this
basic set of operations portable and JIT compilable
*/

#define N 1024
typedef unsigned int vector_element_t;

/*
 * bvector_t
 */ 
 
typedef struct _bvector {
  /* storage */
  /* alignas(128) */ vector_element_t elements[N];
} *bvector_t;
   

/* vector popcount */
    
const unsigned int count(const bvector_t v) {
  unsigned int count = 0;
  for (unsigned int i=0; i < N; ++i)
    count += __builtin_popcount(v->elements[i]);
  return count;
}

    
/* vector density is weight/capacity or hamming_weight by dimensions in this case */
    
const double density(const bvector_t v) {
  return count(v) / (double) (N * sizeof(vector_element_t) * 8);
}


///////////////
// vector ops 
///////////////


/* unitize vector */
    
void unit(bvector_t v) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] = -1;
  }
}
    

/* zero vector */
    
void zero(bvector_t v) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] = 0;
  }
}

    
/* add accumulate (u |= v) in bvectorspace is or */
    
void adda(bvector_t v, const bvector_t u) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] |= u->elements[i];
  }
}
    
/* subtraction  u = u & ~v */
    
void suba(bvector_t v, const bvector_t u) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] &= ~u->elements[i];
  }
}
   
/* multiplication u = u ^ v */
    
void mula(bvector_t v, const bvector_t u) {
  
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] ^= u->elements[i];
  }
}
    
    
/* distance for binary vectorspace: count(u^v) */

const unsigned int distance(const bvector_t v, const bvector_t u) {
  
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] ^ u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }  
  return count;
}
    

/* inner for binary vectorspace: count(u&v) */
    
const unsigned int inner(const bvector_t v, const bvector_t u) {
      
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (register unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] & u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }
  return count;
}


/* countsum for binary vectorspace: count(u|v) */
    
const unsigned int countsum(const bvector_t v, const bvector_t u) {
  
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] | u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }
  return count;
}

    
/* similarity */
    
const double similarity(const bvector_t v, const bvector_t u) {
  unsigned int dims = N * sizeof(vector_element_t) * 8;
  return 1.0 - (distance(v,u) / (double) dims);
}
    
