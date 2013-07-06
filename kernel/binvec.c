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
} bvector_t;
  
/* ensure our pointers are const */
typedef bvector_t * const bvector; 

/* vector popcount */
    
const unsigned int count(const bvector v) {
  unsigned int count = 0;
  for (unsigned int i=0; i < N; ++i)
    count += __builtin_popcount(v->elements[i]);
  return count;
}

    
/* vector density is weight/capacity or hamming_weight by dimensions in this case */
    
const double density(const bvector v) {
  return count(v) / (double) (N * sizeof(vector_element_t) * 8);
}


//////////////////////////////////////////////////////////////////////
//
// vector ops N.B. it is required that u =! v in all of these
// operations i.e. at least if u == v then they are undefined due to
// the restriction of the respective pointers.
//
//////////////////////////////////////////////////////////////////////

/* unitize vector */
    
void unit(bvector v) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] = -1;
  }
}
    

/* zero vector */
    
void zero(bvector v) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] = 0;
  }
}

    
/* add accumulate (u |= v) in bvectorspace is or */
    
void adda(bvector restrict v, const bvector restrict u) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] |= u->elements[i];
  }
}
    
/* subtraction  u = u & ~v */
    
void suba(bvector restrict v, const bvector restrict u) {
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] &= ~u->elements[i];
  }
}
   
/* multiplication u = u ^ v */
    
void mula(bvector restrict v, const bvector restrict u) {
  
  for (unsigned int i=0; i < N; ++i) {
    v->elements[i] ^= u->elements[i];
  }
}
    

//////////////////
// vector metrics
//////////////////

    
/* distance for binary vectorspace: count(u^v) */

const unsigned int distance(const bvector restrict v, const bvector restrict u) {
  
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] ^ u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }  
  return count;
}
    

/* inner for binary vectorspace: count(u&v) */
    
const unsigned int inner(const bvector restrict v, const bvector restrict u) {
      
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (register unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] & u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }
  return count;
}


/* countsum for binary vectorspace: count(u|v) */
    
const unsigned int countsum(const bvector restrict v, const bvector restrict u) {
  
  vector_element_t tmp[N];
  unsigned int count = 0;
  
  for (unsigned int i=0; i < N; ++i) {
    tmp[i] = v->elements[i] | u->elements[i];
    count += __builtin_popcount(tmp[i]);
  }
  return count;
}

    
/* similarity */
    
const double similarity(const bvector restrict v, const bvector restrict u) {
  unsigned int dims = N * sizeof(vector_element_t) * 8;
  return 1.0 - (distance(v,u) / (double) dims);
}
    
