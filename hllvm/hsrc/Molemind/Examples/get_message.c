#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

// define a function pointer type that matches the STG calling
// convention.
// XXX be nice if we could get this from a Haskell header?

// XXX attribute(cc10) - how to get clang to llvm to force cc10?
/*
typedef void (*STGfun)(int64_t* restrict, int64_t* restrict, int64_t* restrict, int64_t, int64_t, int64_t, int64_t,
                       int64_t, int64_t, int64_t* restrict, float, float, float, float, double, double);
*/
typedef void (*STGfun)(int64_t* restrict, int64_t* restrict, int64_t* restrict, int64_t, int64_t, int64_t, int64_t,
                       int64_t, int64_t, int64_t* restrict);


static inline void printStack(int64_t* sp) {
  for (int i=-4; i<5; i++) 
    printf("SP[%d]\t%llx\n", i, sp[i]);
}

static inline void printHeap(int64_t* sp) {
  for (int i=-4; i<5; i++) 
    printf("HP[%d]\t%llx\n", i, sp[i]);
}

// clang -> llvm -> fixup calling convention to cc10 -> assemble

extern void get_message_STG(int64_t* restrict baseReg,
                            int64_t* restrict sp,
                            int64_t* restrict hp,
                            int64_t r1,
                            int64_t r2,
                            int64_t r3,
                            int64_t r4,
                            int64_t r5,
                            int64_t r6,
                            int64_t* restrict spLim
                            /*
                            float f1,
                            float f2,
                            float f3,
                            float f4,
                            double d1,
                            double d2
                            */
                            ) {

  // XXX this is not what we think it is... XXX
  const STGfun f = (STGfun) sp[0];

  printStack(sp);
  printHeap(hp);
  
  printf("ME:\t%p\n", (void *) get_message_STG);
  printf("SK:\t%p\n", (void *) &f);
  
  // undefined variables to fill in all slots
  // clang will emit these as a llvm undef literal

  const int64_t undef_i;
  const float undef_f;
  const double undef_d;

  const int64_t type = r1;
  
  printf("TYPE:%llu\n", type);
  printf("CONT:\t%p...\n", (void *) f);

    
  // "return" unboxed tuple of results -- currently we get a bus error :-)
  return f(baseReg, sp, hp,
           type, undef_i, undef_i, undef_i, undef_i, undef_i,
           spLim
           /*
           undef_f, undef_f, undef_f, undef_f,
           undef_d, undef_d
           */
           );
}
 
