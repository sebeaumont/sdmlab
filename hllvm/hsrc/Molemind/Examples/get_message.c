#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

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
                            int64_t* restrict spLim,
                            float f1,
                            float f2,
                            float f3,
                            float f4,
                            double d1,
                            double d2) {

  // define a function pointer type that matches the STG calling
  // convention - be nice if we could get them from a Haskell header?

  typedef void (*STGfun)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t,
                         int64_t, int64_t, int64_t*, float, float, float, float, double, double);

  const STGfun f = (STGfun)sp[0];

  // undefined variables to fill in all slots
  // clang will emit these as a llvm undef literal

  const int64_t undef_i;
  const float undef_f;
  const double undef_d;

  const int64_t type = r1;
  
  printf("OUT\n");

  // "return" unboxed tuple of results -- currently we get a bus error :-)
  return f(baseReg, sp, hp,
           type, undef_i, undef_i, undef_i, undef_i, undef_i,
           spLim,
           undef_f, undef_f, undef_f, undef_f,
           undef_d, undef_d);
}
 
