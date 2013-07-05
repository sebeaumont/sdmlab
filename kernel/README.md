This is the 'Gator Kernel
-------------------------

Based on the original Alligator DSM vector source code - this is to
see if we can get llvm to do all the optimizations we need and how JIT
compares with static compilation.

Some kind of workflow to investigate/measure:

         clang -emit-llvm -c bitvec.c -o bitvec.bc
         opt -O3 "-make-it-faster" bitvec.bc -o bitvec-opt1.bc
         llvm-dis bitvec-opt1.bc
         llc -O3 -mcpu=i7-avx -filetype=asm bitvec-opt1.bc -o bitvec-opt1.s
         .
         <normal compile and link>
         .
         .

________________________
Internal Research Use Only