#
# makes a set of target files from the binvec.c kernel functions
#

# default target is the optimized object module and the optimized llvm
# assembly

all:	binvec-opt.o binvec-opt.ll

binvec.ll:	binvec.bc
	llvm-dis binvec.bc

binvec.bc:	binvec.c
	clang -emit-llvm -c binvec.c -o binvec.bc

# optimizer pass

binvec-opt.bc: binvec.bc
	opt -O3 -loop-vectorize -force-vector-width=8 -force-vector-unroll=4 binvec.bc -o binvec-opt.bc

binvec-opt.ll:	binvec-opt.bc
	llvm-dis binvec-opt.bc

# code generation - make sure it's AT&T assembler syntax (this is default for x86)

binvec-opt.s:	binvec-opt.bc
	llc -O3 -filetype=asm -mcpu=corei7-avx binvec-opt.bc

# object code

binvec-opt.o:	binvec-opt.s
	clang -c -o binvec-opt.o binvec-opt.s


# clean up generated files

clean:
	@rm -f *.bc *.ll *~ *.s *.o
