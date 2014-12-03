#
# makes a set of target files from the binvec.c kernel functions
#

# default target is the optimized object module and the optimized llvm
# assembly

#llvm=3.5
disasm=llvm-dis
as=llvm-as
optimizer=opt
llc=llc
cc=clang

#
# targets
#

all:	binvec-opt.ll binvec.ll

binvec.ll:	binvec.bc
	$(disasm) binvec.bc

#
# this is the only bit we need for working code 
#  -fslp-vectorize-aggressive might work with clang -- opt doesn't hack it
binvec.bc:	binvec.c
	$(cc) -emit-llvm -c binvec.c -o binvec.bc

#
# optimizer pass - some great loop unrolling stuff here
#
binvec-opt.bc: binvec.bc
	$(optimizer) -O3 -loop-vectorize -force-vector-width=32 -force-vector-unroll=128 binvec.bc -o binvec-opt.bc

binvec-opt.ll:	binvec-opt.bc
	$(disasm) binvec-opt.bc


# have a peek at the generated code -- we really use JIT for portability
# make sure it's AT&T assembler syntax (this is default for x86)

#binvec-opt.s:	binvec-opt.bc
#	$(llc) -O3 -filetype=asm -mcpu=corei7-avx binvec-opt.bc

# object code for benchmarking vs. jit vs. reference

#binvec-opt.o:	binvec-opt.s
#	$(cc) -c -o binvec-opt.o binvec-opt.s


# clean up generated files

clean:
	@rm -f *.bc *.ll *~ *.s *.o
