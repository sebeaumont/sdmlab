#
# makes a set of target files from the binvec.c kernel functions
#

# default target is the optimized object module and the optimized llvm
# assembly
sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.10.sdk
osxinclude=$(sysroot)/usr/include

llvmbin=/usr/local/Cellar/llvm/3.6.2/bin

disasm=$(llvmbin)/llvm-dis
as=$(llvmbin)/llvm-as
optimizer=$(llvmbin)/opt
llc=$(llvmbin)/llc
cc=$(llvmbin)/clang
ld=ld

#
# targets
#

all:	vectors-opt.ll vectors.ll

vectors.ll:	vectors.bc
	$(disasm) vectors.bc

#
# this is the only bit we need for working code 
#  -fslp-vectorize-aggressive might work with clang -- opt doesn't hack it
vectors.bc:	vectors.c
	$(cc) -I$(osxinclude) -I. -I../vspace -emit-llvm -c vectors.c -o vectors.bc

#
# optimizer pass - some great loop unrolling stuff here
#
vectors-opt.bc: vectors.bc
	$(optimizer) -O3 -loop-unroll -loop-vectorize -force-vector-width=128 vectors.bc -o vectors-opt.bc

vectors-opt.ll:	vectors-opt.bc
	$(disasm) vectors-opt.bc


# have a peek at the generated code -- we really use JIT for portability
# make sure it's AT&T assembler syntax (this is default for x86)

vectors-opt.s:	vectors-opt.bc
	$(llc) -O3 -filetype=asm -mcpu=corei7-avx vectors-opt.bc

# object code for benchmarking vs. jit vs. reference

vectors-opt.o:	vectors-opt.s
	$(cc) -c -o vectors-opt.o  vectors-opt.s


vectors.dylib:	vectors-opt.o
	$(cc) -dynamiclib -isysroot $(sysroot) -o vectors.dylib vectors-opt.o

# clean up generated files

clean:
	@rm -f *.bc *.ll *~ *.s *.o
