#
# handmake sdm with llvm toolchain
#


sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk
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

all:	libsdm.dylib

sdm_vspace.ll:	sdm_vspace.bc
	$(disasm) sdm_vspace.bc

#
# this is the only bit we need for working code 
# 
sdm_vspace.bc:	sdm_vspace.c
	$(cc) -I$(osxinclude) -I. -I../vspace -emit-llvm -c sdm_vspace.c -o sdm_vspace.bc

#
# optimizer pass - some great loop unrolling stuff here
#
sdm_vspace-opt.bc: sdm_vspace.bc
	$(optimizer) -O3 -slp-vectorizer -loop-unroll -loop-vectorize -force-vector-width=128 sdm_vspace.bc -o sdm_vspace-opt.bc

#
# get source for bitcode
#
sdm_vspace-opt.ll:	sdm_vspace-opt.bc
	$(disasm) sdm_vspace-opt.bc


# have a peek at the generated code -- we really use JIT for portability
# make sure it's AT&T assembler syntax (this is default for x86)

sdm_vspace-opt.s:	sdm_vspace-opt.bc
	$(llc) -O3 -filetype=asm -mcpu=corei7-avx sdm_vspace-opt.bc

# object code for benchmarking vs. jit vs. reference

#sdm_vspace-opt.o:	sdm_vspace-opt.s
#	$(cc) -c -o sdm_vspace-opt.o  sdm_vspace-opt.s

# depend on source ll so we can transform it (TBS) before compilation

sdm_vspace-opt.o:	sdm_vspace-opt.ll
	$(llc) -O3 -filetype=obj -mcpu=corei7-avx sdm_vspace-opt.ll -o sdm_vspace-opt.o


# dynamic library

libsdm.dylib:	sdm_vspace-opt.o
	$(cc) -dynamiclib -isysroot $(sysroot) -o libsdm.dylib sdm_vspace-opt.o

# clean up generated files

clean:
	@rm -f *.bc *.ll *~ *.s *.o *.dylib
