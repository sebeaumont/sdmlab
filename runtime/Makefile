#!/usr/bin/env make
# TODO CMAKE build this is just a hack to build the runtime

llvm=3.5
disasm=llvm-dis-$(llvm)
optimizer=opt-$(llvm)
llc=llc-$(llvm)
cc=clang


DEBUG=-DDEBUG
SOURCES = Makefile runtime.cpp vectorarray.hpp binary_vectorarray.hpp runtime_exceptions.hpp utils.h unistd_fix.h

# jsut compile the stuff to bit code and optimize later
runtime_stage1.bc:			$(SOURCES)
	$(cc) -emit-llvm -c runtime.cpp -I /usr/local/include/boost -o runtime_stage1.bc

runtime.dylib:	$(SOURCES)
	$(cc) $(DEBUG) -dynamiclib -fpic runtime.cpp -I /usr/local/inlcude/boost -o runtime.dylib -lc++


#
# optimizer pass - some great loop unrolling stuff here
#

runtime.bc:			runtime_stage1.bc
	$(optimizer) -O3 -loop-vectorize -force-vector-width=32 -force-vector-unroll=128 runtime_stage1.bc -o runtime.bc

#
# have a butcher's at the generated code...
# 
runtime.ll:			runtime.bc
	$(disasm) runtime.bc

