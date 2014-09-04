CC=clang
LLVM=3.4

DIS=llvm-dis-$(LLVM)
OPT=llvm-opt-$(LLVM)

DEBUG=-DDEBUG

# TODO CMAKE build this is juast a hack

SOURCES = Makefile runtime.cpp vectorarray.hpp binary_vectorarray.hpp runtime_exceptions.hpp utils.h unistd_fix.h

runtime.bc:			$(SOURCES)
	$(CC) -emit-llvm -c runtime.cpp -I /usr/local/include/boost -o runtime.bc

runtime.dylib:	$(SOURCES)
	$(CC) $(DEBUG) -dynamiclib -fpic runtime.cpp -I /usr/local/inlcude/boost -o runtime.dylib -lc++

runtime.ll:			runtime.bc
	$(DIS) runtime.bc
