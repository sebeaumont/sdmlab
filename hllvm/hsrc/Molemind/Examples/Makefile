#
# make with llvm toolchain and STG/cc10 calling hacks -- very experimental
#

#### /usr/include should do here! 
sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk
llvmbin=/usr/local/opt/llvm37/lib/llvm-3.7/bin
####
llvmver=3.7

osxinclude=$(sysroot)/usr/include

## llvm toolchain

disasm=$(llvmbin)/llvm-dis
as=$(llvmbin)/llvm-as
opt=$(llvmbin)/opt
llc=$(llvmbin)/llc
cc=$(llvmbin)/clang


ld=ld
sed=/usr/bin/sed
ghc=/usr/local/bin/ghc

#
# targets
#

all:	test

get_message.o:	get_message.llp
	#$(llc) -O3 -pre-RA-sched=list-burr -regalloc=greedy -relocation-model=static -filetype=obj get_message.llp -o get_message.o		
	$(llc) -O3 -filetype=obj get_message.llp -o get_message.o


get_message.llp: get_message.ll
	$(sed) -e 's/define void/define ghccc void/; s/call void/call ghccc void/;' < get_message.ll > get_message.llp


get_message.ll:	get_message.c Makefile
	$(cc) -O3 -I$(osxinclude) -I. -emit-llvm -S get_message.c -o get_message.ll


test:	Main.hs Primops.hs get_message.o Makefile
	$(ghc) -fllvm -keep-llvm-files -fforce-recomp -pgmc $(cc) -pgml $(cc) -pgmlc $(llc) -pgmlo $(opt) -O2 -Wall -rtsopts --make -outputdir=. Main.hs Primops.hs RegularFunctions.hs get_message.o -o test
	#$(ghc) -O2 -Wall -rtsopts -threaded --make -outputdir=. Main.hs Primops.hs get_message.o -o test

test2:	Main.hs Primops.hs get_message.o Makefile
	$(ghc) -fllvm -keep-llvm-files -fforce-recomp -pgmc $(cc) -pgml $(cc) -pgmlc $(llc) -pgmlo $(opt) -O2 -Wall -rtsopts -threaded --make -outputdir=. Main.hs Primops.hs get_message.o -o test

clean:
	rm -f test *.o *~
