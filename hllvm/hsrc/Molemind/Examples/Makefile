#
# make with llvm toolchain
#

####
sysroot=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk
####

osxinclude=$(sysroot)/usr/include
llvmbin=/usr/local/opt/llvm/bin
disasm=$(llvmbin)/llvm-dis
as=$(llvmbin)/llvm-as
optimizer=$(llvmbin)/opt
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
# -O3 -pre-RA-sched=list-burr -regalloc=greedy -relocation-model=static'		
	$(llc) -O3 -filetype=obj get_message.llp -o get_message.o


get_message.llp: get_message.ll
	$(sed) -e 's/define void/define cc10 void/; s/call void/call cc10 void/;' < get_message.ll > get_message.llp


get_message.ll:	get_message.c Makefile
	$(cc) -O3 -I$(osxinclude) -I. -emit-llvm -S get_message.c -o get_message.ll



test:	Main.hs Primops.hs get_message.o Makefile
	$(ghc) -v3 -fllvm -pgmc=$(cc) -pgml=$(cc) -O2 -Wall -rtsopts --make -outputdir=. Main.hs Primops.hs get_message.o -o test

clean:
	rm *.o *~
