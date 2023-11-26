#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1986, 1987 by it.
# 
# All rights are reserved.
#
# Author: John Shepherd
#

#
# NU-Prolog 1.5 Bytecode interpreter Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /home/muse/cs/mip/jws/nepolog

BIN	= /home/muse/cs/mip/jws/bin

LIB	= /home/muse/cs/mip/jws/lib/nuprolog

CDEFS	= -DBSD4

CDEFS	= -DMACHINE_SGI -DBSD4 -DFLOAD

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)"

# End Configuration Parameters

SHELL = /bin/sh

# Definitions of some important directories/files

NP	= $(BIN)/np
NC	= $(BIN)/nc
NA	= $(BIN)/na
NUBIN	= $(LIB)/bin.$(NUVERSION)
NULIB	= $(LIB)/lib.$(NUVERSION)
NDIS	= $(NUBIN)/dis
NEP	= $(NUBIN)/nep
NUC	= $(NUBIN)/nuc
NUP	= $(NUBIN)/nuprolog

# Rules for configuring and compiling NU-Prolog source programs
# Don't assume that anything is installed yet

.SUFFIXES:  .no .ns .nl

.nl.ns:  ; ../nep/nep -R ../co/nuc -a $*.nl
.ns.no: ../na/na
	../na/na $*.ns
.c.o:
	--rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

# Files needed to build the Bytecode interpreter

DEFS	= bytecodes.h defs.h nltypes.h tags.h types.h machdep.h db.h malloc.h
SRCS	= inter.c unify.c nep.c syms.c debug.c bytecodes.c wake.c \
	  nonLogic.c memory.c symTab.c load.c display.c error.c arith.c \
	  terms.c atoms.c ioprim.c assert.c simc.c sql.c paths.c \
	  break.c iload.c negation.c unix.c malloc.c write.c fload.c
OBJS	= unify.o nep.o syms.o debug.o bytecodes.o wake.o \
	  nonLogic.o memory.o symTab.o load.o display.o error.o arith.o \
	  terms.o atoms.o ioprim.o assert.o simc.o sql.o paths.o break.o \
	  iload.o negation.o unix.o malloc.o write.o fload.o inter.o
LIBS	= $(SRC)/db/simc/lib/dblib.a
JUNK	= bytecodes.h syms.h syms.c tags.h tags.o tags \
	  orig.inter.s next.inter.s inter.s wake.ns

# Possibilities for CC and LD flags:
#CFLAGS	= $(CDEFS)
#CFLAGS	= -DDEBUG -DDEBUG3 $(CDEFS)
CFLAGS	= -O $(CDEFS)

CFLAGS	= -O -Olimit 4000 -G 500 $(CDEFS)


#	sgi	sgi	sgi	sgi	sgi	sgi	sgi
nep:  $(OBJS) $(LIBS)
	-rm -f nep
	$(CC) -v $(CFLAGS) -o nep -Wl,-jmpopt -Wl,-kD -Wl,800000 \
		$(OBJS) $(LIBS) -lm -lmld
#	ld -o nep -jmpopt -G 500 -kD 800000 /usr/lib/crt1.o $(OBJS) $(LIBS) \
#		-lm -lmld -lc /usr/lib/crtn.o

inter.o: inter.c $(DEFS) $(SRC)/Config.sed
	-rm -f inter.o
	$(CC) $(CFLAGS) -S inter.c
	mv inter.s orig.inter.s
	./opt.sgi orig.inter.s >inter.s
	$(CC) $(CFLAGS) -c inter.s

# Configure and compile the interpreter

all:	nep
	@-echo Interpreter built

# Install the interpreter

install:  $(NEP)
	@-echo Interpreter installed

$(NEP):  nep $(NUBIN)/wake.no
#	chmod 751 nep
#	chgrp nepolog nep
#	rm -f $(NEP) $(NUBIN)/wake.no
	cp nep $(NEP)

$(NUBIN)/wake.no:  wake.no
	cp wake.no $(NUBIN)/wake.no

wake.no:  wake.ns

wake.ns: wake.ns.source $(SRC)/Config.sed
	awk -f ifdef.awk defines="$(CDEFS)" wake.ns.source >wake.ns

bytecodes.h:  bytecodes.c bytec.h.awk $(SRC)/Config.sed
	-rm -f bytecodes.h
	awk -f bytec.h.awk defines="$(CDEFS)" bytecodes.c >bytecodes.h
#	chmod og+r bytecodes.h

syms.c:  syms.source syms.c.awk
	-rm -f syms.c
	awk -f syms.c.awk <syms.source >syms.c
#	chmod og+r syms.c

syms.h:  syms.source syms.h.awk
	-rm -f syms.h
	awk -f syms.h.awk <syms.source >syms.h
#	chmod og+r syms.h

tags.h: tags
	./tags >tags.h

tags: tags.o
	$(CC) $(CFLAGS) -o tags tags.o

tags.o: tags.c machdep.h

$(OBJS):	mu.h version.h machdep.h tags.h types.h syms.h bytecodes.h \
			macros.h malloc.h

display.o iload.o load.o: nltypes.h
simc.o sql.o:  db.h

paths.o: paths.c $(SRC)/Config.sed
	-rm -f paths.o
	$(CC) $(CFLAGS) -DNEP="\"$(NEP)\"" -DNUBIN="\"$(NUBIN)/\"" -DWAKE="\"$(NUBIN)/wake.no\"" -DBIN=\"$(BIN)/\" -DNULIB="\"$(NULIB)/\"" -DHEADER="\"#! $(NEP) -R\\n\"" -c paths.c

# Other things to do with the Bytecode interpreter

# Clean up some mess

clean:  
	rm -f $(OBJS) a.out core nep ERRS .errs $(LINT) $(JUNK) wake.no
