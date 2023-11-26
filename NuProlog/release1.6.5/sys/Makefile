#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1986, 1987 by it.
#
# All rights are reserved.
#

#
# NU-Prolog 1.5 Run-time system Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /nip/jws/nepolog

BIN	= /nip/jws/bin

LIB	= /nip/jws/lib/nuprolog

SHELL = /bin/sh

CDEFS	=

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


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

.SUFFIXES: .no .ns .nl

.nl.ns: ; ../nep/nep -R ../co/nuc -a $*.nl
.ns.no: ../na/na
	../na/na $*.ns


# Files needed to build the Run-time system

SRCS	= parser.nl atoms.nl builtins.nl dkh.nl error.nl io.nl numvars.nl \
	  sort.nl lists.nl meta.nl params.nl load.nl misc.nl ops.nl \
	  mainloop.nl break.nl spy.nl init.nl dcgs.nl convert.nl \
	  listing.nl negtrf.nl xstrf.nl negimp_in.nl solutions.nl callnot.nl \
	  revise.nl unix.nl aggregates.nl
OBJS	= parser.ns atoms.ns builtins.ns dkh.ns error.ns io.ns numvars.ns \
	  sort.ns lists.ns meta.ns params.ns bytecodes.ns load.ns misc.ns ops.ns \
	  mainloop.ns break.ns spy.ns init.ns dcgs.ns convert.ns \
	  listing.ns negtrf.ns xstrf.ns negimp_in.ns solutions.ns callnot.ns \
	  revise.ns unix.ns aggregates.ns
NOBJS	= parser.no atoms.no builtins.no dkh.no error.no io.no numvars.no \
	  sort.no lists.no meta.no params.no bytecodes.no load.no misc.no ops.no \
	  mainloop.no break.no dcgs.no spy.no convert.no \
	  listing.no negtrf.no xstrf.no negimp_in.no solutions.no callnot.no \
	  revise.no unix.no aggregates.no

# Configure and compile the run-time system

all:	nuprolog
	@-echo Run-time system built

nuprolog: ../nep/nep $(NOBJS) init.no
	@-/bin/rm -f nuprolog
	../nep/nep -u 32 -v 16 -w 8 -x 4 -s -i ../nep/wake.no -S nuprolog \
	$(NOBJS) init.no

bytecodes.ns: ../nep/bytecodes.c bytec.ns.awk
	awk -f bytec.ns.awk defines="$(CDEFS)" ../nep/bytecodes.c >bytecodes.ns

# Install the NU-Prolog run-time system

install: $(NUP) $(NP)
	@-echo Run-time system installed

$(NP): nuprolog
	echo exec $(NEP) -T -R $(NUP) '"$$@"' > $(NP)
	-chmod 755 $(NP)

$(NUP):  nuprolog
	cp nuprolog $(NUP)

# Clean up some mess

clean:
	-rm -f *.no bytecodes.ns
	rm -f nuprolog core .errs ERRS
