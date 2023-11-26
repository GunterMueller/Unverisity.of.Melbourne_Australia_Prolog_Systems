#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1986, 1987 by it.
# 
# All rights are reserved.
#

#
# NU-Prolog 1.4 Compiler Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /home/muse/cs/mip/jws/nepolog

BIN	= /home/muse/cs/mip/jws/bin

LIB	= /home/muse/cs/mip/jws/nepolog/lib

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


# Files needed to build the compiler

SRCS	= sys.nl ute.nl class.nl codegen.nl emitcode.nl preludes.nl valloc.nl \
	  variables.nl main.nl canonical.nl negimp_co.nl sortheads.nl \
	  simplecomp.nl cobuiltins.nl
OBJS	= sys.ns ute.ns class.ns codegen.ns emitcode.ns preludes.ns valloc.ns \
	  variables.ns main.ns canonical.ns negimp_co.ns sortheads.ns \
	  simplecomp.ns cobuiltins.ns
NOBJS	= sys.no ute.no class.no codegen.no emitcode.no preludes.no valloc.no \
	  variables.no main.no canonical.no negimp_co.no sortheads.no \
	  simplecomp.no cobuiltins.no
SYS	= ../sys/sort.no ../sys/parser.no ../sys/atoms.no ../sys/builtins.no \
	  ../sys/dkh.no ../sys/error.no ../sys/io.no ../sys/numvars.no \
	  ../sys/meta.no ../sys/bytecodes.no ../sys/load.no ../sys/misc.no \
	  ../sys/ops.no ../sys/params.no ../sys/mainloop.no ../sys/break.no \
	  ../sys/spy.no ../sys/dcgs.no ../sys/init.no ../sys/listing.no \
	  ../sys/convert.no ../sys/negtrf.no ../sys/xstrf.no \
	  ../sys/lists.no ../sys/callnot.no ../sys/solutions.no \
	  ../sys/unix.no ../sys/revise.no ../sys/aggregates.no

# Configure and compile the compiler

all:	nuc
	@-echo Compiler built

nuc:	../nep/nep $(NOBJS) $(SYS) ../nep/wake.no
	@-/bin/rm -f nuc 
	  ../nep/nep -C -u 64 -v 16 -w 8 -x 4 -s -i ../nep/wake.no -S nuc \
	  ../sys/parser.no ../sys/sort.no \
	  ../sys/atoms.no ../sys/params.no ../sys/builtins.no ../sys/dkh.no \
	  ../sys/error.no ../sys/io.no ../sys/numvars.no ../sys/lists.no \
	  ../sys/meta.no ../sys/bytecodes.no ../sys/load.no ../sys/misc.no \
	  ../sys/ops.no ../sys/mainloop.no ../sys/break.no \
	  ../sys/dcgs.no ../sys/spy.no ../sys/listing.no ../sys/convert.no \
	  ../sys/init.no ../sys/unix.no ../sys/revise.no \
	  ../sys/callnot.no ../sys/solutions.no ../sys/aggregates.no \
	  sys.no ute.no class.no codegen.no emitcode.no preludes.no valloc.no \
	  variables.no main.no canonical.no simplecomp.no cobuiltins.no \
	  ../sys/negtrf.no ../sys/xstrf.no \
	  sortheads.no negimp_co.no

../sys/bytecodes.no: ../sys/bytecodes.ns

../sys/bytecodes.ns: ../nep/bytecodes.c ../sys/bytec.ns.awk
	cd ../sys; make $(CONFIG) bytecodes.ns

../nep/wake.no: ../nep/wake.ns

# Install the NU-Prolog libraries

install: $(NUC) $(NC)
	@-echo Compiler installed

$(NUC):  nuc
	cp nuc $(NUC)

$(NC): nc.SH $(SRC)/Config.sed 
	sed -f $(SRC)/Config.sed nc.SH > $(NC)
	-chmod 755 $(NC)


# Clean up some mess
# NOTE: it is unsafe to remove the .ns files
# This will leave you with a nasty boostrapping problem

clean:
	-rm -f *.no
	rm -f nuc .errs ERRS core
