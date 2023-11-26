#
# NU-Prolog 1.5 Library Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /nip/jws/nepolog

BIN	= /nip/jws/bin

LIB	= /nip/jws/lib/nuprolog

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

.SUFFIXES: .nl .ns .no

.nl.ns: ; ../../nep/nep -R ../../co/nuc -a $*.nl
.ns.no: ; ../../na/na $*.ns


LIBS	= arg.no changearg.no compat.no fromonto.no lineio.no logarr.no \
	occurs.no prompt.no samefunctor.no termdepth.no unify.no

# Configure and compile the Quintus libraries

all:	$(LIBS)
	@-echo Quintus Libraries made

arg.no: arg.ns
changearg.no: changearg.ns
compat.no: compat.ns
fromonto.no: fromonto.ns
lineio.no: lineio.ns
logarr.no: logarr.ns
occurs.no: occurs.ns
prompt.no: prompt.ns
samefunctor.no: samefunctor.ns
termdepth.no: termdepth.ns
unify.no: unify.ns

# Install the libraries

install: all
	-chmod 644 $(NULIB)/quintus/*.n[lo]
	cp *.nl *.no $(NULIB)/quintus
	-chmod 444 $(NULIB)/quintus/*.n[lo]
	@-echo Quintus Libraries installed

# Clean up some mess

clean:
	-rm -f *.no *.ns *.o
	-rm -f core .errs ERRS
