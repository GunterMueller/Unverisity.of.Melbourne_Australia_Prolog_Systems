#
# NU-Prolog 1.5 Library Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /home/muse/cs/mip/jws/nepolog

BIN	= /nip/jws/bin

LIB	= /nip/jws/lib/nuprolog

SHELL = /bin/sh

CDEFS	= -DBSD4

CONFIG	= PARMAKE="$(PARMAKE)" SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

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

.nl.ns: ; ../nep/nep -R ../co/nuc -a $*.nl
.ns.no: ; ../na/na $*.ns

.c.o:
	--rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

CFLAGS = -O $(CDEFS)

# Files needed to build the libraries
#
# Note: "man.no" is installed from ../man

SRCS	= arg.nl changeArg.nl compat.nl db.nl dsimc.nl osets.nl \
	predefined.nl simc.nl sql.nl rule.nl debug.nl \
	occurs.nl fromonto.nl lineio.nl logarr.nl prompt.nl termdepth.nl \
	antiUnify.nl unify.nl

OBJS	= arg.ns changeArg.ns compat.ns db.ns dsimc.ns osets.ns \
	predefined.ns simc.ns sql.ns rule.ns debug.ns \
	occurs.ns fromonto.ns lineio.ns logarr.ns prompt.ns termdepth.ns \
	antiUnify.ns unify.ns

LIBS	= arg.no changeArg.no compat.no db.no dsimc.no osets.no \
	predefined.no simc.no sql.no rule.no debug.no \
	occurs.no fromonto.no lineio.no logarr.no prompt.no termdepth.no \
	antiUnify.no unify.no

# Configure and compile the libraries

all:	$(LIBS) public.h Quintus
	@-echo Libraries made

Quintus:
	cd quintus; $(PARMAKE) LIB="$(LIB)" all

libraryPredicate: $(OBJS)
	egrep '^	\.pred	' *.ns | awk -f libpreds.awk

arg.no: arg.ns
antiUnify.no: antiUnify.ns
changeArg.no: changeArg.ns
compat.no: compat.ns
debug.no: debug.ns
fromonto.no: fromonto.ns
lineio.no: lineio.ns
logarr.no: logarr.ns
occurs.no: occurs.ns
osets.no: osets.ns
predefined.no: predefined.ns
prompt.no: prompt.ns
rule.no: rule.ns
sql.no: sql.ns
termdepth.no: termdepth.ns
unify.no: unify.ns

public.h: ../nep/public.h
	cp ../nep/public.h .

db.no:  db.ns
db.ns:  db.nl
db.nl:  db.NL
	-/bin/rm -f db.nl
	sed -f $(SRC)/Config.sed db.NL > db.nl

simc.no:  simc.ns
simc.ns:  simc.nl
simc.nl:  simc.NL $(SRC)/Config.sed
	-/bin/rm -f simc.nl
	sed -f $(SRC)/Config.sed simc.NL > simc.nl

dsimc.no:  dsimc.ns
dsimc.ns:  dsimc.nl
dsimc.nl:  dsimc.NL $(SRC)/Config.sed
	-/bin/rm -f dsimc.nl
	sed -f $(SRC)/Config.sed dsimc.NL > dsimc.nl


# Install the libraries

install: all
	-chmod 644 $(NULIB)/*.nl $(NULIB)/*.no
	cp *.nl *.no $(NULIB)
	-chmod 444 $(NULIB)/*.nl $(NULIB)/*.no
	-chmod 644 $(NULIB)/public.h
	cp public.h $(NULIB)
	-chmod 444 $(NULIB)/public.h
	cd quintus; make LIB="$(LIB)" install
	@-echo Libraries installed

# Clean up some mess

clean:
	-rm -f *.no *.ns *.o
	-rm -f db.nl dsimc.nl simc.nl public.h core .errs ERRS
	cd quintus; make clean
