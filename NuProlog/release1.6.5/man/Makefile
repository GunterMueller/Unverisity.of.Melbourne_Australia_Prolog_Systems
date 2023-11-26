#
# NU-Prolog 1.4 On-line manual Makefile
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

.SUFFIXES: .nl .ns .no

.nl.ns: ; ../nep/nep -R ../co/nuc -a $*.nl
.ns.no: ; ../na/na $*.ns


# Files needed to build the on-line manual

EXECS	= newdb dbload man
SAVES	= newdb.save dbload.save man.save
NSRCS	= newdb.nl dbload.nl man.nl
ASMS	= newdb.ns dbload.ns man.ns
OBJS	= newdb.no dbload.no man.no


# Configure and compile the on-line manual utilities

all: newdb dbload
	@-echo On-line manual utilities made


# Install the on-line manual

install: all db man
	@-echo On-line manual installed

db: newdb dbload $(LIB)/man.$(NUVERSION)/man/00.0000

$(LIB)/man.$(NUVERSION)/man/00.0000: Man.data newdb dbload
	./newdb
	./dbload < Man.data
	chmod 666 $(LIB)/man.$(NUVERSION)/man/*

newdb: newdb.nl $(NC)
	$(NC) -o newdb newdb.nl

newdb.nl: newdb.NL $(SRC)/Config.sed
	-/bin/rm -f newdb.nl
	sed -f $(SRC)/Config.sed newdb.NL > newdb.nl

dbload: dbload.nl $(NC)
	$(NC) -o dbload dbload.nl

dbload.nl: dbload.NL $(SRC)/Config.sed
	-/bin/rm -f dbload.nl
	sed -f $(SRC)/Config.sed dbload.NL > dbload.nl

man:  $(NULIB)/man.no $(NULIB)/man.nl

$(NULIB)/man.no: man.no
	-chmod 644 $(NULIB)/man.no
	cp man.no $(NULIB)/man.no
	-chmod 444 $(NULIB)/man.no

$(NULIB)/man.nl: man.nl
	-chmod 644 $(NULIB)/man.nl
	cp man.nl $(NULIB)/man.nl
	-chmod 444 $(NULIB)/man.nl

man.no: man.ns
man.ns: man.nl
man.nl: man.NL $(SRC)/Config.sed
	-/bin/rm -f man.nl
	sed -f $(SRC)/Config.sed man.NL > man.nl

$(SRC)/Config.sed:

# Clean up some mess

clean:
	/bin/rm -f $(NSRCS) $(ASMS) $(OBJS) $(EXECS) $(SAVES) core ERRS .errs

