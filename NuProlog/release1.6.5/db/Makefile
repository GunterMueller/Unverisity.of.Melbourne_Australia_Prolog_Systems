#
# NU-Prolog 1.4 Database system Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /nip/jws/nepolog

BIN	= /nip/jws/bin

LIB	= /nip/jws/nepolog/lib

SHELL = /bin/sh

CDEFS	= 

CONFIG	= PARMAKE="$(PARMAKE)" SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" CC="$(CC)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Compile all of the indexing schemes, but don't install them

all: Simc Sql

Simc:
	cd simc; $(PARMAKE) $(CONFIG)

Sql:
	cd sql; $(PARMAKE) $(CONFIG)


install: Simc.i Sql.i

Simc.i:
	cd simc; $(PARMAKE) $(CONFIG) install

Sql.i:
	cd sql; $(PARMAKE) $(CONFIG) install

install2:
	cd sql; $(PARMAKE) $(CONFIG) install2

# Organise clean up in sub-directories

clean:
	cd simc; make clean
	cd sql; make clean
