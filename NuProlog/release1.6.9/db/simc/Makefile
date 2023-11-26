#
# NU-Prolog 1.4 SIMC Database system Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /e/users/cs/jws/nepolog

BIN	= /e/users/cs/jws/bin

LIB	= /mip/usr/lib/nepolog

CDEFS	= 

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" CC="$(CC)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Make the SIMC indexing scheme

all:
	(cd lib; $(PARMAKE) $(CONFIG))
	(cd bin; $(PARMAKE) $(CONFIG))



install:
	(cd lib; $(PARMAKE) $(CONFIG) install)
	(cd bin; $(PARMAKE) $(CONFIG) install)


# Clean up some mess

clean:
	(cd bin; make clean)
	(cd lib; make clean)
