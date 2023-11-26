#	MU-Prolog 3.2 Makefile - RLHP database indexing scheme

HOME	= /usr/src/melb/prolog

BIN	= /usr/melb
PROLOG	= $(BIN)/prolog
PLSAVE	= $(BIN)/plsave
LPP	= $(BIN)/lpp
NEGPP	= $(BIN)/negpp

LIB	= /usr/lib/prolog
PLSTATE	= $(LIB)/plstate 
PLLIB	= $(LIB)/pllib
LDLIB	= $(LIB)/dload
DBLIB	= $(LIB)/db
SYSLIB	= $(LIB)/sys

CONFIG	= HOME="$(HOME)" BIN="$(BIN)" LIB="$(LIB)"

#CFLAGS= -O -DMAXPAGE=256 -DMAXBSIZE=1024 # for smaller page sizes
CFLAGS= -O -DMAXPAGE=256 -DMAXBSIZE=4096 -cckr

OBJS= rlhp.o mudd.o newrel.o prbkt.o prdd.o

MYLIB	= $(DBLIB)/rlhp
INCDIR	= $(HOME)/prolog

# How we make this depends on whether builtin or dynamically loaded

dload: install

builtin: all

# Create the utility programs for the RLHP indexing scheme

all: rlhp mudd newrel prbkt prdd

rlhp: rlhp.o

rlhp.o: rlhp.c $(INCDIR)/types.h $(INCDIR)/pred.h $(INCDIR)/local.h
	$(CC) -c -I"$(INCDIR)" $(CFLAGS) rlhp.c

mudd: mudd.o dblib
	$(CC) $(CFLAGS) mudd.o dblib -o mudd

newrel: newrel.o dblib
	$(CC) $(CFLAGS) newrel.o dblib -o newrel

prbkt: prbkt.o dblib
	$(CC) $(CFLAGS) prbkt.o dblib -o prbkt

prdd: prdd.o dblib
	$(CC) $(CFLAGS) prdd.o dblib -o prdd

dblib: lib/bdesc.c lib/bget.c lib/bmatch.c lib/bput.c lib/bsplit.c lib/bucket.h lib/desc.c lib/desc.h lib/fact.c lib/fact.h lib/factio.c lib/hash.c lib/rassert.c lib/rclose.c lib/rcreat.c lib/rel.h lib/rerror.c lib/rhdput.c lib/ropen.c lib/rtrans.c
	cd lib; make "CFLAGS= $(CFLAGS)"

mudd.o: lib/rel.h lib/fact.h
newrel.o: lib/desc.h lib/rel.h
prbkt.o: lib/desc.h lib/rel.h lib/fact.h
prdd.o: lib/desc.h lib/rel.h lib/fact.h

$(HOME)/types.h:
$(HOME)/pred.h:
$(HOME)/local.h:

# Install the Prolog libraries and utilities for the RLHP indexing scheme

install: $(MYLIB) $(MYLIB)/mudd $(MYLIB)/newrel $(MYLIB)/prdd $(MYLIB)/prbkt $(MYLIB)/dblib $(MYLIB)/fact.h $(LIB)/dload/rlhp.o $(PLLIB)/rlhp

$(MYLIB):
	@-if [ ! -d $(MYLIB) ] ; then mkdir $(MYLIB) ; fi

$(MYLIB)/mudd: mudd
	cp mudd $(MYLIB)/mudd

$(MYLIB)/newrel: newrel
	cp newrel $(MYLIB)/newrel
	
$(MYLIB)/prdd: prdd
	cp prdd $(MYLIB)/prdd

$(MYLIB)/prbkt: prbkt
	cp prbkt $(MYLIB)/prbkt

$(MYLIB)/dblib: dblib
	cp dblib $(MYLIB)/dblib
	-ranlib $(MYLIB)/dblib

$(MYLIB)/fact.h: lib/fact.h
	cp lib/fact.h $(MYLIB)/fact.h

$(LIB)/dload/rlhp.o: rlhp.o
	cp rlhp.o $(LIB)/dload/rlhp.o

$(PLLIB)/rlhp: rlhp.pl
	sed -f $(HOME)/Config.sed rlhp.pl > $(PLLIB)/rlhp


# Clean up some old junk

clean:
	rm -f *.o core newrel prbkt prdd mudd dblib
	cd lib; make clean
