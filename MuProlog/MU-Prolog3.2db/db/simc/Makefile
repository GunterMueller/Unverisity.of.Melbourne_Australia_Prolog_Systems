#	MU-Prolog 3.2 Makefile - superimposed codeword indexing scheme

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


# How we make this depends on whether builtin or dynamically loaded

dload: install

builtin: all


all: Lib Bin simc.o

Bin:
	cd bin; make $(CONFIG)

Lib:
	cd lib; make $(CONFIG)

simc.o: simc.c $(HOME)/prolog/types.h $(HOME)/prolog/pred.h
	cc -I"$(HOME)/prolog" -c simc.c

dblib: lib/dblib
	cp lib/dblib dblib

lib/dblib: Lib

install: DestDirs Lib.inst Bin.inst Final.inst

DestDirs:
	@-if [ ! -d $(LIB)/dload ] ; then mkdir $(LIB)/dload ; fi
	@-if [ ! -d $(LIB)/db/simc ] ; then mkdir $(LIB)/db/simc ; fi
Bin.inst:
	cd bin; make $(CONFIG) install

Lib.inst:
	cd lib; make $(CONFIG) install

Final.inst: $(DBLIB)/simc/dblib $(LDLIB)/simc.o $(PLLIB)/simc $(PLLIB)/dsimc

$(LIB)/db/simc/dblib: lib/dblib
	cp lib/dblib $(DBLIB)/simc/dblib
	-ranlib $(DBLIB)/simc/dblib

$(LDLIB)/simc.o: simc.o
	cp simc.o $(LDLIB)/simc.o

$(PLLIB)/simc: simc.pl $(HOME)/Config.sed
	sed -f $(HOME)/Config.sed simc.pl > $(PLLIB)/simc

$(PLLIB)/dsimc: dsimc.pl $(HOME)/Config.sed
	sed -f $(HOME)/Config.sed dsimc.pl > $(PLLIB)/dsimc

# Clean up some mess

clean:
	rm -f simc.o dblib
	cd bin; make clean
	cd lib; make clean
