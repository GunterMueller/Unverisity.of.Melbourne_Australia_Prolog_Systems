#
# MU-Prolog 3.2 Makefile - superimposed codeword indexing scheme
#

HOME	= /usr/src/melb/prolog

BIN	= /usr/melb
PROLOG	= $(BIN)/prolog
PLSAVE	= $(BIN)/plsave
LPP	= $(BIN)/lpp
NEGPP	= $(BIN)/negpp

LIB	= /usr/lib/prolog
PLSTATE	= $(LIB)/plstate 
PLLIB	= $(LIB)/pllib

CONFIG	= HOME="$(HOME)" BIN="$(BIN)" LIB="$(LIB)"


OBJS=	bits.o seg.o rec.o fact.o parse.o reln.o files.o trans.o util.o
SRCS=	bits.c seg.c rec.c fact.c parse.c reln.c files.c trans.c util.c
Inc1=	bits.h seg.h rec.h fact.h parse.h reln.h files.h trans.h util.h
Inc2=	muddlib.h defs.h
INCS=	$(Inc1) $(Inc2)

CFLAGS=-I"../lib" $(CDEFS)
CFLAGS=-I"../lib" -DDBUG -DPAGESIZE=4096 $(CDEFS)

VERS=
MSG="Latest version"

dblib: $(OBJS)
	rm -f dblib
	ar cr dblib $(OBJS)
	-ranlib dblib

install: dblib

$(OBJS): $(INCS)

lint:
	lint -bh $(SRCS)

clean:
	rm -f core dblib $(OBJS)
