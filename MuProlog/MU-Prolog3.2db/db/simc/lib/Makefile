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


OBJS=	addr.o bits.o cache.o dfile.o parse.o rdesc.o rec.o \
	reln.o sdesc.o seg.o split.o trans.o util.o
SRCS=	addr.c bits.c cache.c dfile.c parse.c rdesc.c rec.c \
	reln.c sdesc.c seg.c split.c trans.c util.c
Inc1=	addr.h bits.h cache.h dfile.h parse.h rdesc.h rec.h \
	reln.h sdesc.h seg.h split.h trans.h util.h
Inc2=	simc.h params.h defs.h
INCS=	$(Inc1) $(Inc2)

CFLAGS=-I"../lib" -DDBUG -DDBSTATS -DTIME_STAMP -DPAGESIZE=4096 $(CDEFS)
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
