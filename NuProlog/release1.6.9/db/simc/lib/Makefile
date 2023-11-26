#
# NU-Prolog 1.4 SIMC Indexing scheme Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /usr/src/melb/nuprolog
BIN	= /usr/melb
LIB	= /usr/lib/nuprolog
CDEFS	= 
CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Files needed to make the SIMC indexing scheme

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
CFLAGS=-g -I"../lib" -DDBUG -DPAGESIZE=4096 $(CDEFS)
CFLAGS=-O -I"../lib" -DDBUG -DPAGESIZE=4096 $(CDEFS)

.c.o:
	rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

# Create the SIMC indexing scheme library

all:  dblib.a
install:  dblib.a

dblib.a: $(OBJS)
	rm -f dblib.a
	ar cr dblib.a $(OBJS)
	-ranlib dblib.a

$(OBJS): $(INCS)


# Other things to do to the SIMC indexing scheme

lint:
	lint -bh $(SRCS)

clean:
	rm -f core dblib.a $(OBJS)

