#
# MU-Prolog 3.2 Makefile - superimposed codeword indexing utilities
#

HOME	= /usr/src/melb/prolog
HOME	= /mip/jas/mup

BIN	= /usr/melb
BIN	= /mip/jas/db/bin
PROLOG	= $(BIN)/prolog
PLSAVE	= $(BIN)/plsave
LPP	= $(BIN)/lpp
NEGPP	= $(BIN)/negpp

LIB	= /usr/lib/prolog
LIB	= /mip/jas/db/lib/prolog
PLSTATE	= $(LIB)/plstate 
PLLIB	= $(LIB)/pllib

CONFIG	= HOME="$(HOME)" BIN="$(BIN)" LIB="$(LIB)"

LBIN=	$(LIB)/db/simc
LDIR=	$(HOME)/db/simc/lib

BINS=	consult dbdump dbinfo dbparams ddb newrel prefix
OBJS=	consult.o dbdump.o dbinfo.o dbparams.o ddb.o newrel.o
SRCS=	consult.c dbdump.c dbinfo.c dbparams.c ddb.c newrel.c

INCS	= $(LDIR)/simc.h
LINCS	= $(LDIR)/addr.h $(LDIR)/bits.h $(LDIR)/cache.h \
	  $(LDIR)/defs.h $(LDIR)/dfile.h $(LDIR)/params.h \
	  $(LDIR)/parse.h $(LDIR)/rdesc.h $(LDIR)/rec.h \
	  $(LDIR)/reln.h $(LDIR)/sdesc.h $(LDIR)/seg.h \
	  $(LDIR)/split.h $(LDIR)/trans.h $(LDIR)/util.h
LIBS	= $(LDIR)/dblib

CFLAGS=	-I"$(LDIR)" -pg -DDBUG -cckr
CFLAGS=	-I"$(LDIR)" -O -DDBUG -cckr

VERS=
MSG="Latest version"

all: $(BINS)

install: $(LBIN)/consult $(LBIN)/dbdump $(LBIN)/dbinfo $(LBIN)/dbparams $(LBIN)/ddb $(LBIN)/newrel $(LBIN)/prefix

$(LBIN)/consult: consult
	cp consult $(LBIN)

$(LBIN)/dbdump: dbdump
	cp dbdump $(LBIN)

$(LBIN)/dbinfo: dbinfo
	cp dbinfo $(LBIN)

$(LBIN)/ddb: ddb
	cp ddb $(LBIN)

$(LBIN)/dbparams: dbparams
	cp dbparams $(LBIN)

$(LBIN)/newrel: newrel
	cp newrel $(LBIN)

$(LBIN)/prefix: prefix
	cp prefix $(LBIN)

consult: $(LIBS) $(INCS) consult.o
	$(CC) $(CFLAGS) -o consult consult.o $(LIBS)
dbdump: dbdump.o
	$(CC) $(CFLAGS) -o dbdump dbdump.o $(LIBS)
dbparams: dbparams.o
	$(CC) $(CFLAGS) -o dbparams dbparams.o -lm
ddb: $(LIBS) $(INCS) ddb.o
	$(CC) $(CFLAGS) -o ddb ddb.o $(LIBS)
dbinfo: $(LIBS) $(INCS) dbinfo.o
	$(CC) $(CFLAGS) -o dbinfo dbinfo.o $(LIBS)
prefix: prefix.pl
	$(PROLOG) $(PLSAVE) prefix.pl
newrel: $(LIBS) $(INCS) newrel.o
	$(CC) $(CFLAGS) -o newrel newrel.o $(LIBS)

$(OBJS): $(INCS)

$(INCS): $(LINCS)

$(LIBS):
	cd $(LDIR); make

clean:
	rm -f core $(OBJS) $(BINS)

version:
	co -l$(VERS) $(INCS) $(SRCS)

newvers:
	ci -l -m$(MSG) $(INCS) $(SRCS)
