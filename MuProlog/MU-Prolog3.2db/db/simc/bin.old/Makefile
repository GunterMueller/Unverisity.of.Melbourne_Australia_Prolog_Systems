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

BINS=	assert consult ddb dbparams dbinfo newrel prefix query
OBJS=	assert.o consult.o ddb.o dbparams.o dbinfo.o newrel.o query.o
SRCS=	assert.c consult.c ddb.c dbparams.c dbinfo.c newrel.c query.c

INCS=	$(LDIR)/muddlib.h
LINCS=	$(LDIR)/bits.h $(LDIR)/defs.h $(LDIR)/fact.h $(LDIR)/files.h \
	$(LDIR)/parse.h $(LDIR)/rec.h $(LDIR)/reln.h $(LDIR)/seg.h \
	$(LDIR)/trans.h $(LDIR)/util.h
LIBS=	$(LDIR)/dblib

CFLAGS=	-I"$(LDIR)" -pg -DDBUG
CFLAGS=	-I"$(LDIR)" -DDBUG
CFLAGS=	-I"$(LDIR)" -O
CFLAGS=	-I"$(LDIR)" -O -DDBUG

VERS=
MSG="Latest version"

all: $(BINS)

install: $(LBIN)/assert $(LBIN)/consult $(LBIN)/ddb $(LBIN)/dbparams $(LBIN)/dbinfo $(LBIN)/newrel $(LBIN)/query

$(LBIN)/assert: assert
	cp assert $(LBIN)

$(LBIN)/consult: consult
	cp consult $(LBIN)

$(LBIN)/ddb: ddb
	cp ddb $(LBIN)

$(LBIN)/dbparams: dbparams
	cp dbparams $(LBIN)

$(LBIN)/dbinfo: dbinfo
	cp dbinfo $(LBIN)

$(LBIN)/newrel: newrel
	cp newrel $(LBIN)

$(LBIN)/query: query
	cp query $(LBIN)

assert: $(LIBS) $(INCS) assert.o
	$(CC) $(CFLAGS) -o assert assert.o $(LIBS)
consult: $(LIBS) $(INCS) consult.o
	$(CC) $(CFLAGS) -o consult consult.o $(LIBS)
dbparams: dbparams.o
	$(CC) $(CFLAGS) -o dbparams dbparams.o -lm
ddb: $(LIBS) $(INCS) ddb.o
	$(CC) $(CFLAGS) -o ddb ddb.o $(LIBS)
dbinfo: $(LIBS) $(INCS) dbinfo.o
	$(CC) $(CFLAGS) -o dbinfo dbinfo.o $(LIBS)
query: $(LIBS) $(INCS) query.o
	$(CC) $(CFLAGS) -o query query.o $(LIBS)
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
