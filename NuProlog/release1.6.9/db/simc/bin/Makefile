#
# NU-Prolog 1.4 SIMC utilities Makefile
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


# Important files and directories

NP	= $(BIN)/np
NC	= $(BIN)/nc
NA	= $(BIN)/na
NUBIN	= $(LIB)/bin.$(NUVERSION)
NULIB	= $(LIB)/lib.$(NUVERSION)
NDIS	= $(NUBIN)/dis
NEP	= $(NUBIN)/nep
NUC	= $(NUBIN)/nuc
NUP	= $(NUBIN)/nuprolog

LBIN	= $(LIB)/db.$(NUVERSION)/simc
LDIR	= ../lib

BINS	= consult ddb dbdump dbparams dbinfo newrel
OBJS	= consult.o ddb.o dbdump.o dbparams.o dbinfo.o newrel.o
SRCS	= consult.c ddb.c dbdump.c dbparams.c dbinfo.c newrel.c

INCS	= $(LDIR)/simc.h
LINCS	= $(LDIR)/addr.h $(LDIR)/bits.h $(LDIR)/cache.h \
	  $(LDIR)/defs.h $(LDIR)/dfile.h $(LDIR)/params.h \
	  $(LDIR)/parse.h $(LDIR)/rdesc.h $(LDIR)/rec.h \
	  $(LDIR)/reln.h $(LDIR)/sdesc.h $(LDIR)/seg.h \
	  $(LDIR)/split.h $(LDIR)/trans.h $(LDIR)/util.h
LIBS	= $(LDIR)/dblib.a

#CFLAGS	= -I"$(LDIR)" -pg -O -DDBUG -DSFB $(CDEFS)
CFLAGS	= -I"$(LDIR)" -DDBUG -DDBSTATS $(CDEFS)
CFLAGS	= -g -I"$(LDIR)" -DDBUG -DDBSTATS $(CDEFS)
CFLAGS	= -O -I"$(LDIR)" -DDBUG -DDBSTATS $(CDEFS)

.c.o:
	rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

# Create the SIMC utilities

all: $(BINS)
	@-echo SIMC utilities made

# Install the SIMC utilities

install: $(LBIN) $(LBIN)/consult $(LBIN)/ddb $(LBIN)/dbparams $(LBIN)/dbdump $(LBIN)/dbinfo $(LBIN)/newrel
	@-echo SIMC utilities installed

$(LBIN):
	@-if [ ! -d $(LBIN) ] ; then mkdir $(LBIN) ; fi

$(LBIN)/consult: consult
	cp consult $(LBIN)
$(LBIN)/ddb: ddb
	cp ddb $(LBIN)
$(LBIN)/dbparams: dbparams
	cp dbparams $(LBIN)
$(LBIN)/dbdump: dbdump
	cp dbdump $(LBIN)
$(LBIN)/dbinfo: dbinfo
	cp dbinfo $(LBIN)
$(LBIN)/newrel: newrel
	cp newrel $(LBIN)

consult: $(LIBS) $(INCS) consult.o
	-rm -f consult
	$(CC) $(CFLAGS) -o consult consult.o $(LIBS)
dbparams: dbparams.o
	-rm -f dbparams
	$(CC) $(CFLAGS) -o dbparams dbparams.o -lm
ddb: $(LIBS) $(INCS) ddb.o
	-rm -f ddb
	$(CC) $(CFLAGS) -o ddb ddb.o $(LIBS)
dbdump: $(LIBS) $(INCS) dbdump.o
	-rm -f dbdump
	$(CC) $(CFLAGS) -o dbdump dbdump.o $(LIBS)
dbinfo: $(LIBS) $(INCS) dbinfo.o
	-rm -f dbinfo
	$(CC) $(CFLAGS) -o dbinfo dbinfo.o $(LIBS)
newrel: $(LIBS) $(INCS) newrel.o
	-rm -f newrel
	$(CC) $(CFLAGS) -o newrel newrel.o $(LIBS)

$(OBJS): $(INCS)

$(INCS): $(LINCS)

$(LIBS):
	cd $(LDIR); make


# Clean up some mess

clean:
	rm -f core $(OBJS) $(BINS)
