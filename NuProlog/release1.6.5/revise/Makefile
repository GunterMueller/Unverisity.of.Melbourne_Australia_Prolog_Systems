#
# NU-Prolog 1.4 Revise (incremental compiler) Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /home/mungunni/jws/nepolog

BIN	= /home/mungunni/jws/bin

LIB	= /home/mungunni/lib/nuprolog

SHELL = /bin/sh

CDEFS	= 

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Important files and directories

NP	= $(BIN)/np
NC	= $(BIN)/nc
NA	= $(BIN)/na
NUBIN	= $(LIB)/bin.$(NUVERSION)
NUBIN	= $(BIN)
NULIB	= $(LIB)/lib.$(NUVERSION)
NDIS	= $(NUBIN)/dis
NEP	= $(NUBIN)/nep
NUC	= $(NUBIN)/nuc
NUP	= $(NUBIN)/nuprolog

BINS	= nldiffs nsfix revise rnc
OBJS	= nldiffs.o nsfix.o
SRCS	= nldiffs.c nsfix.c revise.SH rnc.SH

CFLAGS	=

.c.o:
	-rm -f $*.o
	$(CC) -c $(CFLAGS) $(CDEFS) $*.c

# Create the Revise utilities

all: $(BINS)
	@-echo Revise utilities made

nldiffs: nldiffs.o
	-rm -f nldiffs
	$(CC) -o nldiffs nldiffs.o

nsfix: nsfix.o
	-rm -f nsfix
	$(CC) -o nsfix nsfix.o

# Install the Revise utilities

install: $(BIN)/rnc $(BIN)/redit $(BIN)/revise $(NUBIN)/nldiffs $(NUBIN)/nsfix
	@-echo Revise utilities installed

revise: revise.SH $(SRC)/Config.sed
	sed -f $(SRC)/Config.sed revise.SH > revise
	-chmod 755 revise

rnc: rnc.SH $(SRC)/Config.sed
	sed -f $(SRC)/Config.sed rnc.SH > rnc
	-chmod 755 rnc

$(BIN)/revise: revise
	/bin/rm -f $(BIN)/revise
	/bin/cp revise $(BIN)/revise
	-chmod 755 $(BIN)/revise

$(BIN)/rnc: rnc
	/bin/rm -f $(BIN)/rnc
	/bin/cp rnc $(BIN)/rnc
	-chmod 755 $(BIN)/rnc

$(BIN)/redit: redit
	/bin/rm -f $(BIN)/redit
	/bin/cp redit $(BIN)/redit
	-chmod 755 $(BIN)/redit

$(NUBIN)/nldiffs: nldiffs
	cp nldiffs $(NUBIN)

$(NUBIN)/nsfix: nsfix
	cp nsfix $(NUBIN)


# Clean up some mess

clean:
	-rm -f core $(OBJS) $(BINS)
