#
# NU-Prolog 1.4 NAC Makefile
#
# To make "nac", NU-Prolog must be *installed*

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /nip/jws/nepolog

BIN	= /nip/usr/bin

LIB	= /nip/usr/lib/nuprolog

SHELL = /bin/sh

CDEFS	=

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Files needed to build Nac

NC	= $(BIN)/nc
NAC	= $(BIN)/nac
NACS	= $(LIB)/bin.$(NUVERSION)/nac.save

.SUFFIXES: .no .ns .nl

.nl.ns: ; $(NC) -S $*.nl
.ns.no: ; $(NC) -c $*.ns

NLSRCS	= nac.nl addwhen.nl istest.nl
SRCS	= nac.ns addwhen.ns istest.ns
OBJS	= nac.no addwhen.no istest.no


# Configure and compile Nac

all:  nac
	@-echo Nac built

nac nac.save:  $(OBJS) $(LIB)/bin.$(NUVERSION)/nep
	@-rm -f nac.save
	$(NC) -o nac $(OBJS)

nac.no: nac.ns
addwhen.no: addwhen.ns
istest.no: istest.ns

# Install Nac

install:  $(NACS)
	@-echo Nac installed

$(NAC):  nac
	cp nac $(BIN)

$(NACS): nac.save
	echo $(LIB)/bin.$(NUVERSION)/nep -R $(NACS) -a '$$*' > $(NAC)
	cp nac.save $(NACS)


# Other things to do with Nac


# Clean up some mess

clean:
	-rm -f *.no *.ns
	rm -f nac nac.save core .errs ERRS
