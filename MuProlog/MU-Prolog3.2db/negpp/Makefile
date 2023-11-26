#	MU-Prolog 3.2 Makefile - negation pre-processor

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


SRCS=	sysneg.pl solutions.pl \
	ndsysneg.pl ndnoteq.pl ndsolutions.pl \
	npp.pl \
	negpp.pl negtrf.pl \
	unegpp.pl unegtrf.pl


# Create the negpp save file

negpp: negpp.pl npp.pl negtrf.pl $(PLSTATE)
#	$(PROLOG) $(PLSAVE) negpp.pl	NO GOOD under SysV
#	/mip/jas/bin/save negpp.pl	NO GOOD under BSD
	$(PROLOG) $(PLSAVE) negpp.pl

unegpp: unegpp.pl npp.pl unegtrf.pl $(PLSTATE)
#	$(PROLOG) $(PLSAVE) unegpp.pl	NO GOOD under SysV
#	/mip/jas/bin/save unegpp.pl	NO GOOD under BSD
	$(PROLOG) $(PLSAVE) unegpp.pl

$(PLSTATE):


# Install the negpp save file into $(BIN)

install: $(BIN)/negpp $(BIN)/unegpp

$(BIN)/negpp: negpp
	cp negpp $(BIN)/negpp

$(BIN)/unegpp: unegpp
	cp unegpp $(BIN)/unegpp



# Clean up the negpp and unegpp save files

clean:
	rm -f negpp unegpp
#	rm -f negpp.save unegpp.save # under SysV
