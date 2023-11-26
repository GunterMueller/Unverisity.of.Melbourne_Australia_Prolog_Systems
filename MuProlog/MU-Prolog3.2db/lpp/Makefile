#	MU-Prolog 3.2 Makefile - logic pre-processor

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


# Create the lpp save file
# Can't do this util Prolog has been installed

lpp: lpp.pl addwait istest asgen noreorder system
#	$(PROLOG) $(PLSAVE) lpp.pl		NO GOOD under SysV
#	/mip/jas/bin/save lpp.pl		NO GOOD under BSD
	$(PROLOG) $(PLSAVE) lpp.pl

lpp.pl: lpp.pl.dist $(HOME)/Config.sed
	sed -f $(HOME)/Config.sed lpp.pl.dist > lpp.pl


# Install the lpp save file in $(BIN)

install: $(LPP)

$(LPP): lpp
	cp lpp $(LPP)


# Clean up the old lpp save file

clean:
	rm -f lpp
#	rm -f lpp.save # under SysV
