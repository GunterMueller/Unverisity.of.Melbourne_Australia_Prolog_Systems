#	MU-Prolog 3.2 Makefile - Prolog libraries

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

normal:
	@-echo "lib() won't work until the libraries are installed"

install: unstall
	@-echo "Libraries installed"

unstall:
	@-if [ ! -d $(PLLIB) ] ; then mkdir $(PLLIB) ; fi
	@-for libf in * ; \
	do \
		sed -f $(HOME)/Config.sed $$libf > $(PLLIB)/$$libf ; \
	done

