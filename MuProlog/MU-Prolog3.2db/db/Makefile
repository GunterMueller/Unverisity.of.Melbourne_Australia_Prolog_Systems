#	MU-Prolog 3.2 Makefile - database indexing schemes

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

CONFIG	= HOME="$(HOME)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" \
	  UNIFY="$(UNIFY)" SQL_PATH="$(SQL_PATH)" SQL_EXEC="$(SQL_EXEC)" \
	  LOCKFILE="$(LOCKFILE)" DDLOCKFILE="$(DDLOCKFILE)"

# Compile all of the indexing schemes, but don't install them

builtin:
	cd rlhp; make $(CONFIG) builtin
	cd simc; make $(CONFIG) builtin
	@-if expr "$(CDEFS)" : '.*SQL.*' > /dev/null; \
	then \
		cd sql; make $(CONFIG) builtin ; \
	fi

dload:
	cd rlhp; make $(CONFIG) dload
	cd simc; make $(CONFIG) dload
	@-if expr "$(CDEFS)" : '.*SQL.*' > /dev/null; \
	then \
		cd sql; make $(CONFIG) dload ; \
	fi


# Install (possibly after compiling) the database indexing schemes

install: $(PLLIB)/db
	cd rlhp; make $(CONFIG) install
	cd simc; make $(CONFIG) install
	@-if expr "$(CDEFS)" : '.*SQL.*' > /dev/null; \
	then \
		cd sql; make $(CONFIG) install ; \
	fi

install2:
	@-if expr "$(CDEFS)" : '.*SQL.*' > /dev/null; \
	then \
		cd sql; make $(CONFIG) install2 ; \
	fi

$(PLLIB)/db: pllib/dbpreds.pl
	sed -f $(HOME)/Config.sed  pllib/dbpreds.pl > $(PLLIB)/db


# Organise clean up in sub-directories

clean:
	cd rlhp; make clean
	cd simc; make clean
	cd sql; make clean
