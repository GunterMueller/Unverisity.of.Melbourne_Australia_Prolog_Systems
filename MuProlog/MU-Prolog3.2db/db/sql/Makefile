#	MU-Prolog 3.2 Makefile - SQL database indexing scheme

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

CFLAGS	= -O

INCDIR	= $(HOME)/prolog

# How we make this depends on whether builtin or dynamically loaded

dload: install

builtin: all


# Create the utility programs for the RLHP indexing scheme

all: initdb sql.o

initdb: initdb.c $(INCDIR)/types.h $(INCDIR)/pred.h $(INCDIR)/local.h sql.h
	$(CC) -o initdb -I"$(INCDIR)" $(CFLAGS) $(CDEFS) initdb.c

sql.o: sql.c $(INCDIR)/types.h $(INCDIR)/pred.h $(INCDIR)/local.h sql.h
	$(CC) -c -I"$(INCDIR)" $(CFLAGS) $(CDEFS) sql.c


# Install the Prolog libraries and utilities for the SQL indexing scheme

install: $(BIN)/initdb $(LIB)/dload/sql.o $(PLLIB)/sql

$(BIN)/initdb: initdb
	cp initdb $(BIN)/initdb

$(LIB)/dload/sql.o: sql.o
	cp sql.o $(LIB)/dload/sql.o

$(PLLIB)/sql: sql.pl
	sed -f $(HOME)/Config.sed sql.pl > $(PLLIB)/sql

install2: $(BIN)/sqlpp

$(BIN)/sqlpp: sqlpp
	cp sqlpp $(BIN)/sqlpp

sqlpp: sqlpp.pl
	$(PROLOG) $(PLSAVE) sqlpp.pl
#	/mip/jas/bin/save sqlpp.pl # under SysV

# Clean up some old junk

clean:
	rm -f *.o core
