#
# NU-Prolog 1.4 SQL Database system Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /e/users/cs/jws/nepolog

BIN	= /e/users/cs/jws/bin
BIN	= /mip/jas/bin

LIB	= /mip/usr/lib/nepolog

CDEFS	= 

UNIFY	= /stude/unify/lib
SQL_PATH= /stude/unify/bin
SQL_EXEC= /stude/unify/bin/SQL

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" CC="$(CC)" NUVERSION="$(NUVERSION)"

CCONFIG	= -DUNIFY=\"$(UNIFY)\" -DSQL_PATH=\"$(SQL_PATH)\" -DSQL_EXEC=\"$(SQL_EXEC)\"

# End Configuration Parameters


# Definitions of some important directories/files

NP	= $(BIN)/np
NC	= $(BIN)/nc
NA	= $(BIN)/na
NUBIN	= $(LIB)/bin.$(NUVERSION)
NULIB	= $(LIB)/lib.$(NUVERSION)
NDIS	= $(NUBIN)/dis
NEP	= $(NUBIN)/nep
NUC	= $(NUBIN)/nuc
NUP	= $(NUBIN)/nuprolog

CFLAGS = -O

.c.o:
	-/bin/rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

# Compile the SQL indexing scheme

all:  initdb
	@-echo SQL database scheme compiled

install:  $(BIN)/initdb
	@-echo SQL database scheme installed

install2: $(BIN)/sqlpp

$(BIN)/initdb:  initdb
	cp initdb $(BIN)/initdb

initdb:  initdb.c
	$(CC) $(CCONFIG) $(CDEFS) -DNC=\"$(NC)\" -o initdb initdb.c

$(BIN)/sqlpp: sqlpp
	cp sqlpp $(BIN)/sqlpp

sqlpp: sqlpp.nl
	$(NC) -o sqlpp sqlpp.nl -l sql


# Clean up SQL files

clean:
	-/bin/rm -f initdb initdb.o .errs ERR core *.save sqlpp *.n[os]
