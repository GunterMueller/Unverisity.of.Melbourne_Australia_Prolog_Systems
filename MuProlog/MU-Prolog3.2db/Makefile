#	MU-Prolog 3.2 Makefile
#
#	!!! IMPORTANT !!!
#	Before you do a make, check that CFLAGS is to your liking (see below)
#	and, more importantly, BIN is set to the directory where  you want the
#	prolog binaries (prolog, plsave, lpp, etc.) LIB to the top-level prolog
#	library directory and HOME to the current directory.
#
#	Flags that may be of use
#	========================
#
#	GSTKLEN=nnnnn	Used to change the default size of the stack+heap.
#		A large size is recommended for the VAX, say 200000.
#		If you dont have VM and are not running large programs, try
#		40000. You can also edit "types.h".
#
#	DICT	Use a large dictionary. May be necessary for big 
#		programs that use lots of atoms (eg some databases).
#		With large external databases, strings should be used rather
#		than atoms, to save dictionary space.
#		You can also edit "types.h" and (perhaps) change the hash
#		function in "data.c".
#
#	DBASE	For external database version without dynamic loading.
#
#	NEWDBASE For new (dynamically loaded) external database.
#
#	CLINDEX	Clause indexing.  Defines the $clindex predicate.
#		Still under development.
#
#	DLOAD	Dynamic loading of .o files.  Defines the $dload predicate.
#		Still under development.
#
#	PRV	Can be used if the Melbourne University Privileges system
#		is available (you need -lpriv when you compile prolog also).
#		Makes the 'privilege' predicate available, to check if the
#		user is a student etc.
#
#	-k 32k	This is a loader option which sets the C stack size to
#		32k bytes on (some) Perkin Elmer systems which have a very
#		small default stack size.  If the stack is too small, reading
#		and writing very deeply nested terms causes an overflow.
#

HOME	= /usr/src/local/muprolog
HOME	= /home/munta/staff/jws/src/mup

BIN	= /usr/local/bin
BIN	= /home/munta/staff/jws/bin
PROLOG	= $(BIN)/prolog
PLSAVE	= $(BIN)/plsave
LPP	= $(BIN)/lpp
NEGPP	= $(BIN)/negpp

LIB	= /usr/local/lib/muprolog
LIB	= /home/munta/staff/jws/lib/muprolog
PLSTATE	= $(LIB)/plstate 
PLLIB	= $(LIB)/pllib
LDLIB	= $(LIB)/dload
DBLIB	= $(LIB)/db
SYSLIB	= $(LIB)/sys

# Configuration parameters for the Unify/SQL interface:
# Don't define BSD4 for System V-type systems
# Don't define SQL if you don't want the Unify system
#  Also, if no Unify, then remove reference to sql.o in DBOBJS

CDEFS	= -DBSD4 -DSQL -DGSTKLEN=40000 -cckr
CDEFS	= -DBSD4 -cckr
UNIFY	= /stude/unify/lib
SQL_PATH= /stude/unify/bin
SQL_EXEC= /stude/unify/bin/SQL
LOCKFILE= lockfile
DDLOCKFILE= ddlockfile

# For building database system without dynamic loading

DBOBJS	= $(HOME)/db/rlhp/rlhp.o $(HOME)/db/rlhp/dblib \
	  $(HOME)/db/simc/simc.o $(HOME)/db/simc/dblib
#	  $(HOME)/db/simc/simc.o $(HOME)/db/simc/dblib \
#	  $(HOME)/db/sql/sql.o


CONFIG	= HOME="$(HOME)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" \
	  UNIFY="$(UNIFY)" SQL_PATH="$(SQL_PATH)" SQL_EXEC="$(SQL_EXEC)" \
	  LOCKFILE="$(LOCKFILE)" DDLOCKFILE="$(DDLOCKFILE)"

# Configuration parameters for Prolog interpreter:
#
# SysV/NonVAX.BSD: 
#	CFLAGS	= -O -DGSTKLEN=500000 -DDICT -DCLINDEX -DDBASE
# VAX.BSD:
#	CFLAGS	= -O -DGSTKLEN=500000 -DDICT -DCLINDEX -DDLOAD -DNEWDBASE
# PE3420.BSD:
#	CFLAGS	= -O -DGSTKLEN=40000 -DDICT -DCLINDEX -DDLOAD -DNEWDBASE
#	LFLAGS	= -k 32k

CFLAGS	= -O -DGSTKLEN=100000 -DDICT -DCLINDEX -DDLOAD -DNEWDBASE
CFLAGS	= -O -DGSTKLEN=500000 -DDICT -DCLINDEX -DDLOAD -DNEWDBASE
CFLAGS	= -O -DGSTKLEN=50000 -DDICT -DCLINDEX -DDBASE
CFLAGS	= -O -DGSTKLEN=50000 -DDICT -DCLINDEX -DDBASE -cckr
LFLAGS	= -k 32k
LFLAGS	=

LD	= /bin/ld

PLFLAGS	= LD="$(LD)" CFLAGS="$(CFLAGS) $(CDEFS)" LFLAGS="$(LFLAGS)"


# If user simply types "make", assume that they aren't sure
# what they're doing, so print some helpful advice at them

intro:	MakeInfo
	@cat MakeInfo


# Organise the installation of MU-Prolog system
# The steps are:
#	Make sure that the configuration sed script is set up
#	Make sure that the (top level) destination directories exist
#	Install (possibly after compiling) the MU-Prolog binaries
#	Install the standard libraries
#	Install the database system, if the user requires it
#	Install the lpp and negpp pre-processors

install: Config.sed DestDirs Prolog Db Lib Lpp Negpp Sql
	@-echo Make \"install\" done

Config.sed: Makefile
	@rm -f Config.sed ; \
	echo "/^%##/{ s/^%##//;" >> Config.sed ; \
	echo "s/BIN/`echo $(BIN) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed ; \
	echo "s/HOME/`echo $(HOME) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed ; \
	echo "s/LIB/`echo $(LIB) | sed -e 's/\//\\\\\//g'`/g;n;d" >> Config.sed ; \
	echo "}" >> Config.sed

DestDirs:
	@-if [ ! -d $(BIN) ] ; then mkdir $(BIN) ; fi
	@-if [ ! -d $(LIB) ] ; then mkdir $(LIB) ; fi
	@-if [ ! -d $(PLLIB) ] ; then mkdir $(PLLIB) ; fi
	@-if [ ! -d $(LDLIB) ] ; then mkdir $(LDLIB) ; fi
	@-if [ ! -d $(DBLIB) ] ; then mkdir $(DBLIB) ; fi
	@-if [ ! -d $(SYSLIB) ] ; then mkdir $(SYSLIB) ; fi

Prolog:
	@-if expr "$(CFLAGS)" : '.*NEWDBASE.*' > /dev/null; \
	then \
		cd prolog; make $(CONFIG) $(PLFLAGS) install ; \
	elif expr "$(CFLAGS)" : '.*DBASE.*' > /dev/null; \
	then \
		cd prolog; make $(CONFIG) $(PLFLAGS) DBOBJS="$(DBOBJS)" install_db ; \
	else \
		cd prolog; make $(CONFIG) $(PLFLAGS) install ; \
	fi

Lib:
	cd pllib; make $(CONFIG) install

Db:
	@-if expr "$(CFLAGS)" : '.*DBASE.*' > /dev/null; \
	then \
		cd db; make $(CONFIG) install ; \
	else \
		echo No database ; \
	fi

Lpp:
	cd lpp; make $(CONFIG) install

Negpp:
	cd negpp; make $(CONFIG) install

Sql:
	@-if expr "$(CFLAGS)" : '.*DBASE.*' > /dev/null; \
	then \
		cd db; make $(CONFIG) install2 ; \
	else \
		echo No database ; \
	fi

# Clean up the mess after it has been installed
# and seems to be working OK, or just before installing

clean:
	cd prolog; make clean
	cd db; make clean
	cd lpp; make clean
	cd negpp; make clean
