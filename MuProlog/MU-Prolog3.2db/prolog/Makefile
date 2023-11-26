#	MU-Prolog 3.2 Makefile - prolog interpreter
#
#	The values given here for the configuration parameters are
#	default values only (suitable for use on BSD4.2/3 machines
#	at the University of Melbourne); the site-specific values
#	will be passed in from the main Makefile in the MU-Prolog
#	parent directory. See that file for more details.
#

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

CFLAGS	= -DGSTKLEN=40000 -DDICT -DDLOAD -DCLINDEX -DNEWDBASE 
LFLAGS	=

LD	= /bin/ld

OBJS	= predu.o io.o unify.o init.o data.o preda.o predb.o \
	  predd.o predc.o predc2.o predio.o prede.o dload.o main.o

DBOBJS	= 


# Build prolog binary in current directory

prolog: $(OBJS)
	$(CC) $(LFLAGS) -o prolog $(OBJS)

prolog_db: $(OBJS)
	cd $(HOME)/db/rlhp; make $(CONFIG) dblib rlhp.o
	cd $(HOME)/db/simc; make $(CONFIG) dblib simc.o
	cd $(HOME)/db/sql; make $(CONFIG) sql.o
	cc $(LFLAGS) -o prolog $(OBJS) $(DBOBJS)

local.h: $(HOME)/Config.sed mlocal.c
	cc -DBIN=\"$(PROLOG)\" -DLIB=\"$(LIB)\" $(CFLAGS) mlocal.c
	a.out > local.h
	rm -f a.out

dict.h: local.h mdict.h data.o
	sed -f Makedict.sed mdict.h > makedict.c
#	cp mdict.h makedict.c
#	ed < makedict.ed
	cc $(CFLAGS) data.o makedict.c
	a.out > dict.h
	rm -f a.out makedict.c makedict.o

io.o: types.h pred.h Makefile

unify.o: types.h Makefile

init.o: types.h dict.h pred.h init.h mdict.h Makefile

data.o: types.h Makefile

preda.o: types.h dict.h pred.h Makefile

predb.o: types.h dict.h pred.h Makefile

predc.o: types.h dict.h pred.h Makefile

predc2.o: types.h dict.h pred.h Makefile

predd.o : types.h pred.h Makefile

predio.o: types.h dict.h pred.h Makefile

predu.o: types.h dict.h pred.h Makefile

main.o: types.h dict.h pred.h Makefile

prede.o: types.h pred.h muddpl.h Makefile

dload.o: types.h pred.h local.h dict.h Makefile
	cc $(CFLAGS) -DBIN=\"$(PROLOG)\" -DLD=\"$(LD)\" -c dload.c


# Install prolog and plsave binaries into $(BIN)
# Install plstate file into $(LIB)

install: $(PROLOG) $(SYSLIB)/listing.pl

install_db: prolog_db $(PROLOG) $(SYSLIB)/listing.pl

$(PROLOG): prolog boot.pl preds.pl dcgs.pl listing.pl spy.pl mainlp.pl\
		sys.pl save.pl dbpreds.pl  plsave.pl
	cp prolog $(PROLOG)
	rm -f $(PLSTATE)
	-echo \'save.pl\'. | ./prolog
	cp $(PLSTATE) $(PLSAVE)
	rm -f $(PLSTATE)
	echo \'sys.pl\'. | ./prolog

$(SYSLIB)/listing.pl: listing.pl
	sed -f $(HOME)/Config.sed listing.pl > $(SYSLIB)/listing.pl


# Clean up unnecesary files from the current directory

clean:
	rm -f *.o dict.h local.h core
	rm -f plstate prolog plsave
