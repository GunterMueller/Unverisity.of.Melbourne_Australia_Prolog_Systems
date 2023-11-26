#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1986, 1987 by it.
# 
# All rights are reserved.
#
# Author: John Shepherd
#

#
# NU-Prolog 1.4 Assembler Makefile
#

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /e/users/cs/jws/nepolog

BIN	= /e/users/cs/jws/bin

LIB	= /e/users/cs/jws/nepolog/lib

SHELL = /bin/sh

CDEFS	= 

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

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

.c.o:
	-/bin/rm -f $*.o
	$(CC) -c $(CFLAGS) $*.c

# Files needed to build the Assembler

DEFS	= areas.h labels.h ../nep/bytecodes.h ../nep/defs.h na.h \
		../nep/types.h ../nep/machdeps.h ../nep/tags.h
SRCS	= areas.c ../nep/bytecodes.c ../nep/syms.c ../nep/display.c lex.c \
		main.c parse.y util.c labels.c symtab.c
CSRCS	= areas.c ../nep/bytecodes.c ../nep/syms.c ../nep/display.c lex.c \
		main.c y.tab.c util.c labels.c symtab.c
OBJS	= areas.o ../nep/bytecodes.o ../nep/syms.o ../nep/display.o \
		main.o parse.o lex.o util.o labels.o symtab.o
TEMPS	= y.tab.c y.tab.h parse.c parse.h

CFLAGS	= -I"../nep" -DXXDBUG -DYYDBUG $(CDEFS)
CFLAGS	= -O -I"../nep" $(CDEFS)
LDFLAGS	=
YYFLAGS	= -d
LXFLAGS	=
LIBS	=

# Configure and compile the assmbler

all:	na dis
	@-echo Assembler built

# Install the assembler

install: $(NA) $(NDIS)
	@-echo Assembler installed

$(NA): na
	cp na $(NA)
	cp na $(NUBIN)/na

$(NDIS): dis
	rm -f $(NDIS)
	cp dis $(NDIS)

na: $(OBJS)
	rm -f na
	$(CC) $(LDFLAGS) -o na $(OBJS) $(LIBS)

parse.o parse.h: parse.y na.h
	$(YACC) $(YYFLAGS) parse.y
	$(CC) $(CFLAGS) -c y.tab.c
#rm -f y.tab.c
	mv y.tab.o parse.o
	mv y.tab.h parse.h

lex.o: na.h parse.h lex.c
	$(CC) $(CFLAGS) -c lex.c

lex.c: ../nep/bytecodes.c lex.make lex.c.awk
	sh lex.make "$(CDEFS)"

areas.o lex.o main.o parse.o util.o labels.o symtab.o:						\
		../nep/defs.h ../nep/machdep.h ../nep/bytecodes.h ../nep/syms.h		\
		../nep/types.h ../nep/nltypes.h areas.h ../nep/tags.h ../nep/version.h
areas.h	: ../nep/defs.h

dis: dis.o util.o ../nep/bytecodes.o ../nep/display.o
	rm -f dis
	$(CC) $(LDFLAGS) -o dis dis.o util.o ../nep/bytecodes.o ../nep/display.o

dis.o: ../nep/nltypes.h

../nep/bytecodes.h: ../nep/bytecodes.c
	cd ../nep; make $(CONFIG) bytecodes.h
	
../nep/bytecodes.o:
	cd ../nep; make $(CONFIG) bytecodes.o

../nep/syms.h:
	cd ../nep; make $(CONFIG) syms.h

../nep/syms.c: ../nep/syms.source
	cd ../nep; make $(CONFIG) syms.c

../nep/syms.o: 
	cd ../nep; make $(CONFIG) syms.o

../nep/display.o: ../nep/tags.h
	cd ../nep; make $(CONFIG) display.o

../nep/tags.h:
	cd ../nep; make $(CONFIG) tags.h


# Other things to do with the Assembler

#  Make some lint (good late night reading)

lint	: $(CSRCS)
	lint -ach $(CSRCS)

#  Make a tags file

tags	: $(DEFS) $(SRCS)
	ctags $(DEFS) $(SRCS)

#  Clean up some mess

clean	:
	rm -f $(TEMPS) $(OBJS) na dis dis.o core ERRS .errs

