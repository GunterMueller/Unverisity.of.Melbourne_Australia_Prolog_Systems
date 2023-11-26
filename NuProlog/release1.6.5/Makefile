#
# NU-Prolog 1.6.2 Makefile
#

# Configuration Parameters:
#
#	Tailor these definitions to suit the local environment.
#
#	There are two definitions for each tunable parameter.
#	The first definition is a standard default.
#	The second definition should be modified or deleted.

SRC	= /usr/local/src/prolog/nuprolog/release1.6.5
SRC	= /home/muse/cs/mip/jws/release1.6.5

BIN	= /usr/local/bin
BIN	= /home/muse/cs/mip/jws/bin/sgi

LIB	= /usr/local/lib/prolog/nuprolog
LIB	= /home/muse/cs/mip/jws/lib/nuprolog/sgi

# Machine type.  Select one of the following machine categories.  If
# more than one is appropriate, choose the first occuring one.
#
# Machines not on this list may need work.
#
#	MACHINE_SUN3	MACHINE_SUN4	MACHINE_MACII
#	MACHINE_ENCORE			# Plain BSD or SysV
#	MACHINE_ENCORE_BSD_EMUL	# BSD Emulation under SysV port
#	MACHINE_VAX		MACHINE_386		MACHINE_ELXSI	MACHINE_MIPS
#	MACHINE_SGI
#	MACHINE_DEC				# DECstations (MIPS)
#	MACHINE_SUN4_SOLARIS	# Sun4 running Solaris 2
#	MACHINE_ALPHA			# Alpha AXP running OSF/1
#
#	MACHINE_GENERIC

MACHINE	= -DMACHINE_SGI

#
# SysV: 
#	BSD4 = 0
#	CDEFS	=
# BSD:
#	BSD4 = 1
#	CDEFS	= -DBSD4
# or, if your loader supports -A, for the foreign function interface
#	CDEFS = -DBSD4 -DFLOAD
# This is not available for MACHINE_SUN4_SOLARIS.

BSD4 = 1
CDEFS	= $(MACHINE) -DBSD4 -DLARGEMODEL
CDEFS	= $(MACHINE) -DBSD4 -DPNP
CDEFS	= $(MACHINE) -DBSD4
CDEFS	= $(MACHINE) -DBSD4 -DNO_LOCAL_VARS -DSEGMENTED_STACKS
CDEFS	= $(MACHINE) -DBSD4 -DNO_LOCAL_VARS -DSHALLOW
CDEFS	= $(MACHINE) -DBSD4 -DNO_LOCAL_VARS -DPNP
CDEFS	= $(MACHINE) -DBSD4
CDEFS	= $(MACHINE) -DBSD4 -DFLOAD

#
# Some multi-processors like Encore have parallel makes.
# PARMAKE can be used to stimulate these.
#
#Note that it is not possible to do the top-level make in parallel.
#

PARMAKE = make -N 3
PARMAKE = pmake -v -J 3
PARMAKE = make

CC = /usr/local/bin/gcc
CC = /usr/local/gcc/bin/gcc
CC = cc

# End Configuration Parameters
#
#	DO NOT modify anything below here.

# 1.6.5
NUVERSION = 10605

SHELL = /bin/sh

CONFIG	= PARMAKE="$(PARMAKE)" SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" CC="$(CC)" NUVERSION="$(NUVERSION)"


# These definitions are reconstructed in the Makefiles in
# the subdirectories of the NU-Prolog distribution.
# They could have been passed via CONFIG ... but weren't.
# They aren't always needed in the sub-makes.

NP	= $(BIN)/np
NC	= $(BIN)/nc
NA	= $(BIN)/na
NIT	= $(BIN)/nit
NUBIN	= $(LIB)/bin.$(NUVERSION)
NULIB	= $(LIB)/lib.$(NUVERSION)
NUDDB	= $(LIB)/db.$(NUVERSION)
NDIS	= $(NUBIN)/dis
NEP	= $(NUBIN)/nep
NUC	= $(NUBIN)/nuc
NUP	= $(NUBIN)/nuprolog


# If user simply types "make", assume that they aren't sure
# what they're doing, so print some helpful advice at them.

intro:	MakeInfo
	@cat MakeInfo


# Organise the installation of NU-Prolog system
# The steps are:
#	Make sure that the configuration sed script is set up
#	Make sure that the destination directories exist
#	Install (possibly compiling) the assembler
#	Install (possibly compiling) the interpreter/run-time system
#	Install (possibly compiling) the compiler
#	Install the standard libraries
#	Install the database system
# NOTE: The order of these steps is important. Don't change it.

install: Config.sed DestDirs Db Np1 Na Np2 Nc Sys Lib Nit Nac Man Rev Db2
	@echo Make \"install\" done

Config.sed: Makefile
	@-echo "Reconfiguring ..."
	@rm -f Config.sed
	@echo "/^[#%]##/{ s/^[#%]##//;" >> Config.sed
	@echo "s/NP/`echo $(NP) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NC/`echo $(NC) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NA/`echo $(NA) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NUBIN/`echo $(NUBIN) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NULIB/`echo $(NULIB) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NDIS/`echo $(NDIS) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NEP/`echo $(NEP) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NUC/`echo $(NUC) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NUP/`echo $(NUP) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/SRC/`echo $(SRC) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/LIB/`echo $(LIB) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/BIN/`echo $(BIN) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/BSD4/`echo $(BSD4) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "s/NUVERSION/`echo $(NUVERSION) | sed -e 's/\//\\\\\//g'`/g;" >> Config.sed
	@echo "n;d" >> Config.sed
	@echo "}" >> Config.sed
	@-echo "Extracting Makefile(s) ..."
	@rm -f nep/Makefile
	@awk -f nep/ifdef.awk defines="$(MACHINE)" nep/Makefile.skel >nep/Makefile
	@-echo "Creating version files ..."
	@rm -f Version
	@echo "$(NUVERSION)" > Version
	@rm -f nep/version.h
	@echo "#define NUVERSION $(NUVERSION)" > nep/version.h

DestDirs:
	@-if [ ! -d $(BIN) ] ; then mkdir $(BIN) ; fi
	@-if [ ! -d $(LIB) ] ; then mkdir $(LIB) ; fi
	@-if [ ! -d $(NUBIN) ] ; then mkdir $(NUBIN) ; fi
	@-if [ ! -d $(NULIB) ] ; then mkdir $(NULIB) ; fi
	@-if [ ! -d $(NULIB)/quintus ] ; then mkdir $(NULIB)/quintus ; fi
	@-if [ ! -d $(NUDDB) ] ; then mkdir $(NUDDB) ; fi

Na:
	cd na; make $(CONFIG) install

Np1:
	cd nep; $(PARMAKE) $(CONFIG) nep

Np2:
	cd nep; $(PARMAKE) $(CONFIG) install

Nc:
	cd co; $(PARMAKE) $(CONFIG) install

Sys:
	cd sys; $(PARMAKE) $(CONFIG) install

Lib:
	cd lib; $(PARMAKE) $(CONFIG) install

Db:
	cd db; $(PARMAKE) $(CONFIG) install

Nit:
	cd nit; $(PARMAKE) $(CONFIG) install

Nac:
	cd nac; $(PARMAKE) $(CONFIG) install

Man:
#	echo Making the manual takes too long.
	cd man; make $(CONFIG) install

Rev:
	cd revise; $(PARMAKE) $(CONFIG) install

Db2:
	cd db; $(PARMAKE) $(CONFIG) install2


# Clean up the mess after the system has been installed
# and seems to be working OK, or just before re-installing

clean:
	rm -f ERRS core a.out a.out.save
	cd co; make clean
	cd db; make clean
	cd lib; make clean
	cd na; make clean
	cd nep; make clean
	cd sys; make clean
	cd revise; make clean
	cd nit; make clean
	cd nac; make clean
	cd man; make clean

realclean: clean
	rm -fr $(NULIB)
