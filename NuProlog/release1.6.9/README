/*
 * Please note that NU-Prolog in all its versions is the property of
 * The University of Melbourne and is Copyright 1985, 1995 by it.
 * 
 * All rights are reserved.
 *
 * NU-Prolog is made available for teaching and academic research.
 * It may not be redistributed or used for any other purpose without
 * written permission.
 */

NU-Prolog 1.6.9 Distribution

This is the top level of the NU-Prolog distribution tree.  The
NU-Prolog Manual is available separately.

If you have retrieved a copy of NU-Prolog by FTP please send mail to
mip@cs.mu.oz.au with
	your name,
	your institution, and
	the version retrieved.
Please do not make your copy available for further redistribution.


The directory should contain the following files and directories:

README
	This file

Copyright
	The copyright message

NOTES
	Occasional notes on a particular release

Makefile
	Main Makefile for the NU-Prolog system.  *Before* making the system,
	you should check that the configuration parameters are correct for
	your site.

	The major configuration parameters are:

	SRC	Name of NU-Prolog source directory (i.e. current directory)
	    [default: /local/src/nuprolog]
	BIN	Directory where major executables are to be installed
	    [default: /usr/local/bin]
	LIB	Directory where libraries and other executables are installed
	    [default: /usr/local/lib/prolog/nuprolog]
	MACHINE	The type of machine (and OS) to build for.
		Choose one of those listed.
	BSD4	Whether the machine is "enough" like BSD 4 for NU-Prolog to
		be happy.  Even System V machines are usually like enough
		for this to be set.
	    [default: 1]
	CDEFS	Global definitions for C programs in the system.  -DFLOAD
		enables the foreign function interface.  This works on
		VAX, Encore BSD, Suns, and SGI.  It may work for others.
	    [default: $(MACHINE) -DBSD4 -DFLOAD]
	PARMAKE	How to drive the child makes run by the top-level Makefile.
		Note that it is not possible to make the top-level in parallel
		itself.
	    [default: make]
	CC	Which C compiler to use.
	    [default: cc]

	To actually create the NU-Prolog system, type:

		make install

	*after* you have fixed the configuration parameters.

nep/Makefile.skel
	This is processed by the top-level Makefile to make nep/Makefile.

co/
	Source for the NU-Prolog compiler.  The compiler itself is
	implemented in NU-Prolog.  NU-Prolog assembler source files are
	provided to make bootstrapping possible.  DON'T remove them.

sys/
	Source for the NU-Prolog built-in predicates and the interactive
	system.  It is implemented in NU-Prolog and forms the basis for
	"np".  As for co/, NU-Prolog assembler source files are provided
	to make bootstrapping possible.  DON'T remove them.

db/
	Sources for the various database indexing schemes which NU-prolog
	supports.

lib/
	Source for major NU-Prolog libraries.

man/
	Source for on-line manual.  This is implemented as a SIMC database.
	It will be created automatically by "make install".  It provides
	a good test for whether the system has come up OK.

na/
	Source for NU-Prolog assembler.  This is the first thing created,
	as much of the system is supplied in the form of NU-Prolog abstract
	machine code and needs only assembly before use.

nep/
	Source for NU-Prolog abstract machine (enhanced Warren engine).
	This is the mechanism which executes assembled NU-Prolog programs.

nit/
	Source for a NU-Prolog program checker.  It checks NU-Prolog programs
	for common types of programming errors (cf. "lint(1)").

revise/
	Source for a NU-Prolog incremental program revision system.  It
	organises recompilation after making small changes to NU-Prolog
	programs.  It is used by the "revise" predicate in "np".
	is used by "np" and "nc".

If the "make" is successful, you should find the four main components of
the NU-Prolog system in the BIN directory.  They are:

np	interactive system (similar to MU-Prolog interpreter)

nc	NU-Prolog compiler (transforms ".nl" files to ".no" and "a.out")

nit	NU-Prolog Incompetence Tester (checks NU-Prolog programs)

na	NU-Prolog assembler (used by "nc")

As well, some other programs will appear in the BIN directory:

initdb	Initialise databases for use with the UNIFY/SQL interface

sqlpp	Preprocessor for queries to the Unify/SQL interface

revise	Incremental program revision system

Further details of how these programs are used may be found in the manual.

Mail bug reports to "mip@cs.mu.oz.au".  Please try to enclose a *small*
piece of code which illustrates the bug.

Good luck with the installation.
