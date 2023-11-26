/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/
/*
mlocal.c - create local definition for local.h
Checks word size etc. and prints a few #defines
*/

#include <stdio.h>
char *strcat(), *strcpy();

#define TAG (unsigned)0xC0000000	/* top 2 bits of 32 bits */

#define error(X) err=1, fprintf(stderr, "ERROR: %s\n",  X)

char buf[100];			/* string buffer */
int err=0;			/* error flag */

main(){
	int count;
	unsigned i;
	unsigned long l;

	/* Type Int must be an integer the same size as a pointer */
	/* types.h currently assumes it is 32 bits (could be changed) */

	if(sizeof(int) == sizeof(int*)) {
		i = 1;				/* count bits in int */
		for(count=1; i <<= 1; count++)
			;
		if(count != 32)
			error("Int not 32 bits: change types.h");
		printf("#define Int int\n");
		printf("#define Int_is_int\n");
	} else if(sizeof(long) == sizeof(long*)) {
		l = 1;				/* count bits in long */
		for(count=1; l <<= 1; count++)
			;
		if(count != 32)
			error("Int not 32 bits: change types.h");
		printf("#define Int long\n");
	} else {
		error("No integer the size of a pointer?");
		error("What is this - a Prime or something?");
	}

	/* The top two bits in Ints are used as a tag, which is assumed to */
	/* be zero for addresses */

	if((int)&err & TAG)
		error("Top bits in address set: change types.h");

	/* Currently, some macros assume a short is two bytes (will fix). */

	if(sizeof(short) != 2)
		error("sizeof(short) != 2 : change types.h?");

	/* startup clause for prolog - for initial boot */

	printf("#define PLBOOTC \"$startup :- $pcons('boot.pl'),\
 $start('%s/plstate'). \"\n", LIB);

	/* PRFILE - a string containing the system save file */
	/* used for restore in startup - except for initial boot */

	printf("#define PRFILE \"%s/plstate\"\n", LIB);

	/* libdirectory predicate */

	printf("#define PLLIBDIR \"libdirectory('%s/pllib'):-true. \"\n", LIB);

	/* Header for save files - 31 chars + NULL, see next line for example */
	/* #define SAVEHDR "#!/usr/bin/prolog             \n"		      */

	strcpy(buf, BIN);
	strcat(buf, "                            ");
	printf("#define SAVEHDR \"#!%.28s\\n\"\n", buf);

#ifdef DBASE
	/* external database programs */

	printf("#define PMUDD \"%s/db/rlhp/mudd\"\n", LIB);
	printf("#define PLNEWREL \"$dbnewrel('%s/db/rlhp/newrel'):-true. \"\n", LIB);
#endif
#ifdef NEWDBASE
	printf("#define PMUDD \"%s/db/rlhp/mudd\"\n", LIB);
#endif

	/* We now have to find out if fcntl is defined. */
	/* If is it, we use it.  Otherwise, we use dup2 and ioctl */
	/* (assuming they exist).   There is no nice way to test this. */

	if(access("/usr/include/fcntl.h", 4) == 0)
		printf("#define FCNTL 1\n");
	else if(access("/usr/include/sys/ioctl.h") != 0)
		error("Neither fcntl or ioctl defined? - change prede.c\n");
/*	Shouldnt be necessary any more - see dload.c
	if(access("/.attbin", 0) == 0)
		printf("#define pyramid\n");
*/
	exit(err);
}
