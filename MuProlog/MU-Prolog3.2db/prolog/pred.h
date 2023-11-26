/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1983 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

/*
pred.h
globals for pred*.c, init.c, dload.c and main.c
*/
#include <stdio.h>

extern Int *goalstk, *gtop, *hbot, *rsetend,
	*nextgoal, debug, plerrnum, gargc, *cont, wflags
	;
#define MAXARGC 32
extern char * gargv[MAXARGC];
extern Ptr null;

#define MAXFILES 20
extern FILE *pfiles[];
extern char pfmode[];

extern int nvars;
struct avar {
	int olddict, newdict, newnum;
	levtype oldlev;
	} ;
typedef struct avar *avarp;

extern avarp vend;

/*
	ERROR numbers
*/
#define EMISC	1
#define EELIST	10
#define EECONST	11
#define EEINT	12
#define EEFUNC	13
#define EESTR	14
#define EE10	15
#define EEVAR	16
#define EErwa	17

#define EUINT	50
#define EUFUNC	51
#define EUVAR	52
#define EPROT	100
#define EOPEN	110
#define EFILE	115
#define ERESTORE	120
#define EDBQUERY	200
#define	EDBNOSLOT	201
#define	EDBNOPIPE	202
#define	EDBACTIVE	203
#define ENOPROC	310
#define EINTR	320
#define ENOMEM	330
#define EFPE	340
#define ENOVARS	350
#define EABORT	360
#define EDLOAD	400
