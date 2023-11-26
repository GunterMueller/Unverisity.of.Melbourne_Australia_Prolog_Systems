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
#define MAXDBQ	8	/* maximum number of db processes */

typedef struct {
	int pred;
	char flag;	/* '\0'=no process, 'P'=passive, 'A'=active */
	FILE *qpipe;
	FILE *apipe;
} DBQ;

#define mudd_pipe(n)	(dbq[n].apipe)		/* answer pipe for mudd */
#define mudd_end(n)	dbq[n].flag='P'

#define DB_QY		'q'	/* partial match query - only one answer */
#define DB_QYALL	'Q'	/* partial match query - find all answers */
#define DB_RT		'r'	/* retract */
#define	DB_RTALL	'R'	/* retractall */
#define DB_ASS		'a'	/* synchronous assert */
#define DB_ASA		'A'	/* asynchronous assert */
