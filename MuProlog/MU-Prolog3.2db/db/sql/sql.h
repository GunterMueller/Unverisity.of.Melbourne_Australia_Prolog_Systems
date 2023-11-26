/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melborne University			*
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
	int db_int;
	char flag;	/* '\0'=no process, 'P'=passive, 'A'=active 
					'S'= start new process */
	FILE *qpipe, *apipe;
} DBQ;

#define mudd_pipe(n)	(dbq[n].apipe)		/* answer pipe for mudd */
#define mudd_end(n)	dbq[n].flag='P'

#define DB_QYALL	'Q'	/* partial match query - find all answers */
#define DB_MOD		'm'	/* assert */
