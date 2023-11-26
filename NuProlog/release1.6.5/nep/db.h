/*
 * db.h - definitions for database indexing schemes
 *
 * Copyright (C) 1986, The University of Melbourne
 *
 * Author: John Shepherd
 */

/*
 * RLH, SQL i/o structures
 */

#define MAXDBQ	8	/* maximum number of db processes */

typedef struct {
	Object dbref;
	char flag;	/* '\0'=no process, 'P'=passive, 'A'=active 
					'S'= start new process */
	FILE *qpipe;
	FILE *apipe;
} DBQ;

#define mudd_pipe(n)	(dbq[n].apipe)		/* answer pipe for mudd */
#define mudd_end(n)	dbq[n].flag='P'

/*
 * RLH, SQL transaction types
 */

#define DB_QY		'q'	/* partial match query - only one answer */
#define DB_QYALL	'Q'	/* partial match query - find all answers */
#define DB_MOD		'm'	/* assert */
#define DB_RT		'r'	/* retract */
#define	DB_RTALL	'R'	/* retractall */
#define DB_ASS		'a'	/* synchronous assert */
#define DB_ASA		'A'	/* asynchronous assert */

/*
 * SIMC transaction types
 */

#define opRETRACT	001	/* delete fact(s) from database/relation */
#define opASSERT	002	/* insert fact into database/relation */
#define opQUERY		004	/* retrieve fact(s) from database/relation */

/*
 * Return values for database predicates
 */

#define	FAIL		0
#define SUCCEED		1

/*
 * Macros for converting things from internal NU-Prolog form to usable form
 */

#define	AtomToString(x)	(eCharStar(((Atom *) eRef(x))->a_pname))
#define	StringToChars(x)	((char *)(eRef(x)))

#ifdef PSTOT
int p_pstot();
#endif /* PSTOT */
int p_simc_query(), p_simc_next(), p_simc_assert();
int p_simc_delete(), p_simc_end(), p_simc_abort();
