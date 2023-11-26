/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, James Thom, Melbourne University		*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

/*
prede.c
functions to access external database
*/

/* YUK! Wasteful */
#if 0 /* def DBASE */

#include "types.h"
#include "pred.h"
#include "muddpl.h"
#include <stdio.h>

#ifdef FCNTL
#include <fcntl.h>
#else
#include <sys/ioctl.h>
#endif

DBQ dbq[MAXDBQ];

extern wflags;
extern fget();

p_dbquery(t, l)		/* Prolog fn to initiate query from external DB */
	Ptr t;
	levtype l;
{
	Ptr tq, td;
	levtype lq;
	int f;

	findbind((Ptr)targ(1, t), l, &td, &lq);
	findbind((Ptr)targ(2, t), l, &tq, &lq);
	findbind((Ptr)targ(3, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
	if((f = mudd_req(dname(atdict(td)), tq, lq, DB_QYALL)) < 0) {
		return(ERROR);
	}
	pfiles[f + MAXFILES] = mudd_pipe(f);
	pfmode[f + MAXFILES] = 'r';
	nbind(t, l, ConsInt(f+MAXFILES));
	/* *rsetend++ = RDB;		/* call mudd_abort on backtrack */
	/* *rsetend++ = f;
	*/
	return(SUCCEED);
}

p_db_end(t, l)			/* called at end of db query */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	pfiles[tnum(t)] = NULL;
	mudd_end(tnum(t) - MAXFILES);
	return(SUCCEED);
}

p_dbretract(t, l)	/* Prolog fn to retract in external DB */
	Ptr t;
	levtype l;
{
	Ptr td, r;
	levtype l1, b;
	int f;

	findbind((Ptr)targ(1, t), l, &td, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
	if ((f = mudd_req(dname(atdict(td)), t, l, DB_RT)) < 0)
		return(ERROR);
	if(!(r = getterm(fget, (int **) mudd_pipe(f)))) {
		printf(stderr, "Horrible error 1\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	if(!strcmp(dname(tdict(r)), "?-")) {
		printt(stderr, r, gbindings(goalstk)),printf("\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	if(!strcmp(dname(tdict(r)), "db_end"))
		return(CUTFAIL);
	g_rset(gtop) = (Int) (gbindings(gtop) + nvars);
	rsetend = grset(gtop);
	for(b = gbindings(gtop); b < (levtype) rsetend; b++) {
		b->btermp = NULL;
		b->blev = NULL;
	}
	*rsetend++ = RTERM;	/* reclaim term on backtracking */
	*rsetend++ = (Int) r;
	unify(t, l, r, gbindings(gtop));
				/* read db_end (we assume) from pipe */
	if(!getterm(fget, (int **) mudd_pipe(f))) {
		printf(stderr, "Horrible error 1\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	return(SUCCEED);
}

p_dbrall(t, l)	/* Prolog fn to retract all in external DB */
	Ptr t;
	levtype l;
{
	Ptr td, r;
	levtype l1;
	int f;

	findbind((Ptr)targ(1, t), l, &td, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
	if ((f = mudd_req(dname(atdict(td)), t, l, DB_RTALL)) < 0)
		return(ERROR);
	if(!(r = getterm(fget, (int **) mudd_pipe(f)))) {
		printf(stderr, "Horrible error 1\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	if(!strcmp(dname(tdict(r)), "?-")) {
		printt(stderr, r, gbindings(goalstk)),printf("\n");
		dispterm(r);
		plerror(EDBQUERY);
		return(ERROR);
	}
	dispterm(r);
	return(SUCCEED);
}

p_dbassert(t, l)	/* Prolog fn to assert in external DB */
	Ptr t;
	levtype l;
{
	Ptr r, td;
	levtype l1;
	int f;

	findbind((Ptr)targ(1, t), l, &td, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
#define DBSYNC	/* synchronous asserts - the default */
#ifdef DBSYNC
	if ((f = mudd_req(dname(atdict(td)), t, l, DB_ASS)) < 0)
		return(ERROR);

	/* read term from answer pipe (ie. wait until assert has been done) */

	if(!(r = getterm(fget, (int **) mudd_pipe(f)))) {
		printf(stderr, "Horrible error 2\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	if(!strcmp(dname(tdict(r)), "?-")) {
		printt(stderr, r, gbindings(goalstk)),printf("\n");
		dispterm(r);
		plerror(EDBQUERY);
		return(ERROR);
	}
	dispterm(r);
#else
	/* asynchronous asserts (we dont wait for confirmation) */

	if ((f = mudd_req(dname(atdict(td)), t, l, DB_ASA)) < 0)
		return(ERROR);
#endif
	return(SUCCEED);
}

	/* the following was written by James Thom and slightly modified */
	/* to port it to sysV						 */

mudd_req(dbname,t, l,type)
char *dbname;		/* database name */
Ptr t;			/* prolog term */
levtype l;		/* level of term */
char type;		/* the type of request DB_QY, DB_RT, etc */
{
/* returns query number 0 to MAXDBQ-1 */
/* returns -1 if unable to make query */

	register q, qq;
	int pred_int;

	findbind(t, l, &t, &l);
	pred_int = tdict(t);
	if (type == DB_QYALL) {
		/* partial match query - seek all answers */

		/* look for a passive process with the same predicate */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].pred == pred_int && dbq[q].flag == 'P') {
				dbq_write(q, t, l, type);
				dbq[q].flag = 'A';
				return(q);
			}
	
		/* look for an empty slot */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].flag == '\0') {
				if (! dbq_new(dbname, pred_int, q)) {
					return(-1);
				}
				dbq_write(q, t, l, type);
				dbq[q].flag = 'A';
				return(q);
			}
	
		/* look for a non-active process and kill it */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].flag == 'P') {
				mudd_abort(q);
				if (! dbq_new(dbname, pred_int, q)) {
					return(-1);
				}
				dbq_write(q, t, l, type);
				dbq[q].flag = 'A';
				return(q);
			}
	
		/* no slot available */
	
		plerror(EDBNOSLOT);
		return(-1);
	} else {
		/* single query, retract, retractall or assert */

		/* check no active process with the same predicate */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].pred == pred_int && dbq[q].flag == 'A') {
				plerror(EDBACTIVE);
				return(-1);
			}
	
		/* look for a passive process with the same predicate */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].pred == pred_int && dbq[q].flag == 'P') {
				dbq_write(q, t, l, type);

				/* abort any others - so mudd doesnt hang */
				for (qq = q+1; qq < MAXDBQ; qq++) {
					if (dbq[qq].pred == pred_int &&
					    dbq[qq].flag == 'P') {
						mudd_abort(qq);
					}
				}
				return(q);
			}
	
		/* look for an empty slot */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].flag == '\0') {
				if (! dbq_new(dbname, pred_int, q)) {
					return(-1);
				}
				dbq_write(q, t, l, type);
				dbq[q].flag = 'P';
				return(q);
			}
	
		/* look for a non-active process and kill it */
	
		for (q = 0; q < MAXDBQ; q++)
			if (dbq[q].flag == 'P') {
				mudd_abort(q);
				if (! dbq_new(dbname, pred_int, q)) {
					return(-1);
				}
				dbq_write(q, t, l, type);
				dbq[q].flag = 'P';
				return(q);
			}
	
		/* no slot available */
	
		plerror(EDBNOSLOT);
		return(-1);
	}
}

dbq_new(dbname, pred_int, slot)
	char *dbname;
	int pred_int;
	int slot;
{
	int apd[2], qpd[2];

	if (pipe(qpd) < 0) {
		plerror(EDBNOPIPE);
		return(0);
	}
	if (pipe(apd) < 0) {
		close(qpd[0]);
		close(qpd[1]);
		plerror(EDBNOPIPE);
		return(0);
	}
	if (!fork()) {
		close(0);
		if(dup(qpd[0]) != 0)
			_exit(1);
		close(qpd[0]);
		close(qpd[1]);
		close(1);
		if(dup(apd[1]) != 1)
			_exit(1);
		close(apd[0]);
		close(apd[1]);
		execl(PMUDD, "PROLOG-mudd",dbname, dname(pred_int), 0);
		printf("?-db_error(dberr_exec).\n");
		fflush(stdout);
		_exit(1);
	}
	close(qpd[0]);
	close(apd[1]);
	dbq[slot].qpipe = fdopen(qpd[1],"w");
	dbq[slot].apipe = fdopen(apd[0],"r");
		/* pipes should be closed on exec */
		/* actually, we should do this manually, */
		/* after we fork.	*/
#ifdef FCNTL
	fcntl(qpd[1], F_SETFD, 1);
	fcntl(apd[0], F_SETFD, 1);
#else
	ioctl(qpd[1], FIOCLEX, NULL);
	ioctl(qpd[0], FIOCLEX, NULL);
#endif
	dbq[slot].pred = pred_int;
	return(1);
}


dbq_write(slot, t, l, type)		/* write query down pipe */
	int slot;
	Ptr t;
	levtype l;
	char type;
{
	register FILE *fp;
	int n, i, swflags;
	
	fp = dbq[slot].qpipe;
	putc(type, fp);
	swflags = wflags;
	wflags = 15;	/* magic number - print with quotes etc (change) */
	n = tnargs(t);
	fprintf(fp, "%s", dname(tdict(t)));
	putc('\0', fp);
	for(i = 1; i <= n; i++) {
		printt(fp, (Ptr)targ(i, t), l);
		putc('\0', fp);
	}
	putc('\0', fp);
	wflags = swflags;
	fflush(fp);
}

mudd_abort(n)
	int n;
{
	if (dbq[n].flag == 'A') {
		fclose(dbq[n].qpipe);
		fclose(dbq[n].apipe);
		wait(0);
		dbq[n].flag = '\0';
		dbq[n].pred = 0;
	}
}

mudd_clear() /* not used currently ??? */
{
	register i;

	/* abort any active processes */
	for (i = 0; i < MAXDBQ; mudd_abort(i++));
}

p_mudd_abort(t, l)
Ptr t;
levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	pfiles[tnum(t)] = NULL;
	mudd_abort(tnum(t) - MAXFILES);
	return(SUCCEED);
}

#endif
