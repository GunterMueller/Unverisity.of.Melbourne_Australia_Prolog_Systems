/************************************************************************
*									*
*			MU-PROLOG DATABASE				*
*			========= ========				*
*									*
* (C) Copyright 1983 Lee Naish, James Thom, Melbourne University	*
*									*
*	Written by Lee Naish & James Thom				*
*	Department of Computer Science,					*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	Extensively hacked by jas & rao, May 1985			*
*									*
************************************************************************/

/*
simc.c
functions to access external database using superimposed coding
*/

#include "types.h"
#include "pred.h"
#include "muddpl.h"
#include <stdio.h>

#ifdef FCNTL
#include <fcntl.h>
#else
#include <sys/ioctl.h>
#endif

extern wflags;
extern fget();
extern char *sdisplay();

p_simc_query(t, l)	/* Prolog fn to initiate query from external DB */
	Ptr t;
	levtype l;
{
	Ptr tq, td;
	levtype lq;
	int f; /* really a Trans pointer */
	int swflags;
	char *s;
	char query[256];

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

	findbind(tq, lq, &tq, &lq);
	
	swflags = wflags;
	wflags = 15;	/* magic number - print with quotes etc (change) */
	s = sdisplay(tq, lq, query);
	*s = '\0';
	wflags = swflags;

#if 0
fprintf(stderr,"trans_open(%s,%s,QUERY,0)\n",dname(atdict(td)),query);
#endif
	if((f = trans_open(dname(atdict(td)), query, 04/*opQUERY*/, 0)) == 0)
		return(ERROR);

	nbind(t, l, ConsInt(f));
	return(SUCCEED);
}

#if 0
/*
 * NO SUPERJOIN FOR MU-PROLOG ... yet
 */
#ifdef SFB
p_simc_sfbquery(t, l)	/* Prolog fn to initiate query from external DB */
	Ptr t;
	levtype l;
{
	Ptr tq, td, tv;
	levtype lq, lv;
	int f; /* really a Trans pointer */
	int swflags;
	char *s, query[256];
	char *v, vector[64];

	findbind((Ptr)targ(1, t), l, &td, &lq);
	findbind((Ptr)targ(2, t), l, &tq, &lq);
	findbind((Ptr)targ(3, t), l, &tv, &lv);
	findbind((Ptr)targ(4, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}

	findbind(tq, lq, &tq, &lq);
	findbind(tv, lv, &tv, &lv);
	
	swflags = wflags;
	wflags = 15;	/* magic number - print with quotes etc (change) */
	s = sdisplay(tq, lq, query);
	*s = '\0';
	v = sdisplay(tv, lv, vector);
	*v = '\0';
	wflags = swflags;

#if 1
fprintf(stderr,"trans_open(%s,%s,04,%s)\n",dname(atdict(td)),query,vector);
#endif
	/*
	 * ... and even if there was a SUPERJOIN,
	 * this is not the correct interface ...
	 */
	if((f = trans_sfbopen(dname(atdict(td)), query, 04/*QUERY*/, vector)) == 0) {
		return(ERROR);
	}
	nbind(t, l, ConsInt(f));
	return(SUCCEED);
}
#endif SFB
#endif


p_simc_end(t, l)			/* called at end of db query */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	trans_close(tnum(t));
	return(SUCCEED);
}

p_simc_delete(t, l)			/* delete the "current" record */
	Ptr t;				/* used by the retract preidcates */
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	rec_delete(tnum(t));
	return(SUCCEED);
}

p_simc_next(t, l)
	Ptr t;
	levtype l;
{
	Ptr ttr, rt, newt, tans;
	levtype ltr, lans, b;
	char *answer;
	char *trans_fetch();

	findbind((Ptr)targ(1, t), l, &ttr, &ltr);
	if(ttype(ttr) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}

	findbind((Ptr)targ(2, t), l, &tans, &lans);
	if(ttype(tans) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}

	if ((answer = trans_fetch(tnum(ttr))) == NULL)
		answer = "?-db_end.";
	else
		strcat(answer, ".");
#if 0
fprintf(stderr,"answer:%s\n",answer);
#endif

	if(!(rt = stot(answer)))
		return(FAIL);
	newt = rt;
	g_rset(gtop) = (Int) (gbindings(gtop) + nvars);
	rsetend = grset(gtop);
	for(b = gbindings(gtop); b < (levtype) rsetend; b++) {
		b->btermp = NULL;
		b->blev = NULL;
	}
	*rsetend++ = RTERM;	/* reclaim term on backtracking */
	*rsetend++ = (Int) newt;
	addbind(tans, lans, newt, gbindings(gtop));
	return(SUCCEED);
}

#if 0
/*
 * UNFINISHED/UNNECESSARY junk
 */
p_simc_retract(t, l)	/* Prolog fn to retract in external DB */
	Ptr t;
	levtype l;
{
	Ptr td, r;
	levtype l1, b;
	int f;

#if 1
	plerror(EDBQUERY);
	return(ERROR);
#else
	findbind((Ptr)targ(1, t), l, &td, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
	if ((f = trans_open(dname(atdict(td)), t, l, (F_RETRACT|F_ONE))) == 0)
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
#endif
}

p_simc_rall(t, l)	/* Prolog fn to retract all in external DB */
	Ptr t;
	levtype l;
{
	Ptr td, r;
	levtype l1;
	int f;

#if 1
	plerror(EDBQUERY);
	return(ERROR);
#else
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
#endif
}
/*
 * END OF junk
 */
#endif

p_simc_assert(t, l)	/* Prolog fn to assert in external DB */
	Ptr t;
	levtype l;
{
	Ptr r, td;
	levtype l1;
	int f;
	int swflags;
	char *s;
	char query[256];

	findbind((Ptr)targ(1, t), l, &td, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}

	swflags = wflags;
	wflags = 15;	/* magic number - print with quotes etc (change) */
	s = sdisplay(t, l, query);
	*s = '\0';
	wflags = swflags;

	if(trans_assert(dname(atdict(td)), query, 02/*opASSERT*/) != 1) {
		plerror(EDBQUERY);
		return(ERROR);
	}
#ifdef TODO
	/* HANDLE ERROR MESSAGES PROPERLY */

	if(!(r = getterm(fget, (int **) mudd_pipe(f)))) {
		return(ERROR);
	}
	if(!strcmp(dname(tdict(r)), "?-")) {
		printt(stderr, r, gbindings(goalstk)),printf("\n");
		dispterm(r);
		plerror(EDBQUERY);
		return(ERROR);
	}
	dispterm(r);
#endif
	return(SUCCEED);
}

p_simc_abort(t, l)			/* called at end of db query */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	trans_close(tnum(t));
	return(SUCCEED);
}
