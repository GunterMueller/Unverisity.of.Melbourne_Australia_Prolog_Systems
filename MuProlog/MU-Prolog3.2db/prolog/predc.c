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
predc.c
functions called by PROLOG involving control of execution
etc
*/

#include "types.h"
#include "pred.h"
#include "dict.h"
char *strcpy();

/* YUK! */
#ifdef DBASE
#define NEWDBASE
#endif

extern Int ndelay
	;

p_debug(t, l)				/* change/return debugging status */
	Ptr t;
	levtype l;
{
	return(sysvar(t, l, &debug));
}

sysvar(t, l, varp)			/* change/return system variable */
	Int * varp;
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	switch(tagtype(t)) {
	case TAGATOM:
	case TAGCOMP:
		plerror(EUFUNC);
		return(ERROR);
	case TAGNUM:
		*varp = tnum(t);
		break;
	case TAGVAR:
		nbind(t, l, ConsInt(*varp));
	}
	return(SUCCEED);
}

p_ndelay(t, l)				/* return number of delayed procs */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	nbind(t, l, ConsInt(ndelay));
	return(SUCCEED);
}

p_errnum(t,l)				/* return number of latest error */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	nbind(t, l, ConsInt(plerrnum));
	return(SUCCEED);
}


p_int(t,l)			/* tests if arg in an int */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	switch(tagtype(t)) {
	case TAGNUM : return(SUCCEED);
	case TAGATOM:
	case TAGCOMP: return(FAIL);
	case TAGVAR : addbind(t, l, (Ptr)ConsInt(0), NULLL);
			return(DELAY);
	}
}

p_integer(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) == TNUM)
		return(SUCCEED);
	else
		return(FAIL);
}

p_var(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) == TVAR)
		return(SUCCEED);
	else
		return(FAIL);
}

p_is_(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) == TVAR && vtdict(t) == D_)
		return(SUCCEED);
	else
		return(FAIL);
}

p_nonvar(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR)
		return(SUCCEED);
	else
		return(FAIL);
}

p_atom(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	return(IsAtom(t));
}

p_atomic(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	return(IsAtomic(t));
}

p_ngrnd(t,l)			/*  waits until ground then fails */
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int a;

restart:			/* tro is needed for very long lists */
	findbind(t, l, &t1, &l1);
	if(ttype(t1) == TVAR) {
		if(gprev(gtop) != gparent(gtop) || resumed(gtop)) {
			plerror(EPROT);
			return(ERROR);
		}
		rsetend = gtop;		/* pop stack */
		gtop = gprev(gtop);
		addbind(t1, l1, (Ptr)ConsInt(0), NULLL);
		return(DELPREV);	/* so mainloop knows a fudge is on */
	} else if(IsAtomic(t1))
		return(FAIL);
	for(a = 1; a < tnargs(t1); a++)
		if(p_ngrnd((Ptr)targ(a, t1), l1) == DELPREV)
			return(DELPREV);
	t = (Ptr) targ(a, t1);
	l = l1;
	goto restart;		/* avoids tail recursion */
}

p_cut(t, l)				/* cut operation - normal version */
	Ptr t;
	levtype l;
{
	Int *c, *p;

	p = gtop;
	do {
		p = gparent(p);
		findbind(gtcall(p), gbindings(gparent(p)), &t, &l);
	} while(IsComp(t) && (ctdict(t) == Dcomma || ctdict(t) == Dsemi
					/*|| ctdict(t) == Dcall non-standard*/
			));
	for(c = gprev(gtop); c >= p; c = gprev(c)) {
		if(!delayed(c) && !beencut(gproc(c)) && cnextc(gproc(c)))
			g_proc(c) = cut(gproc(c));
#ifdef NEWDBASE
		{int *r;
		Ptr t1;
		levtype l1;
		/* for calls to $dbquery, abort the query */
		findbind(gtcall(c), gbindings(gparent(c)), &t1, &l1);
		if(IsComp(t1) && ctdict(t1) == D_dbab) {
			/*r = (int *) grset(c) + 4; /* 4 for resetting the nbind*/
			/*mudd_abort(*(r+1));
			/**r = (int) r;
			*/
				/* New db interface - need to call scheme
				   specific abort function.  This involves
				   lots of indirection via a prolog proc
					eg $db_onabort(rlhp_abort(N))
					   rlhp_abort(N) :- C function
				*/
			Ptr t2;
			levtype l2;

				/* t2,l2 = rlhp_abort(N) */
			findbind(targ(1, t1), l1, &t2, &l2);
			if(!IsFunc(t2))
				fprintf("db_abort predicate not there!\n");
			else
				(cfunc(d1clause(ctdict(t2))))(t2, l2);
		}}
#endif
	}
	return(SUCCEED);
}

p_lcut(t, l)		/* local cut for -> ; which is blocked by ; */
	Ptr t;
	levtype l;
{
	Int *c, *p;

	p = gtop;
	do {
		p = gparent(p);
		findbind(gtcall(p), gbindings(gparent(p)), &t, &l);
	} while(IsComp(t) && (ctdict(t) == Dcomma /*|| ctdict(t) == Dsemi NB*/
					/*|| ctdict(t) == Dcall non-standard*/
			));
	for(c = gprev(gtop); c >= p; c = gprev(c)) {
		if(!delayed(c) && !beencut(gproc(c)) && cnextc(gproc(c)))
			g_proc(c) = cut(gproc(c));
#ifdef NEWDBASE
		{int *r;
		Ptr t1;
		levtype l1;
		/* for calls to $dbquery, abort the query */
		findbind(gtcall(c), gbindings(gparent(c)), &t1, &l1);
		if(IsComp(t1) && ctdict(t1) == D_dbab) {
			/*r = (int *) grset(c) + 4; /* 4 for resetting the nbind*/
			/*mudd_abort(*(r+1));
			/**r = (int) r;
			*/
				/* New db interface - need to call scheme
				   specific abort function.  This involves
				   lots of indirection via a prolog proc
					eg $db_onabort(rlhp_abort(N))
					   rlhp_abort(N) :- C function
				*/
			Ptr t2;
			levtype l2;

				/* t2,l2 = rlhp_abort(N) */
			findbind(targ(1, t1), l1, &t2, &l2);
			if(!IsFunc(t2))
				fprintf("db_abort predicate not there!\n");
			else
				(cfunc(d1clause(ctdict(t2))))(t2, l2);
		}}
#endif
	}
	return(SUCCEED);
}

p_soft_cut(t, l)			/* soft cut for NU-Negation */
	Ptr t;
	levtype l;
{
	Int *c, *p;

	p = gtop;
	do {
		p = gparent(p);
		findbind(gtcall(p), gbindings(gparent(p)), &t, &l);
	} while(IsComp(t) && (ctdict(t) == Dcomma || ctdict(t) == Dsemi
					/*|| ctdict(t) == Dcall non-standard*/
			));
	/* for(c = gprev(gtop); c >= p; c = gprev(c)) {*/
	c = p;	/* just clobber first choice point */
		if(!delayed(c) && !beencut(gproc(c)) && cnextc(gproc(c)))
			g_proc(c) = cut(gproc(c));
#ifdef NEWDBASE
		{int *r;
		Ptr t1;
		levtype l1;
		/* for calls to $dbquery, abort the query */
		findbind(gtcall(c), gbindings(gparent(c)), &t1, &l1);
		if(IsComp(t1) && ctdict(t1) == D_dbab) {
			/*r = (int *) grset(c) + 4; /* 4 for resetting the nbind*/
			/*mudd_abort(*(r+1));
			/**r = (int) r;
			*/
				/* New db interface - need to call scheme
				   specific abort function.  This involves
				   lots of indirection via a prolog proc
					eg $db_onabort(rlhp_abort(N))
					   rlhp_abort(N) :- C function
				*/
			Ptr t2;
			levtype l2;

				/* t2,l2 = rlhp_abort(N) */
			findbind(targ(1, t1), l1, &t2, &l2);
			if(!IsFunc(t2))
				fprintf("db_abort predicate not there!\n");
			else
				(cfunc(d1clause(ctdict(t2))))(t2, l2);
		}}
#endif
	/* } */
	return(SUCCEED);
}

		/* does reset(r) refer to a binding of _ at level l? */
#define boundto_(r, l)	(IsVar(*(Ptr*)*r) && vtdict(*(Ptr*) *r) == D_\
				&& *(levtype *) *(r+2) == l)

p_teq(t,l)				/* equality test (no construct) */
	Ptr t;
	levtype l;
{
	Ptr t1,t2;
	levtype l1,l2;
	Int *r,*r1;

	r = rsetend;
	findbind((Ptr)targ(1, t), l, &t2, &l2);
	findbind((Ptr)targ(2, t), l, &t1, &l1);
	if(!unify(t2, l2, t1, l1))
		return(FAIL);
	else {
		for(r1 = r; r1 < rsetend; r1 = r1 + 4)
			if(!boundto_(r1, l) && !bound_(r1, l, t))
				return(DELAY);
		return(SUCCEED);
	}
}

static
bound_(r, l, cs)	/* does reset(r) refer to a binding of _ in the term */
			/* cs at level l ? */
	Int *r;
	levtype l;
	Ptr cs;
{
	int n, i;

	start:
	if((levtype) *r < l)
		return(0);
	if(ttype(cs) == TVAR)
		if(vtdict(cs) == D_ && l + tvnum(cs) == (levtype) *r)
			return(1);
		else
			return(0);
	if(IsAtomic(cs))
		return(0);
	n = tnargs(cs);
	for(i = 1; i < n; i++)
		if(bound_(r, l, (Ptr)targ(i, cs)))
			return(1);
	cs = (Ptr) targ(n, cs);
	goto start;
}

p_tne(t,l)				/* inequality test */
	Ptr t;
	levtype l;
{
	switch(p_teq(t,l)) {
	case DELAY:	return(DELAY);
	case FAIL :
			reset(grset(gtop));
			rsetend = grset(gtop);	/* seems to be needed */
			return(SUCCEED);
	case SUCCEED :	return(FAIL);
	}
}

		/* was reset entry r bound to a variable in t/l */
#define boundtolocal(r,t,l) (occurs((Ptr)*(Ptr)*r, (levtype)*(Ptr)*(r+2), t, l)==SUCCEED)

p_is_eq(t,l)			/* equality test (NU-Negation) */
	Ptr t;			/* $is_eq(Locals, T1, T2, R) */
	levtype l;
{
	Ptr t1,t2,t3;
	levtype l1,l2,l3;
	Int *r,*r1;

	r = rsetend;
	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t2, &l2);
	findbind((Ptr)targ(3, t), l, &t3, &l3);
	findbind((Ptr)targ(4, t), l, &t, &l);
	if(!unify(t2, l2, t3, l3)) {
		reset(grset(gtop));
		rsetend = grset(gtop);	/* needed? */
		if(unify(t, l, ConsFunc(find(1, "fail"),0), gbindings(goalstk)))
			return(SUCCEED);
		else
			return(FAIL);
	} else {
		for(r1 = r; r1 < rsetend; r1 = r1 + 4)
			if(!boundlocal((levtype)*r1, t1, l1)
					&& !boundtolocal(r1, t1, l1))
				return(DELAY);
		reset(grset(gtop));
		rsetend = grset(gtop);	/* needed? */
		if(unify(t, l, ConsFunc(find(1, "true"),0), gbindings(goalstk)))
			return(SUCCEED);
		else
			return(FAIL);
	}
}

	/* does address r correspond to a var in t/tl? */
boundlocal(r, t, tl)
	Ptr  t;
	levtype r, tl;
{
	Ptr t1;
	levtype l1;
	int i;

	if(IsComp(t))
		for (i = tnargs(t); i > 0; i--) {
			t1 = (Ptr)targ(i, t);
			if(IsVar(t1) && (tl + tvnum(t1)) == (levtype) r)
				return(1);
			findbind(t1, tl, &t1, &l1);
			if(boundlocal(r, t1, l1))
				return(1);
		}
	else if(IsVar(t) && (tl + tvnum(t)) == (levtype) r)
		return(1);
	return(0);
}

		/* should really return (list of?) goals so */
		/* prolog can get at them */
p_pasleep(t, l)				/* print sleeping goals */
	Ptr t;
	levtype l;
{
	Int *c, *p;

	plerrnum = 0;
	p = gtop;
	do {
		p = gparent(p);
		if(!gparent(p))
			return(SUCCEED); /* interrupted in Prolog main loop */
		findbind(gtcall(p), gbindings(gparent(p)), &t, &l);
	} while(!IsComp(t) || ctdict(t) != D_cmd);
	for(c = gprev(gtop); c >= p; c = gprev(c)) {
		if(asleep(c)) {
			printt(stdout, gtcall(c), gbindings(gparent(c)));
			printf(",\n");
		}
	}
	return(SUCCEED);
}

p_abort(t, l)				/* abort operation */
	Ptr t;
	levtype l;
{
	Int *c, *p;

	plerrnum = 0;
	p = gtop;
	do {
		p = gparent(p);
		if(!gparent(p))
			return(SUCCEED); /* interrupted in Prolog main loop */
		findbind(gtcall(p), gbindings(gparent(p)), &t, &l);
	} while(!IsComp(t) || ctdict(t) != D_cmd);
	for(c = gprev(gtop); c >= p; c = gprev(c)) {
		if(!delayed(c) && !beencut(gproc(c)) && cnextc(gproc(c)))
			g_proc(c) = cut(gproc(c));
#ifdef NEWDBASE
		{int *r;
		Ptr t1;
		levtype l1;
		/* for calls to $dbquery, abort the query */
		findbind(gtcall(c), gbindings(gparent(c)), &t1, &l1);
		if(IsComp(t1) && ctdict(t1) == D_dbab) {
			/*r = (int *) grset(c) + 4; /* 4 for resetting the nbind*/
			/*mudd_abort(*(r+1));
			/**r = (int) r;
			*/
				/* New db interface - need to call scheme
				   specific abort function.  This involves
				   lots of indirection via a prolog proc
					eg $db_onabort(rlhp_abort(N))
					   rlhp_abort(N) :- C function
				*/
			Ptr t2;
			levtype l2;

				/* t2,l2 = rlhp_abort(N) */
			findbind(targ(1, t1), l1, &t2, &l2);
			if(!IsFunc(t2))
				fprintf("db_abort predicate not there!\n");
			else
				(cfunc(d1clause(ctdict(t2))))(t2, l2);
		}}
#endif
	}
	printf("\nExecution aborted\n");
	return(FAIL);
}

extern savedata();
extern onintr();	/* one of the last function loaded */
			/* => its address changes if any changes are made */

p_save(t,l)                     /* save prolog state */
	Ptr t;
	levtype l;
{
	int fd;
	Int *bufp;
	FILE *f;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsAtom(t)) {
		plerror(EFILE);
		return(ERROR);
	}
	if(f = fopen(dname(atdict(t)), "r")) {	/* if file exists */	
		char c1, c2;			/* check that it looks like a */
						/* save file - not a program */
		fscanf(f, "%c%c", &c1, &c2);	/* to destroy by accident */
		fclose(f);
		if(c1 != '#' || c2 != '!') {
			fprintf(stderr, "%s is not a Prolog save file\n",
					dname(tdict(t)));
			return(FAIL);
		}
	}
	if((fd = creat(dname(atdict(t)), 0755)) < 0) {
		plerror(EFILE);
		return(ERROR);
	}
	strcpy(savebuf.header, SAVEHDR);
	bufp = &savebuf.l[S_CONSTS];
	*bufp++ = (Int) VERSION;
	*bufp++ = (Int) GSTKLEN;
	*bufp++ = (Int) DICTLEN;
	*bufp = (Int) onintr;
		/* vars in main and dload have already been put in buffer */
	savedata(fd);
#ifdef DLOAD
	savedload(fd);
#endif
	close(fd);
	return(SUCCEED);
}

p_restore(t,l)
	Ptr t;
	levtype l;
{
	int fd;
	Int *bufp;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsAtom(t) || (fd = open(dname(atdict(t)), 0)) < 0)
		return(FAIL);
	read(fd, (char *)&savebuf, sizeof(savebuf));
	bufp = &savebuf.l[S_CONSTS];
	if(strcmp(savebuf.header, SAVEHDR) != 0
		|| *bufp++ != (Int) VERSION
		|| *bufp++ != (Int) GSTKLEN
		|| *bufp++ != (Int) DICTLEN
		|| *bufp != (Int) onintr
		|| !restdata(fd)
#ifdef DLOAD
		|| !restdload(fd)
#endif
		|| read(fd, (char *)&t, 1) != 0) {
		fprintf(stderr, "saved state not compatible\n");
		return(FAIL);
	}
	close(fd);
	return(RESTORE);
}
