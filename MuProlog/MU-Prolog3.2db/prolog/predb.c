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
predb.c
functions called by PROLOG involved in the (internal) database
*/
#include "types.h"
#include "dict.h"
#include "pred.h"
char *strcpy();

int nvars;		/* number of vars in clause being asserted */
avarp vend;

p_nassert(t, l)			/* assert a clause */
	Ptr t;
	levtype l;
{
	Ptr	newt, t1;
	register Ptr	c;
	levtype	l1;
	register int	d, cn;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(!IsComp(t1) || ctdict(t1) != Darrow) {
		plerror(EUFUNC);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(IsFunc(t)) {
		plerror(EUFUNC);
		return(ERROR);
	}
	nvars = 0;		/* get a copy of the arg to nassert */
	vend = (avarp) rsetend;
	newt = tcopy(t1, l1);
	{int d;		/* to avoid expression ov. on 3240 */
	d = tdict((Ptr)targ(1, newt));
	if(!IsFunc((Ptr)targ(1, newt)) || dprotect(d)) {
		plerror(EPROT);
		dispterm(newt);
		return(ERROR);
	}
	}
	if(nvars > MAXVARS) {
		plerror(ENOVARS);
		return(ERROR);
	}
	d = tdict((Ptr)targ(1, newt));
	if(d1clause(d) == null)
		d_1clause(d) = NULL;
	if(ttype(t) == TNUM) {
		cn = tnum(t);
		for(c =  d0clause(d); --cn > 0 && cnextc(c); c = cnextc(c))
			;
		if(cn) {
			dispterm(newt);
			return(FAIL);	/* no clause cn */
		}
	} else {
		cn = 1;
		for(c = d0clause(d); cnextc(c); c = cnextc(c))
			cn++;
		nbind(t, l, ConsInt(cn));
	}
	t = cnextc(c);
	c_nextc(c) = (Int) newmem(CCLSZ);
	c = cnextc(c);
	c_nextc(c) = (Int) t;
	c_nlinks(c) = 1;
	c_nvars(c) = nvars;
	c_clause(c) = (Int) newt;
	return(SUCCEED);
}

Int
vcopy(t, l)		/* handles copying variables */
	Ptr	t;
	register levtype l;
{
	register int	i, sameid, oldd, newd, newn;
	register avarp	v;
	char	newname[IDLEN + 2], c;

	sameid = 0;
	oldd =
	vend->olddict = vtdict(t);
	vend->oldlev =
	l = l + tvnum(t);
	v = (avarp) rsetend;
	while(v->oldlev != l || v->olddict != oldd) {
		if(v->newdict == oldd)
			sameid++;
		v++;
	}
	if(v != vend) {
		newd = v->newdict;
		newn = v->newnum;
	} else {
		if(!sameid || oldd == D_) {
			newd =
			vend->newdict = oldd;
		} else {
			strcpy(newname + 2, dname(oldd));
			/*c = newname[2];*/
			/*newname[0] = '_';*/
			i = 0;
			do {
				/*
				sprintf(newname + 1, "%d", i);
				if(i < 10)
					newname[2] = c;
				*/
				newname[0] = 'A' + i%26;
				if (i/26 > 0)
					sprintf(newname + 1, "%d", i/26);
				else
					newname[1] = '\0';
				newd =
				vend->newdict = find(0, newname);
				v = (avarp) rsetend;
				while(v->newdict != newd)
					v++;
				i++;
			} while(v != vend);
		}
		newn =
		vend->newnum =
		nvars++;
		vend++;
	}
	return(ConsVar(newd, newn));
}

p_wait(t, l)					/* add a mode declaration */
	Ptr	t;	/* do not make t a register */
	levtype	l;
{
	Ptr	p,mt;
	levtype	l1;
	register int	d,m,n;

	findbind((Ptr)targ(1, t), l,  &p, &l1);
	if(!IsFunc(p)) {
		plerror(EEFUNC);
		return(ERROR);
	}
	if(IsAtom(p))
		return(SUCCEED);
	m = 0;
	for(n = 1; n <= tnargs(p); n++) {
		findbind((Ptr)targ(n, p), l1, &mt, &l);
		if ( (ttype(mt) != TNUM) || (tnum(mt) & ~1) != 0) {
			plerror(EE10);
			return(ERROR);
		}
		m = (m << 1) + tnum(mt);
	}
	d = ctdict(p);
	if(d1clause(d) == null)
		d_1clause(d) = NULL;
	for(t = &d_wait(d); *t; t = (Ptr) *t)
		;
	*t = (Int) newmem(2);
	t = (Ptr) *t;
	*t = NULL;
	*(t+1) = ~m;
	return(SUCCEED);
}

p_nretract(t, l)			/* retract Nth clause */
	Ptr	t;
	levtype	l;
{
	Ptr	c;
	levtype	cl;
	register int	d, n;

	findbind((Ptr)targ(1, t), l, &c, &cl);
	if(!IsComp(c) || ctdict(c) != Darrow) {
		plerror(EUFUNC);
		return(ERROR);
	}
	findbind((Ptr)targ(1, c), cl, &c, &cl);
	if(!IsFunc(c) || dprotect(tdict(c))) {
		plerror(EPROT);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	d = tdict(c);
	if(d1clause(d) == (Ptr) null)
		return(FAIL);
	c = d0clause(d);
	for(n = tnum(t); n > 1 && c; n--)
		c = cnextc(c);
	if(n != 1 || !cnextc(c))
		return(FAIL);
	t = cnextc(c);
	c_nextc(c) = c_nextc(cnextc(c));
	if(!--c_nlinks((Ptr) t)) {
		dispterm(cclause((Ptr) t));
		dispmem(CCLSZ, t);
	} else
		c_nextc((Ptr) t) = NULL;
	return(SUCCEED);
}

static Ptr
ccopy(c)		/* get a copy of clause c & grab memory for vars etc */
	Ptr	c;
{
	Ptr	newt;
	register levtype b;

	rsetend = rsetend + 2;
	nvars = 1;
	vend = (avarp) rsetend;
	newt = tcopy(cclause((Ptr) c), gbindings(goalstk));
	g_rset(gtop) = (Int) (gbindings(gtop) + nvars);
	rsetend = grset(gtop);
	for(b = gbindings(gtop) + 1; b < (levtype) rsetend; b++) {
		b->btermp = NULL;
		b->blev = NULL;
	}
	*rsetend++ = RTERM;		/* reclaim space on backtrack */
	*rsetend++ = (Int) newt;
	return(newt);
}

p_1nclause(t, l)		/* get a copy of a term from the database */
	register Ptr	t;
	register levtype	l;
{
	Ptr c;
	Ptr	tp, tc, tn;
	levtype	lp, lc, ln;
	register int	n, d;

	findbind((Ptr)targ(1, t), l, &tp, &lp);
	if(!IsComp(tp) || ctdict(tp) != Darrow) {
		plerror(EUFUNC);
		return(ERROR);
	}
	findbind((Ptr)targ(1, tp), lp, &tp, &lp);
	if(!IsFunc(tp)) {
		plerror(EUFUNC);
		return(ERROR);
	}
	if(dprotect(tdict(tp))) {
		plerror(EPROT);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &tc, &lc);
	if(ttype(tc) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	findbind((Ptr)targ(3, t), l, &tn, &ln);
	if(IsFunc(tn)) {
		plerror(EUFUNC);
		return(ERROR);
	} else if(ttype(tn) == TNUM)
		n = tnum(tn);
	else
		n = 1;
	d = tdict(tp);
	if(d1clause(d) == (Ptr) null)
		return(CUTFAIL);
	for(c = d0clause(d); n-- && c; c = cnextc(c))
		;
	if(!c || cnvars(c) == -1)
		return(CUTFAIL);
	addbind(tc, lc, ccopy(c), gbindings(gtop));
	if(ttype(tn) == TVAR) {
		gbindings(gtop)->blev = (levtype) ConsInt(1);
		addbind(tn, ln, (Ptr)gbindings(gtop)->blev, NULLL);
	} else
		gbindings(gtop)->blev = (levtype) ConsInt(-2);
	return(SUCCEED);
}

p_2nclause(t, l)			/* get next clause */
	register Ptr	t;
	register levtype l;
{
	Ptr	tc, tn;
	levtype	lc, ln;
	register Ptr	c;
	register int	n;

	n = tnum(gbindings(gtop)->blev) + 1;
	gbindings(gtop)->blev = (levtype) ConsInt(n);
	findbind((Ptr)targ(1, t), l, &tc, &lc);
	findbind((Ptr)targ(1, tc), lc, &tc, &lc);
	for(c = d0clause(tdict(tc)); c && n > 0; c = cnextc(c))
		n--;
	if(!c || n)
		return(CUTFAIL);
	else {
		findbind((Ptr)targ(2, t), l, &tc, &lc);
		findbind((Ptr)targ(3, t), l, &tn, &ln);
		addbind(tc, lc, ccopy(c), gbindings(gtop));
		addbind(tn, ln, (Ptr)gbindings(gtop)->blev, NULLL);
		return(SUCCEED);
	}
}

static
retract(prev, tc, lc)			/* retract matching clause */
	Ptr prev;
	Ptr tc;
	levtype lc;
{
	Ptr t, c, prev1;
	int first, first1;

	prev1 = prev;
	first1 =
	first = (int) gbindings(gtop)->btermp;
	c = cnextc(prev);
	for( ; c; c = cnextc(c)) {
/*printf("nlinks=%d\n", cnlinks(c));*/
		t = ccopy(c);
/*printf("done copy\n");*/
		if(unify(tc, lc, t, gbindings(gtop)))
			break;
/*printf("not unified\n");*/
		reset(grset(gtop));
		rsetend = (Int*) gbindings(gtop);
		g_rset(gtop) = (Int) rsetend;
		prev = &c_nextc(c);
		first = 0;
	}
	if(!c)
/*{printf("failing\n");*/
		return(CUTFAIL);
/*}*/
	*prev = (Int) cnextc(c);
/*printf("*prev=%x\n", *prev);*/
	if(!first)
		c_nlinks(prev)++;
	if(!first1) {		/* remove clause prev pointed to */
/*printf("nlinks=%d\n", cnlinks(prev1));*/
		if(!--c_nlinks(prev1)) {
			if(debug&32) printf("clause removed\n");
			dispterm(cclause(prev1));
			dispmem(CCLSZ, prev1);
		}
	}
/*printf("nlinks=%d\n", cnlinks(c));*/
	if(!--c_nlinks(c)) {		/* remove clause just retracted */
if(debug&32) printf("clause removed\n");
		dispterm(cclause(c));
		dispmem(CCLSZ, c);
	} else
		c_nextc(c) = NULL;
	gbindings(gtop)->blev = (levtype) prev;
	gbindings(gtop)->btermp = (Ptr) first;
/*printf("succeeding\n");*/
	return(SUCCEED);
}

p_1retract(t,l)				/* retract first matching clause */
	Ptr t;
	levtype l;
{
	Ptr th;
	levtype lh;
	int d;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsComp(t) || ctdict(t) != Darrow) {
		plerror(EUFUNC);
		return(ERROR);
	}
	findbind((Ptr)targ(1, t), l, &th, &lh);
	if(!IsFunc(th) || dprotect(tdict(th))) {
		plerror(EPROT);
		return(ERROR);
	}
	d = tdict(th);
	if(d1clause(d) == (Ptr) null)
		return(CUTFAIL);
/*printf("p_1retract: c_nlinks(d1clause(d))= %d\n", c_nlinks(d1clause(d)));*/
	gbindings(gtop)->btermp = (Ptr) 1;	/* flag for first clause */
	return(retract(d0clause(d), t, l));
}

p_2retract(t, l)			/* retract next matching clause */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	return(retract((Ptr) gbindings(gtop)->blev, t, l));
}

p_retwait(t, l)				/* retract all waits for proc */
	Ptr t;
	levtype l;
{
	int d;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(IsAtom(t))
		return(SUCCEED);
	if(!IsFunc(t) || dprotect(ctdict(t))) {
		plerror(EPROT);
		return(ERROR);
	}
	d = ctdict(t);
	if(dwait(d)) {
		displist(2, dwait(d));
		d_wait(d) = NULL;
	}
	return(SUCCEED);
}

p_1proc(t, l)			/* search dict for procs */
	Ptr t;
	levtype l;
{
	Ptr tp;
	levtype lp;

	findbind((Ptr)targ(1, t), l, &tp, &lp);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) != TVAR || ttype(tp) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	*(rsetend++) = 0;
	g_rset(gtop) = (Int) rsetend;
	return(FAIL);
}

p_2proc(t, l)			/* search dict for procs */
	Ptr t;
	levtype l;
{
	Ptr tp;
	levtype lp;
	int i;

	findbind((Ptr)targ(1, t), l, &tp, &lp);
	findbind((Ptr)targ(2, t), l, &t, &l);
	rsetend = (Ptr) gbindings(gtop);
	for(i = *rsetend; i < DICTLEN; i++)
		if(!dfree(i) && dnargs(i) &&
				d1clause(i) != (Ptr) null && !dprot(i))
			break;
	if(i < DICTLEN) {
		*(rsetend++) = i + 1;
		g_rset(gtop) = (Int) rsetend;
		addbind(tp, lp, (Ptr)ConsFunc(find(1, dname(i)), 0), NULLL);
		addbind(t, l, (Ptr)ConsInt(dnargs(i)-1), NULLL);
		return(SUCCEED);
	} else
		return(CUTFAIL);
}
