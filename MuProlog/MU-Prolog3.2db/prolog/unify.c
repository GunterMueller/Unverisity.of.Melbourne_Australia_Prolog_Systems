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
unify.c
unification, bindings, reset list, delaying, waking
messy interface with main - should be cleaned up a bit
*/
#include <stdio.h>
#include "types.h"
#include "dict.h"

extern Int *rsetend, debug;
extern Int *gtop;
extern Ptr woken;

extern Int waitflag;

unify(h, hl, c, cl)
	register Ptr c;
	register levtype cl;
	register Ptr h;		/* Not needed as a register (jws) */
	register levtype hl;	/* Not needed as a register (jws) */
{
	register levtype l;
	register int n;


	start:				/* used instead of tail recursion */
	if(IsVar(h))
		do {
			l = &hl[tvnum(h)];
			if(!l->btermp) {	/* h is a variable */
				if(IsVar(c))
					do {
						l = &cl[tvnum(c)];
						if(!l->btermp) {
							if(hl != cl || h != c)
							    addbind(h,hl,c,cl);
							return(SUCCEED);
						}
						cl = l->blev;
					} while(IsVar(c = l->btermp));
				addbind(h, hl, c, cl);
				return(SUCCEED);
			}
			hl = l->blev;
		} while(IsVar(h = l->btermp));
	/* h is not a variable */
	if(IsVar(c))
		do {
			l = &cl[tvnum(c)];
			if(!l->btermp) {	/* c is a variable */
				addbind(c, cl, h, hl);
#define OLDWAIT
#ifndef OLDWAIT
				if(hl == gbindings(gtop))
#endif
				 waitflag = 1;
				return(SUCCEED);
			}
			cl = l->blev;
		} while(IsVar(c = l->btermp));
	if(IsAtomic(c) || IsAtomic(h))
		if(c == h)
			return(SUCCEED);
#ifdef PARAM
		else if(IsParam(c) && dplev(atdict(c))) {
			cl = dplev(atdict(c));
			c = dpterm(atdict(c));
			goto start;
		} else if(IsParam(h) && dplev(atdict(h))) {
			hl = dplev(atdict(h));
			h = dpterm(atdict(h));
			goto start;
		}
#endif
		else
			return(FAIL);
	if(thead(c) != thead(h))
		return(FAIL);
	n = tnargs(c);
	while(--n > 0)
		if(!unify((Ptr)*++h, hl, (Ptr)*++c, cl))
			return(FAIL);
	c = (Ptr) *(c+1);
	h = (Ptr) *(h+1);
	goto start;
}

addbind(v, l, t, l1)	/* bind v at level l to t at level l1 */
			/* if gbindings(gtop) > l put it on the reset list */
	register Ptr v, t;
	register levtype l, l1;
{
	register Ptr p;

	if(l >= gbindings(gtop)) {
		bterm(v, l) = t;
		blevel(v, l) = l1;
	} else {
		if(p = (Ptr) blevel(v,l)) {
			register Ptr w, *wp;

			w = woken;
			wp = &woken;
			do {
				while(litem(w) > litem(p)) {
					wp = (Ptr*) &l_next(w);
					w = lnext(w);
				}
				if(litem(p) > litem(w)) {
					*wp = newmem(2);
					litem(*wp) = litem(p);
					l_next(*wp) = (Int) w;
					wp = (Ptr*) &l_next(*wp);
				}
				p = lnext(p);
			} while(p);
		}
		/* following are expanded for extra speed */
		/* set((Int *) &bterm(v, l), (Int) t); */
		/* set((Int *) &blevel(v, l), (Int) l1); */
		{
			register Int *btmp, *rs;

			rs = rsetend;

			btmp = (Int *) &bterm(v, l);
			*rs = (Int) btmp;
			*(rs+1) = *btmp;
			*btmp = (Int) t;

			btmp = (Int *) &blevel(v, l);
			*(rs+2) = (Int) btmp;
			*(rs+3) = *btmp;
			*btmp = (Int) l1;

			rsetend = rs + 4;
		}
	}
}

findbind(v, l, t, l1)		/* find binding of var v at level l */
	register Ptr v;
	Ptr *t;
	register levtype l;
	levtype *l1;
{
	register Ptr v1;
	register levtype bt;

	start:
	while(IsVar(v)) {
		bt = &l[tvnum(v)];
		if(!(v1 = bt->btermp))
			break;
		l = bt->blev;
		v = v1;
	}
#ifdef PARAM
	if(IsParam(v) && dplev(atdict(v))) {
		l = dplev(atdict(v));
		v = dpterm(atdict(v));
		goto start;
	}
#endif
	*t = v;
	*l1 = l;
}


set(addr, newval)		/* make assignment & put it on reset list */
				/* so it can be undone by reset later */
	Int *addr, newval;
{
	*rsetend = (Int) addr;
	*(rsetend+1) = *addr;
	rsetend = rsetend + 2;
	*addr = newval;
}

reset(rsetstart)		/* undo changes made by set */
	register Int *rsetstart;
{
	register Int *rs;	/* jws */

	for(rs = rsetend-2; rs >= rsetstart; rs -= 2)
		switch(*rs) {
		case RSTRUCT:
			dispmem(2, (Ptr)*(rs+1));
			break;
		case RTERM:
			dispterm((Ptr) *(rs+1));
			break;
#if 0 /*def DBASE */
		case RDB:
			/* ???????? missing mudd_abort ???????? */
			break;
#endif
		default:
			*((Int *)*rs) = *(rs+1);
		}
	rsetend = rs;
}

wakeprocs(nextgoal, gtop)		/* wake up procs in woken list */
					/* constructed by addbind */
	register Int    **nextgoal;
	Int     *gtop;
{
	register Int *p;
	register Ptr w;

	*nextgoal = gtop;
	w = woken;
	while(lnext(w) != (Ptr) NULL) {
		p = ((Int*)litem(w));
		if(asleep(p)) {
			set(&g_nextchild(p), (Int) *nextgoal);
			*nextgoal = p;
if(debug & 8){
printf("WAKE ");
printt(stdout,gtcall(p), gbindings(gparent(p)));
printf("\n");
}
		}
		w = lnext(w);
	}
}

delayp(gtop)				/* delay a proc call */
	register Int    *gtop;
{
	register Int *r, *rend;
	register Ptr t;

	rend = rsetend;
	for(r = grset(gtop); r< rend; r = r+4) {
		*((Int *) *r) = *(r+1);	/*reset btermp in binding */
		t = newmem(2);
		l_next(t) = *(r+3);		/* add to list of procs */
		litem(t) = (Int) gtop;
		*((Int *) *(r+2)) = (Int) t;	/* blevel = new list */
		*r = RSTRUCT;			/* reclaim space on backtrack */
		*(r+1) = (Int) t;
	}
	g_nextchild(gtop) = (Int) gtop;	/* mark as delayed (and asleep)*/
}
