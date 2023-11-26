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
preda.c
functions called by PROLOG involving arithmatic
etc
*/

#include <signal.h>
#include "types.h"
#include "dict.h"
#include "pred.h"

int fpeflag;			/* floating exception flag */

onfpe()
{
	fpeflag = 1;
	signal(SIGFPE, onfpe);
}

nbind(t,l,i)			/* grab memory and bind var to a simple term */
	Ptr t;
	levtype l;
	Int i;
{
		addbind(t, l, (Ptr)i, NULLL);
}

p_is(t, l)				/* is with expressions */
	Ptr t;
	levtype l;
{
	Ptr tl, tr;
	levtype ll, lr;
	Int r;

	findbind((Ptr)targ(1, t), l, &tl, &ll);
	findbind((Ptr)targ(2, t), l, &tr, &lr);
	switch(eval(tr, lr, &r)) {
	case ERROR : return(ERROR);
	case DELAY : return(DELAY);
	default:
		if(ttype(tl) == TNUM)
			if(tnum(tl) == r)
				return(SUCCEED);
			else
				return(FAIL);
		else if(!IsVar(tl)) {
				plerror(EUFUNC);
				return(ERROR);
		} else {
			nbind(tl, ll, ConsInt(r));
			return(SUCCEED);
		}
	}
}


p_relop(t,l)				/* relational ops with expressions */
 Ptr t;
	levtype l;
{
	Int r;

	switch(eval(t, l, &r)) {
	case SUCCEED :	if(r)
				return(SUCCEED);
			else
				return(FAIL);
	case DELAY :	return(DELAY);
	case ERROR :	return(ERROR);
	}
}

static
eval(t, l, r)				/* eval expression & put result in r */
 Ptr t;
	levtype l;
	register Int *r;
{

 Ptr t1;
	levtype l1;
	Int a1, a2;
	int e;
	switch(tagtype(t)) {
	case TAGVAR :
			addbind(t, l, (Ptr)ConsInt(0), NULLL);
			return(DELAY);
	case TAGNUM:
			*r = tnum(t);
			return(SUCCEED);
	case TAGATOM:
#ifdef PARAM
			if(IsParam(t)) {
				*r = atdict(t);
				return(SUCCEED);
			}
#endif
			plerror(EUFUNC);
			return(ERROR);
	default:
			if(tnargs(t) == 1) {
				findbind((Ptr)targ(1, t), l, &t1, &l1);
				if((e = eval(t1, l1, &a1)) != SUCCEED)
					return(e);
				switch(ctdict(t)) {
				case Duminus:	*r = -a1;
						return(SUCCEED);
				case Dbnot:	*r = ~a1;
						return(SUCCEED);
				default:
						plerror(EUFUNC);
						return(ERROR);
				}
			}
			if(tnargs(t) != 2) {
				plerror(EUFUNC);
				return(ERROR);
			}
			findbind((Ptr)targ(1, t), l, &t1, &l1);
			if((e = eval(t1, l1, &a1)) != SUCCEED)
				return(e);
			findbind((Ptr)targ(2, t), l, &t1, &l1);
			if(ctdict(t) == Ddot) {
				if(!IsAtom(t1) || atdict(t1) != Dnil) {
					plerror(EUFUNC);
					return(ERROR);
				}
			} else if((e = eval(t1, l1, &a2)) != SUCCEED)
				return(e);
			switch(ctdict(t)) {
			case Dplus	: *r = a1 + a2; break;
			case Dminus	: *r = a1 - a2; break;
			case Dmult	: *r = a1 * a2; break;
			case Ddiv	: *r = a1 / a2; break;
					/* pity C has no mod operator ... */
			case Dmod	: *r = (a1%a2+a2)%a2; break;
			case Dlt	: *r = a1 < a2; break;
			case Dle	: *r = a1 <= a2; break;
			case Dgt	: *r = a1 > a2; break;
			case Dge	: *r = a1 >= a2; break;
			case Deeq	: *r = a1 == a2; break;
			case Dene	: *r = a1 != a2; break;
			case Dland	: *r = a1 && a2; break;
			case Dlor	: *r = a1 || a2; break;
			case Dband	: *r = a1 & a2; break;
			case Dbor	: *r = a1 | a2; break;
			case Dxor	: *r = a1 ^ a2; break;
			case Dshiftl	: *r = a1 << a2; break;
			case Dshiftr	: *r = a1 >> a2; break;
			case Ddot	: *r = a1; break;
			default:
					plerror(EUFUNC);
					return(ERROR);
			}
			if(fpeflag) {
				fpeflag = 0;
				plerror(EFPE);
				return(ERROR);
			} else
				return(SUCCEED);
	}
}

p_plus(t, l)				/* tri-directional plus */
	Ptr t;
	levtype l;
{
	Ptr t1, t2, t3;
	levtype l1, l2, l3;
	register Int r;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t2, &l2);
	findbind((Ptr)targ(3, t), l, &t3, &l3);
	if(ttype(t1) == TNUM && ttype(t2) == TNUM)
		r = tnum(t1) + tnum(t2);
	else if(ttype(t2) == TNUM && ttype(t3) == TNUM) {
		r = tnum(t3) - tnum(t2);
		t3 = t1;
		l3 = l1;
	} else if(ttype(t1) == TNUM && ttype(t3) == TNUM) {
		r = tnum(t3) - tnum(t1);
		t3 = t2;
		l3 = l2;
	} else {
		if(ttype(t1) == TVAR)
			addbind(t1, l1, (Ptr)ConsInt(0), NULLL);
		else if(IsFunc(t1)) {
			plerror(EUFUNC);
			return(ERROR);
		}
		if(ttype(t2) == TVAR && (t2 != t1 || l2 != l1))
			addbind(t2, l2, (Ptr)ConsInt(0), NULLL);
		else if(IsFunc(t2)) {
			plerror(EUFUNC);
			return(ERROR);
		}
		if(ttype(t3) == TVAR && (t3 != t1 || l3 != l1)
					&& (t3 != t2 || l3 != l1))
			addbind(t3, l3, (Ptr)ConsInt(0), NULLL);
		else if(IsFunc(t3)) {
			plerror(EUFUNC);
			return(ERROR);
		}
		return(DELAY);
	}
	if(ttype(t3) == TNUM)
		if(r == tnum(t3))
			return(SUCCEED);
		else
			return(FAIL);
	else {
		nbind(t3, l3, ConsInt(r));
		return(SUCCEED);
	}
}

p_functor(t, l)				/* break func into const + arity */
	register Ptr t;
	register levtype l;
{
 Ptr ft, v1, v2;
	levtype l1, l2;

	findbind((Ptr)targ(1, t), l, &ft, &l1);
	if(ttype(ft) == TNUM) {
		plerror(EUINT);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &v1, &l1);
	findbind((Ptr)targ(3, t), l, &v2, &l2);
	if(ttype(v1) != TVAR || ttype(v2) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(ttype(ft) == TVAR)
		return(DELAY);
	if(IsComp(ft)) {
		nbind(v1, l1, ConsFunc(find(1, dname(ctdict(ft))), 0));
		nbind(v2, l2, ConsInt(tnargs(ft)));
	} else {
		nbind(v1, l1, ConsFunc(find(1, dname(atdict(ft))), 0));
		nbind(v2, l2, ConsInt(0));
	}
	return(SUCCEED);
}

p_univ(t, l)			/* =.. (break up term) */
 Ptr t;
	levtype l;
{
	Ptr t1, t2, t3;
	levtype l1, l2, l3, b;
	int n, v;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t2, &l2);
	if(ttype(t1) == TVAR) {
		t3 = t2;
		l3 = l2;	/* calc n = number of args + 1 */
		for(n = 0; IsComp(t3) && ctdict(t3) == Ddot; n++)
			findbind((Ptr)targ(2, t3), l3, &t3, &l3);
		if(ttype(t3) == TVAR) {
			addbind(t3, l3, (Ptr)ConsInt(0), NULLL);
			if(t1 != t3 || l1 != l3)
				addbind(t1, l1, (Ptr)ConsInt(0), NULLL);
			return(DELAY);
		} else if(!IsAtom(t3) || atdict(t3) != Dnil || !n)  {
			plerror(EELIST);
			return(ERROR);
		}
		rsetend = rsetend + 3 * n - 1;	/* grab memory */
		g_rset(gtop) = (Int) rsetend;
		findbind((Ptr)targ(2, t2), l2, &t3, &l3);
				/* each arg is a var bound to a member of t2 */
		for(b = gbindings(gtop); IsComp(t3) && ctdict(t3) == Ddot;
				b++) {
			b->btermp = (Ptr) targ(1, t3);
			b->blev = l3;
			findbind((Ptr)targ(2, t3), l3, &t3, &l3);
		}
		findbind((Ptr)targ(1, t2), l2, &t2, &l2);
		if(ttype(t2) == TVAR) {
			addbind(t2, l2, (Ptr)ConsInt(0), NULLL);
			return(DELAY);
		} else if(!IsAtom(t2)) {
			plerror(EECONST);
			return(ERROR);
		}
		t3 = (Ptr) b;
		if(n == 1)		/* an atom */
			addbind(t1, l1, t2, NULLL);
		else {
			{char *s;	/* to avoid expression ov. on 3240 */
			s = dname(tdict(t2));
			thead(t3) = ConsFunc(find(n,s), n - 1);
			}
			for(v = 0; v < n - 1; v++)
				targ(v+1, t3) = ConsVar(D_, v);
			addbind(t1, l1, (Ptr)b, gbindings(gtop));
		}
		return(SUCCEED);
	} else if(ttype(t1) == TNUM) {
		return(FAIL);
	} else {
		if(IsComp(t1))
			n = tnargs(t1);
		else
			n = 0;
		rsetend = rsetend + 5 * n + 3;
		g_rset(gtop) = (Int) rsetend;
		b = gbindings(gtop);
		for(v = 1; v <= n; v++) {
			b->btermp = (Ptr) targ(v, t1);
			b->blev = l1;
			b++;
		}
		t3 = (Ptr) b;
		thead(t3) = ConsFunc(Ddot, 2);
		if (IsAtom(t1))
			targ(1, t3) = (Int) t1;
		else
			{char *s;	/* to avoid expression ov. on 3240 */
			s = dname(tdict(t1));
			targ(1, t3) = ConsFunc(find(1, s), 0);
			}
		targ(2, t3) = (Int) (t3 + 3);
		t3 = (Ptr) (t3 + 3);
		for(v = 0; v < n; v++) {
			thead(t3) = ConsFunc(Ddot, 2);
			targ(1, t3) = ConsVar(D_, v);
			targ(2, t3) = (Int) (t3 + 3);
			t3 = (Ptr) (t3 + 3);
		}
		targ(2, t3-3) = ConsFunc(Dnil, 0);
		return(unify((Ptr)b, gbindings(gtop), t2, l2));
	}
}

p_name(t, l)				/* name (cvt const to string) */
 Ptr t;
	levtype l;
{
	Ptr t1, t2, t3;
	levtype l1, l2, l3;
	ident i;
	char *s;
	int n;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t2, &l2);
	if(ttype(t1)  == TVAR) {
		s = i;
		n = IDLEN-1;
		while(IsComp(t2) && ctdict(t2) == Ddot && --n) {
			findbind((Ptr)targ(1, t2), l2, &t3, &l3);
			if(ttype(t3) == TVAR) {
				addbind(t3, l3, (Ptr)ConsInt(0), NULLL);
				if(t3 != t1 || l3 != l1)
					addbind(t1, l1, (Ptr)ConsInt(0), NULLL);
				return(DELAY);
			} else if(ttype(t3) != TNUM || (tnum(t3) & ~255)) {
				plerror(EESTR);
				return(ERROR);
			}
			*s++ = tnum(t3);
			findbind((Ptr)targ(2, t2), l2, &t2, &l2);
		}
		if(ttype(t2) == TVAR) {
			addbind(t2, l2, (Ptr)ConsInt(0), NULLL);
			if(t1 != t2 || l1 != l2)
				addbind(t1, l1, (Ptr)ConsInt(0), NULLL);
			return(DELAY);
		} else if(!IsAtom(t2) || atdict(t2) != Dnil) {
			plerror(EESTR);
			return(ERROR);
		}
		*s = '\0';
		addbind(t1, l1, (Ptr)ConsFunc(find(1, i), 0), NULLL);
		return(SUCCEED);
	} else if(ttype(t1) == TNUM) {
		plerror(EUINT);
		return(ERROR);
	} else if(!IsAtom(t1)) {
		plerror(EECONST);
		return(ERROR);
	} else {
		if(!*dname(atdict(t1)))		/* null atom ('') */
			return(unify((Ptr)ConsFunc(Dnil, 0), NULLL, t2, l2));
		t3 = (Ptr) rsetend;
		for(s = dname(atdict(t1)); *s; s++) {
			thead(t3) = ConsFunc(Ddot, 2);
			targ(1, t3) = ConsInt(*s);
			targ(2, t3) = (Int)(t3 + 3);
			t3 = (Ptr) (t3 + 3);
		}
		targ(2, t3-3) = ConsFunc(Dnil, 0);
		rsetend = (Int*) t3;
		g_rset(gtop) = (Int) rsetend;
		return(unify((Ptr) gbindings(gtop), gbindings(gtop),
						t2, l2));
	}
}
