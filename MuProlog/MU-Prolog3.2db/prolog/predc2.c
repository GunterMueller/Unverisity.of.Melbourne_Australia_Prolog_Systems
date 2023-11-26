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
predc2.c
functions called by PROLOG involving control of execution
etc
This was part of predc.c but it causes DS's compiler to crash so he split the
file.
*/

#include "types.h"
#include "dict.h"
#include "pred.h"

extern Int ndelay   /* put this in pred.h ? */
	;

p_exit(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	exit((int)XtrInt(t));
}
p_depth(t, l)
	Ptr t;
	levtype l;
{
	Int *g, d, i;
	Ptr t1;
	levtype l1;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsVar(t)) {
		plerror(EEVAR);
		return(ERROR);
	}
	g = gtop;
	d = 0;
	do {
		g = gparent(g);
		findbind(gtcall(g), gbindings(gparent(g)), &t1, &l1);
		i = tdict(t1);
		if(i != Dcall && i != Dcomma && i != Dsemi)
			d++;
	} while(tdict(t1) != D_cmd);
	addbind(t, l, (Ptr)ConsInt(d), NULLL);
	return(SUCCEED);
}

p_ancestor(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1, g;
	levtype l1;
	int d, i;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(!IsInt(t1)) {
		plerror(EEINT);
		return(ERROR);
	}
	d = XtrInt(t1)-1;
	findbind((Ptr)targ(2, t), l, &t, &l);
	g = gtop;
	do {
		g = gparent(g);
		findbind(gtcall(g), gbindings(gparent(g)), &t1, &l1);
		i = tdict(t1);
		if(i != Dcall && i != Dcomma && i != Dsemi)
			d--;
	} while(d >= 1 && tdict(t1) != D_cmd);
	if(d != 0)
		return(FAIL);
	else
		return(unify(t, l, t1, l1));
}

p_occurs(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;

	findbind((Ptr)targ(2, t), l, &t1, &l1);
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(IsComp(t)) {
		plerror(EUFUNC);
		return(ERROR);
	}
	return(occurs(t, l, t1, l1));
}

occurs(p, pl, t, tl)
	Ptr p, t;
	levtype pl, tl;
{
	Ptr t1;
	levtype l1;
	int i;

	if(IsComp(t))
		for (i = tnargs(t); i > 0; i--) {
			findbind((Ptr)targ(i, t), tl, &t1, &l1);
			if(occurs(p, pl, t1, l1) == SUCCEED)
				return(SUCCEED);
		}
	else if(p == t && (!IsVar(p) || pl == tl))
		return(SUCCEED);
	return(FAIL);
}

