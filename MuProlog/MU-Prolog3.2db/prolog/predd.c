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
predd.c:
		preds changing the dictionary etc.
*/
#include "types.h"
#include "pred.h"
extern int dentries;

p_hide(t,l)			/* make pred hiding */
	Ptr t;
	levtype l;
{
	int d, i;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsFunc(t)) {
		plerror(EEFUNC);
		return(ERROR);
	}
	d = tdict(t);
	d_prot(d) = (dprot(d) & ~DHBITS) + DHIDING;
	if(++dentries < DICTLEN) {
		for(i = (d + 1) % DICTLEN; ; i = (i + 1) % DICTLEN)
			if(dfree(i))
				break;
		dict[i] = newmem(DENTRYSZ);
		d_name(i) = (Int) dname(d);
		d_nargs(i) = dnargs(d);
		d_1clause(i) = (Int) null;
		d_wait(i) = NULL;
#ifdef CLINDEX
		d_clindex(i) = NULL;	/* (jws) */
#endif
		d_prot(i) = 0;
		d_prprec(i) =
		d_inprec(i) =
		d_psprec(i) = -1;
		d_nlinks(i) = 1;
	} else
		error("dictionary explodes - bang!\n");
	return(SUCCEED);
}

p_hidden(t,l)		/* make visible proc hidden and hiding proc visible */
	Ptr t;
	levtype l;
{
	int i, j;

	for(i = 0; i < DICTLEN; i++)
		if(!dfree(i) && dhide(i) == DHIDING) {
			d_prot(i) = (dprot(i) & ~DHBITS) + DVISIBLE;
			for(j = (i+1) % DICTLEN; ; j = (j+1) % DICTLEN)
				if(!dfree(j) && dname(j) == dname(i) &&
						dnargs(j) == dnargs(i) &&
						dhide(j) == DVISIBLE)
					break;
			d_prot(j) = (dprot(j) & ~DHBITS) + DHIDDEN;
		}
	return(SUCCEED);
}

p_protect(t,l)				/* protect proc */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsFunc(t)) {
		plerror(EEFUNC);
		return(ERROR);
	}
	d_prot(tdict(t)) |= DPROT;
	return(SUCCEED);
}

p_unprot(t,l)				/* unprotect proc */
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsFunc(t)) {
		plerror(EEFUNC);
		return(ERROR);
	}
	d_prot(tdict(t)) &= ~DPROT;
	return(SUCCEED);
}

#ifdef CLINDEX
p_clindex(t, l)				/* setup clause indexing (jws) */
	Ptr t;
	levtype l;
{
	register int n;
	register Int d;
	register Ptr proc, ci;
	Ptr t1;
	levtype l1;

	findbind((Ptr)targ(2, t), l, &t1, &l1);
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsInt(t)) {
		plerror(EEINT);
		return(ERROR);
	}
	if(!IsComp(t1)) {
		plerror(EEFUNC);
		return(ERROR);
	}
	n = tnum(t);
	d = tdict(t1);
	proc = dproc(d);
	if(n < 1 || n >= dnargs(d)) {
		fprintf(stderr, "index %d out of range\n", n);
		plerror(EMISC);
		return(ERROR);
	}
	if(dclindex(d) != NULL) {
		fprintf(stderr, "%s already indexed\n", dname(d));
		plerror(EMISC);
		return(ERROR);
	}
	if(proc == (Ptr) null) {
		fprintf(stderr, "%s has no clauses\n", dname(d));
		plerror(EMISC);
		return(ERROR);
	}

/* build the list of (key, clause list)-pairs */
	for(; proc != NULL; proc = cnextc(proc)) {
		register Int di;

		di = targ(n, chead(proc));

		if(IsVar(di))
			continue;
		if(IsComp(di))
			di = thead(di);

		for(ci = dclindex(d); ci != NULL; ci = clnext(ci))
			if(clpred(ci) == di)
				break;
		if(ci == NULL) {
			ci = newmem(CLSIZE);
			cl_next(ci) = (Int) dclindex(d);
			cl_pred(ci) = di;
			cl_proc(ci) = NULL;
			d_clindex(d) = (Int) ci;
		}

		c_nlinks(proc) += DICTLEN;	/* prevent reclaimation */
	}

/*
Put the node containing the variable clause list and the number of indexed
argument at the beginning of the index
*/
	ci = (Ptr) newmem(CLSIZE);
	cl_next(ci) = (Int) dclindex(d);
	cl_n(ci) = n;
	cl_proc(ci) = NULL;
	d_clindex(d) = (Int) ci;

/* build the clause lists */
	for(proc = dproc(d); proc != NULL; proc = cnextc(proc)) {
		register Ptr cl;
		register Int di;

		di = targ(n, chead(proc));

		if(IsComp(di))
			di = thead(di);

		for(ci = dclindex(d); ci != NULL; ci = clnext(ci)) {
			if(!IsVar(di) && (di != clpred(ci) || ci == dclindex(d)))
				continue;
			cl = clproc(ci);
			if(cl == NULL) {
				cl = (Ptr) newmem(CCLSZ);
				cl_proc(ci) = (Int) cl;
			} else {
				while(cnextc(cl) != NULL)
					cl = cnextc(cl);
				c_nextc(cl) = (Int) newmem(CCLSZ);
				cl = cnextc(cl);
			}
			c_nextc(cl) = (Int) NULL;
			c_nlinks(cl) = DICTLEN+1; /* Prevent reclaimation */
			c_nvars(cl) = cnvars(proc);
			c_clause(cl) = (Int) cclause(proc);
		}
	}

	return(SUCCEED);
}
#endif

#ifdef PARAM

static int freeparam, npfree = 0;

p_newparam(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int i;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsAtom(t1) || !IsVar(t)) {
		plerror(EECONST);
		return(ERROR);
	}
	if(!npfree) {	/* free list empty- get a free spot somewhere */
		freeparam = find(-1, dname(atdict(t1)));
		d_nargs(freeparam) = 1;
		d_prot(freeparam) = DPARAM;
		d_plev(freeparam) = NULL;
		npfree = 1;
	}
	set(&npfree, npfree-1);
	i = freeparam;
	d_name(i) = d_name(atdict(t1));
	addbind(t, l, (Ptr)ConsFunc(i, 0), NULL);
	set(&freeparam, dplev(i));
	set(&d_plev(i), NULL);
	return(SUCCEED);
}

p_bind(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(!IsParam(t1)) {
		plerror(EECONST);
		return(ERROR);
	}
	set(&d_plev(atdict(t1)), l);
	d_pterm(atdict(t1)) = (Int) t;
	return(SUCCEED);
}

p_isparam(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(IsParam(t))
		return(SUCCEED);
	else
		return(FAIL);
}

#endif
