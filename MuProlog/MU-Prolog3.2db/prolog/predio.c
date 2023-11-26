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
predio.c
functions called by PROLOG involved in I/O
*/
#include "types.h"
#include "pred.h"
#include "dict.h"

#ifdef FCNTL
#include <fcntl.h>
#endif

/* YUK! */
#ifdef DBASE
#define NEWDBASE
#endif
#ifdef NEWDBASE
#include "muddpl.h"
#else
#define MAXDBQ 0
#endif

#include <signal.h>
#include <setjmp.h>
extern jmp_buf intrbuf;

FILE *
pfiles[MAXFILES + MAXDBQ] = {
		stdin, stdout, stderr, stdin, stdout };
char
pfmode[MAXFILES + MAXDBQ] = {
		'r', 'w', 'w', 'r', 'w' };

extern fget();

p_getc(t, l)			/* get char from input */
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int f;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(ttype(t1) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	f = tnum(t1);
	if(f < 0 || f > MAXFILES || !pfiles[f] || pfmode[f] != 'r') {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(FAIL);
	} else {
		extern int doljump;

		if (setjmp(intrbuf) == 0) {
			doljump = 1;
			nbind(t,l, ConsInt(getc(pfiles[f])));
			if(pfiles[f] == stdin && feof(stdin))
				rewind(stdin);
		}
		doljump = 0;
		return(SUCCEED);
	}
}

p_putc(t,l)			/* output char */
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int f;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(ttype(t1) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	f = tnum(t1);
	if(f < 0 || f > MAXFILES || !pfiles[f] || pfmode[f] == 'r') {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	} else {
		putc((char) tnum(t), pfiles[f]);
		fflush(pfiles[f]);
		return(SUCCEED);
	}
}

static
freefile()
{
	int i;

	for(i = MAXFILES-1; i > 2 && pfiles[i]; i--)
		;
	if(!pfiles[i])
		return(i);
	else
		return(-1);
}

p_open(t,l)			/* open a file */
	Ptr t;
	levtype l;
{
	Ptr tf, tn;
	levtype lf, ln;
	FILE *fopen();
	int i;

	findbind((Ptr)targ(1, t), l, &tf, &lf);
	if(!IsAtom(tf)) {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &tn, &ln);
	if(IsFunc(tn)) {
		plerror(EFILE);
		return(ERROR);
	} else if(ttype(tn) == TVAR) {
		if((i = freefile()) > 0)
			nbind(tn, ln, ConsInt(i));
		else {
			plerror(EOPEN);
			return(ERROR);
		}
	} else
		i = tnum(tn);
	if(i < 0 || i >= MAXFILES || pfiles[i]) {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(3, t), l, &t, &l);
	if(!IsAtom(t)) {
		plerror(EErwa);
		return(ERROR);
	}
	pfmode[i] = *dname(atdict(t));
	if(pfmode[i] != 'r' && pfmode[i] != 'w' && pfmode[i] != 'a') {
		plerror(EErwa);
		return(ERROR);
	}
	if(!(pfiles[i] = fopen(dname(atdict(tf)),dname(atdict(t))))) {
		plerror(EOPEN);
		return(ERROR);
	}
	return(SUCCEED);
}
p_close(t,l)			/* close file */
	Ptr t;
	levtype l;
{
	int i;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	i = tnum(t);
	if(i < 5 || i >= MAXFILES ||  !pfiles[i]) {
		plerror(EFILE);
		return(ERROR);
	}
	if(pfiles[i] != stdin && pfiles[i] != stdout && pfiles[i] != stderr)
		fclose(pfiles[i]);
	pfiles[i] = NULL;
	return(SUCCEED);
}

p_fmove(t, l)			/* move a file descriptor */
		/* this horrible bit of code is needed by the "next" */
		/* predicate, which needs to change standard in and out */
		/* It relies on inner workings of fclose and stdio.h */
	Ptr t;
	levtype l;
{
	int n1,n2;
	FILE *old, *new;
	Ptr tn;
	levtype ln;

	findbind((Ptr)targ(1, t), l, &tn, &ln);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(tn) != TNUM || ttype(t) != TNUM || (n1=tnum(tn)) < 0
			|| (n2=tnum(t)) < 0 || n1 >= MAXFILES
			|| n2 >= MAXFILES || !pfiles[n1] || !pfiles[n2]) {
		plerror(EFILE);
		return(ERROR);
	}
	if(fileno(pfiles[n2]) != n2) {
		fprintf(stderr, "error in fmove\n");
		plerror(EFILE);
		return(ERROR);
	}
	pfmode[n2] = pfmode[n1];
	old = pfiles[n1];
	new = pfiles[n2];
	pfiles[n1] = NULL;
	fclose(new);		/* close stream we are copying to */

#ifdef FCNTL
	if(fcntl(fileno(old), F_DUPFD, n2) != n2) {
		plerror(EFILE);
		return(ERROR);
	}
#else
	dup2(fileno(old), n2);
#endif
			/* this is really awful and relies on stdio */
			/* Still, if people really want 'next' in prolog... */
	*new = *old;		/* copy stream */
	old->_ptr =
	old->_base = NULL;	/* make it look like there is no buffer */
	old->_flag &= ~_IOMYBUF;
	fileno(new) = n2;
	fclose(old);		/* close old stream(without affecting buffer)*/
	return(SUCCEED);
}

p_fcopy(t, l)			/* copy a prolog "file descriptor" */
	Ptr t;
	levtype l;
{
	int n1,n2;
	Ptr tn;
	levtype ln;

	findbind((Ptr)targ(1, t), l, &tn, &ln);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(tn) != TNUM || ttype(t) != TNUM || (n1=tnum(tn)) < 0
			|| (n2=tnum(t)) < 0 || n1 >= MAXFILES
			|| n2 >= MAXFILES || !pfiles[n1]) {
		plerror(EFILE);
		return(ERROR);
	}
	pfmode[n2] = pfmode[n1];
	pfiles[n2] = pfiles[n1];
	return(SUCCEED);
}

p_read(t,l)			/* read a term */
	Ptr t;
	levtype l;
{
	Ptr newt, rt;
	levtype b;
	int i;

	findbind((Ptr)targ(1, t), l, &rt, &b);
	if(ttype(rt) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	i = tnum(rt);
	if(i < 0 || i >= MAXFILES+MAXDBQ || !pfiles[i] || pfmode[i] != 'r') {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	} else {
		if(!(rt = getterm(fget, (int **) pfiles[i])))
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
		addbind(t, l, newt, gbindings(gtop));
		return(SUCCEED);
	}
}

p_wlist(t,l)			/* list a proc in prefix format (DEAD) */
				/* now just lists wait declarations */
	Ptr t;
	levtype l;
{
	Ptr t1;
/*	Ptr t2, prev;
	levtype l1;
*/
	int n;
	FILE *f;

/*
	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(ttype(t1) != TNUM || (n=tnum(t1)) < 0 || n >= MAXFILES || !pfiles[n]
				|| pfmode[n] == 'r') {
		plerror(EFILE);
		return(ERROR);
	}
*/
	 n = 4;
	f = pfiles[n];
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(!IsFunc(t)) {
		plerror(EEFUNC);	/* can't list a var or int */
		return(ERROR);
	}
	if(d1clause(tdict(t)) == (Ptr) null || dprot(tdict(t)))
		return(FAIL);	/* no proc */
	for(t1 = dwait(tdict(t)); t1; t1 = lnext(t1)) {
		fprintf(f,"?- wait %s(", dname(tdict(t)));
		n = 1 << tnargs(t) >> 1;
		while(n) {
			fprintf(f,"%ld",(long)( (~litem(t1) & n)/n));
			if(n >>= 1)
				fprintf(f,", ");
		}
		fprintf(f,").\n");
	}
	return(SUCCEED);
#ifdef garbage
	t1 = d1clause(tdict(t));
	prev = NULL;
	while(t1 && t1 != prev) {
		if(cisfunc((Ptr) t1)) {
			fprintf(f,"\"");
			printt(f,t, (levtype)gbindings(goalstk));
			fprintf(f," :- C_FUNCTION.\"\n");
		} else {
			t2 = (Ptr)targ(1, cclause((Ptr) t1));
			printt(f,t2, (levtype)gbindings(goalstk));
			t2 = (Ptr)targ(2, cclause((Ptr) t1));
			if(!IsAtom(t2) || atdict(t2) != Dtrue) {
				fprintf(f," :- ");
				printt(f,t2, (levtype)gbindings(goalstk));
			}
			fprintf(f,".\n");
		}
		prev = t1;
		t1 = cnextc((Ptr) t1);
	}
	if(t1)
		fprintf(f,"Above clause repeated indefinitely.\n");
	return(SUCCEED);
#endif
}

p_write(t, l)			/* write term */
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int n, swflags;

	swflags = wflags;
	if(tnargs(t) != 2) {
		findbind((Ptr)targ(3, t), l, &t1, &l1);
		if(!IsInt(t1)) {
			plerror(EEINT);
			return(ERROR);
		}
		wflags = XtrInt(t1);
	}
	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(ttype(t1) != TNUM || (n=tnum(t1)) < 0 || n >= MAXFILES || !pfiles[n]
				|| pfmode[n] == 'r') {
		wflags = swflags;
		plerror(EFILE);
		return(ERROR);
	}
	printt(pfiles[n], (Ptr)targ(2, t), l);
	fflush(pfiles[n]);
	wflags = swflags;
	return(SUCCEED);
}

p_pcons(t, l)			/* consult prefix file */
	Ptr t;
	levtype l;
{
	int ch;
	Ptr t1;
	levtype l1;
	FILE *f, *fopen();

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(!IsAtom(t1)) {
		plerror(EECONST);
		return(ERROR);
	}
	if(!(f = fopen(dname(atdict(t1)), "r"))) {
		plerror(EOPEN);
		return(ERROR);
	}
	while((ch = getc(f)) != EOF) {
		t = getterm(fget, (int **) f);
		p_nassert(t, gbindings(goalstk));
		dispterm(t);
	}
	fclose(f);
	return(SUCCEED);
}

p_addop(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1, t2;
	levtype l1, l2;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t2, &l2);
	findbind((Ptr)targ(3, t), l, &t, &l);
	if(!IsInt(t1)) {
		plerror(EEINT);
		return(ERROR);
	}
	if(!IsAtom(t2) || !IsAtom(t)) {
		plerror(EECONST);
		return(ERROR);
	}
	if(!op((int)XtrInt(t1), dname(atdict(t2)), atdict(t))) {
		plerror(EUFUNC);
		return(ERROR);
	}
	return(SUCCEED);
}

p_backtrace(t, l)			/* write ancestors */
	Ptr t;
	levtype l;
{
	Int n, *c;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	n = tnum(t);
	for(c = gparent(gtop); gparent(c) && n-- > 0; c = gparent(c)) {
		findbind(gtcall(c), gbindings(gparent(c)), &t, &l);
		if(tdict(t) == Dcomma || tdict(t) == Dsemi || tdict(t) == Dcall)
			n++;
		else {
			printf("?- ");
			printt(stdout,t, l);
			printf(".\n");
		}
	}
	return(SUCCEED);
}

p_fprintf(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	Int i, f, arg[16];
#define PBUFLEN 500
	char *bp, buf[PBUFLEN];

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(ttype(t1) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	f = tnum(t1);
	if(f < 0 || f >= MAXFILES || !pfiles[f] || pfmode[f] != 'w') {
		plerror(EFILE);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &t1, &l1);
	if(ttos(t1, l1, buf, 500) != SUCCEED) {
		plerror(EESTR);
		return(ERROR);
	}
	arg[0] = (Int) buf;
	for(bp = buf; *bp++; )          /* find end of string */
		;
	findbind((Ptr)targ(3, t), l, &t, &l);
	for(i = 1; IsComp(t) && i < 16; i++) {
		if(ctdict(t) != Ddot) {
			plerror(EELIST);
			return(ERROR);
		}
		findbind((Ptr)targ(1, t), l, &t1, &l1);
		switch(tagtype(t1)) {
		case TAGNUM:
			arg[i] = tnum(t1);
			break;
		case TAGATOM:
			arg[i] = (Int) dname(atdict(t1));
			break;
		case TAGCOMP:
			if(ctdict(t) != Ddot) {
				plerror(EUFUNC);
				return(ERROR);
			}
			if(ttos(t1, l1, bp, buf + PBUFLEN - bp)
					!= SUCCEED) {
				plerror(EESTR);
				return(ERROR);
			}
			arg[i] = (Int) bp;
			while(*bp++)
				;
			break;
		case TAGVAR:
			plerror(EUVAR);
			return(ERROR);
		}
		findbind((Ptr)targ(2, t), l, &t, &l);
	}
	if(!IsAtom(t) || atdict(t) != Dnil) {
		plerror(EESTR);
		return(ERROR);
	}
	fprintf(pfiles[f], (char*)arg[0], arg[1], arg[2],
			arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9],
			arg[10], arg[11], arg[12], arg[13], arg[14], arg[15]);
	return(SUCCEED);
}

ttos(t, l, s, n)	/* prolog string to C string of length <= n */
	Ptr t;
	levtype l;
	char *s;
	int n;
{
	Ptr t1;
	levtype l1;

	while(IsComp(t) && ctdict(t) == Ddot && --n) {
		findbind((Ptr)targ(1, t), l, &t1, &l1);
		if(ttype(t1) == TVAR)
			return(DELAY);
		else if(ttype(t1) != TNUM || (tnum(t1) & ~255))
			return(ERROR);
		*s++ = tnum(t1);
		findbind((Ptr)targ(2, t), l, &t, &l);
	}
	if(ttype(t) == TVAR)
		return(DELAY);
	else if(!IsAtom(t) || atdict(t) != Dnil)
		return(ERROR);
	*s = '\0';
	return(SUCCEED);
}

p_pipe(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int pd[2], ip, op;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(IsInt(t1)) {
		ip = XtrInt(t1);
		if(ip <= 4 || ip >= MAXFILES || pfiles[ip]) {
			plerror(EFILE);
			return(ERROR);
		}
	} else if(IsVar(t1)) {
		if((ip = freefile()) < 0) {
			plerror(EOPEN);
			return(ERROR);
		} else
			addbind(t1, l1, (Ptr)ConsInt(ip), gbindings(goalstk));
	} else {
		plerror(EUFUNC);
		return(ERROR);
	}
	if(IsInt(t)) {
		op = XtrInt(t);
		if(op <= 4 || op >= MAXFILES || pfiles[op]) {
			plerror(EFILE);
			return(ERROR);
		}
	} else if(IsVar(t)) {
		pfiles[ip] = stdin;		/* so freefile doesnt grab ip */
		if((op = freefile()) < 0) {
			pfiles[ip] = NULL;
			plerror(EOPEN);
			return(ERROR);
		} else
			addbind(t, l, (Ptr)ConsInt(op), gbindings(goalstk));
	} else {
		plerror(EUFUNC);
		return(ERROR);
	}
	if(pipe(pd) < 0) {
		pfiles[ip] = NULL;
		plerror(EOPEN);
		return(ERROR);
	}
	pfiles[ip] = fdopen(pd[0], "r");
	pfmode[ip] = 'r';
	pfiles[op] = fdopen(pd[1], "w");
	pfmode[op] = 'w';
	return(SUCCEED);
}

p_wflags(t, l)
	Ptr t;
	levtype l;
{
	return(sysvar(t, l, &wflags));
}
