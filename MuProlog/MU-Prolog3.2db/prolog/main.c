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
main.c
main loop handling goalstk
*/
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/times.h>
#include "types.h"
#include "dict.h"
#include "pred.h"

extern Int wflags;

extern onfpe();
Int     debug=0,					/* debug status */
	ndelay,                                 /* no. of delayed procs */
	plerrnum,				/* error number */
	intflag,				/* set on interrupts */
	gargc,					/* copy of argc */
	waitflag
	;
Int
	*nextgoal,
	*cont;
char *gargv[MAXARGC];			/* copy of argv */
Ptr woken;				/* list of woken procs */

long	start_time;
struct	tms tbuf;

#define savemain() {			/* save vars used by main */\
				/* gtop is saved by savedata */\
	long *bp;\
\
	bp = &savebuf.l[S_MAIN];\
	*bp++ = (long) wflags;\
	*bp++ = (long) debug;\
	*bp++ = (long) ndelay;\
	*bp++ = (long) proc;\
	*bp++ = (long) woken;\
	*bp++ = (long) call;\
	*bp++ = (long) clev;\
	*bp = (long) cont;\
}

#define restmain() {			/* restore vars */\
	long *bp;\
\
	bp = &savebuf.l[S_MAIN];\
	wflags = (Int) *bp++;\
	debug = (Int) *bp++;\
	ndelay = (Int) *bp++;\
	proc = (Ptr) *bp++;\
	woken = (Ptr) *bp++;\
	call = (Ptr) *bp++;\
	clev = (levtype) *bp++;\
	cont = (Ptr) *bp;\
}

main(argc, argv)
	int argc;
	char *argv[];
{
	register Ptr t;
	int onintr();

	start_time = times(&tbuf);
	if(debug) {
		Int temp;               /* DS: needed below */
		if(XtrInt(ConsInt(-1)) != -1)
			printf("Not 2's comp?: change macros in types.h?\n");
		temp = (Int) goalstk;         /* DS: added due to bug in M68000 compiler, */
			    /* DS: the instruction "cmpl #_goalstk,#_goalstk+8000" does not exist! */
		if( temp > (Int) (goalstk+GSTKLEN)  /* DS: temp was goalstk */
			|| !IsComp(goalstk+GSTKLEN))
			printf("Stack address out of range: reduce size?\n");
		printf("Debug flag value (eg. 127) ? ");
		{ int tempdebug;
		scanf("%d", &tempdebug);
		debug = tempdebug;
		}
	}
	if (argc <= MAXARGC)
		gargc = argc;
	else
		gargc = MAXARGC;
	for (argc = 0; argc < gargc; argc++)
		gargv[argc] = argv[argc];
#if 1
	pfiles[0] = stdin;
	pfiles[1] = stdout;
	pfiles[2] = stderr;
	pfiles[3] = stdin;
	pfiles[4] = stdout;
#endif
	initdict();
	if(signal(SIGFPE, SIG_IGN) != SIG_IGN)
		signal(SIGFPE, onfpe);
	t = stot("$startup. ");
	gtop = goalstk;
	g_parent(gtop) = (Int) NULL;
	g_nextchild(gtop) = (Int) t;
	rsetend = (Ptr)gbindings(gtop);
	ndelay = 0;
	mainloop();
}

Ptr	f;

mainloop(){			/* main loop of interpreter - handles goalstk*/
	register Int *proc;
	register Ptr call;
	register levtype clev;
	Ptr c1;
	levtype l1;
	Int td;			/* (jws) */

	nextgoal = gtop;
	woken = newmem(2);
	l_next(woken) = NULL;	/* sentinel for addbind */
/********/		/* find next subgoal to call, grow stack, init vars */
getcall:
/********/
		{ register Int* p;

		p = nextgoal;
		while(asleep(p))
			p = gprev(p);
		while(!gnextchild(p)) {
			findsg:
			if(resumed(p))
				p = gnextwake(p);
			else
				p = gparent(p);
		}
		if(delayed(p)) {	/* we are resuming a delayed call */
			ndelay--;
			rsetend++;      /* create room for gnextwake */
			g_prev(rsetend) = ~GPTRMASK | (Int) gtop;
			gtop = rsetend;
			g_nextwake(gtop) = g_nextchild(p);
			g_parent(gtop) = g_parent(p);
			g_call(gtop) = g_call(p);
			proc = gproc(p);
			if(tdict(gcall(gtop)) == Dcomma) {
				cont = (Ptr) targ(2, gcall(gtop));
				findbind((Ptr)targ(1,gcall(gtop)),
					gbindings(gparent(p)), &c1, &l1);
			} else {
				cont = NULL;
				findbind(gcall(gtop), gbindings(gparent(p)),
					&c1, &l1);
			}
			call = c1;
			clev = l1;
		} else {
			if(IsAtom(gnextchild(p)) &&
					atdict(gnextchild(p)) == Dtrue)
				goto findsg;
			g_prev(rsetend) = (Int) gtop;
			gtop = rsetend;
			g_call(gtop) = g_nextchild(p);
			g_parent(gtop) = (Int) p;
			if(tdict(gcall(gtop)) == Dcomma) {
				cont = (Ptr) targ(2, gcall(gtop));
				findbind((Ptr)targ(1,gcall(gtop)),
					gbindings(p), &c1, &l1);
			} else {
				cont = NULL;
				findbind(gcall(gtop), gbindings(p),
					&c1, &l1);
			}
			call = c1;
			clev = l1;
			td = tdict(call);
			if(!IsFunc(call)) {
				plerror(EEFUNC);
				proc = d1clause(DERROR);
			} else if(dproc(td) == (Ptr) null) {
				plerror(ENOPROC);
				proc = d1clause(DERROR);
			} else {
#ifdef CLINDEX
				if(dclindex(td) != NULL) {
					register Int di;	/* index key */
					Ptr x;			/* tempories */
					levtype l;

					proc = dclindex(td);
					di = targ(cln(proc), call);
					findbind((Ptr)di, clev, &x, &l);
					di = (Int) x;
					if(IsVar(di)) {
						proc = d1clause(td);
					} else {
						if(IsComp(di))
							di = thead(di);
						for(proc = clnext(proc);
							proc != NULL; proc = clnext(proc))
							if(clpred(proc) == di)
								break;
						if(proc != NULL)
							proc = clproc(proc);
						else {
							/* key doesn't match anything */
							proc = clproc(dclindex(td));
						}
					}
				} else
#endif
					proc = d1clause(td);
				if(!proc) {
					g_proc(gtop) = (Int) null;
					goto backtrack;
				}
			}
		}
		}
		if(rsetend > hbot-1000 && plerrnum != ENOMEM
					&& plerrnum != EABORT) {
			plerror(ENOMEM);
			proc = d1clause(DERROR);
		}
		goto try;

/********/		/* current clause fails: try next one or backtrack */
fail:
/********/
		/*	reset(grset(gtop));	/* now open coded */
		{
		register Int *rsetstart= grset(gtop);
		register Int *rend=rsetend;
		for(rend = rend-2; rend>= rsetstart; rend = rend-2)
			switch(*rend) {
			case RSTRUCT:
				dispmem(2, (Ptr)*(rend+1));
				break;
			case RTERM:
				dispterm((Ptr) *(rend+1));
				break;
#if 0 /*def DBASE*/
			case RDB:
				mudd_abort(*(rend+1));
				break;
#endif
			default:
				*((Int *)*rend) = *(rend+1);
			}
		rsetend = rend;
		}

		if(proc = cnextc(proc))
			goto try;
		/* goto backtrack */

/********/		/* no more clauses: pop stack etc */
backtrack:
/********/
		/*do {*/
if(debug & 2) {
findbind(gtcall(gtop), gbindings(gparent(gtop)),&c1, &l1);
if(tdict(c1) != Dcomma) {
printf("FAIL ");
printt(stdout,gtcall(gtop), gbindings(gparent(gtop)));
printf("\n");
} }
			if(!resumed(gtop)) {
				g_nextchild(gparent(gtop)) = g_call(gtop);
				rsetend = gtop;
			} else {
				ndelay++;
				rsetend = gtop - 1;
			}
			gtop = gprev(gtop);
			/*	reset(grset(gtop));	/* open coded */
			{
			register Int *rsetstart= grset(gtop);
			register Int *rend=rsetend;
			for(rend = rend-2; rend>= rsetstart; rend = rend-2)
				switch(*rend) {
				case RSTRUCT:
					dispmem(2, (Ptr)*(rend+1));
					break;
				case RTERM:
					dispterm((Ptr) *(rend+1));
					break;
#if 0 /*def DBASE*/
				case RDB:
					mudd_abort(*(rend+1));
					break;
#endif
				default:
					*((Int *)*rend) = *(rend+1);
				}
			rsetend = rend;
			}
			if(beencut(gproc(gtop))) {
				proc = NULL;
				g_proc(gtop) = uncut(gproc(gtop));
			} else
				proc = cnextc(gproc(gtop));
			if((Ptr) gproc(gtop) != null &&
				!--c_nlinks((Ptr)gproc(gtop))) {
				dispterm(cclause((Ptr) gproc(gtop)));
				dispmem(CCLSZ, gproc(gtop));
			}
		if(delayed(gtop)) {
			ndelay--;
			goto backtrack;
		}
		if(!proc)
			goto backtrack;
		/* end of loop */
		nextgoal = gtop;
		if(tdict((Ptr)gcall(gtop)) == Dcomma) {
			cont = (Ptr) targ(2, (Ptr)gcall(gtop));
			findbind((Ptr)targ(1,(Ptr)gcall(gtop)), gbindings(
				gparent(gtop)), &c1, &l1);
		} else {
			cont = NULL;
			findbind((Ptr) gcall(gtop), gbindings(
				gparent(gtop)), &c1, &l1);
		}
		call = c1;
		clev = l1;
		/* goto try */

/********/		/* try clause: init bindings and unify/call system */
try:			/* pred */
/********/
		if(intflag) {
			if(plerrnum != ENOMEM)
				plerror(EINTR);
			else
				plerror(EABORT);
			proc = d1clause(DERROR);
			intflag = 0;
/*
			signal(SIGINT, onintr);	/* not needed for UNIX 4.2 */
/**/
		}
		g_rset(gtop) = (Int) grsetlist(gtop, proc);
		rsetend = grset(gtop);
		{register levtype b;
		for( b = gbindings(gtop); b < (levtype) rsetend; b++) {	
			b->btermp = (Ptr) NULL;
			b->blev = (levtype) NULL;
		}
		}
if(debug & 1 && tdict(call) != Dcomma) {
printf("TRY  ");
printt(stdout,call,clev);
printf("\n");
}
		if(lnext(woken)) {
			displist(2, lnext(woken));
			l_next(woken) = NULL;
			litem(woken) = 0;    /* sentinel for addbind */
		}
		if(cisfunc(proc)) {
			if(tdict(call) == Dsave)
				savemain();
			f = NULL;
			switch(cfunc(proc)(call, clev)) {
			case SUCCEED:	goto succeed;
			case FAIL:	goto fail;
			case DELAY:	goto delay;
			case ERROR:
					reset(grset(gtop));
					proc = d1clause(DERROR);
					goto try;
			case CUTFAIL:
					proc = null;
					goto fail;
			case DELPREV:
					nextgoal = gtop;
					cont = gnextchild(gparent(gtop));
					proc = gproc(gtop);
					c_nlinks(proc)--;
					goto delay;
			case RESTORE:
					restmain();
					goto succeed;
			}
		} else {
			f = (Ptr) targ(2, cclause(proc));
						/* unification */
			{ register int wait;
			{ register Ptr h, c;
			  register int n;
			  register levtype hl;

			h = chead(proc);
			hl = gbindings(gtop);
			c = call;
			if(IsVar(h)) {
				addbind(h, hl, c, clev);
				goto succeed;
			}
			if(IsAtom(c))
				goto succeed;
			n = tnargs(c);
			wait = 0;
			while(n-- > 0) {
				waitflag = 0;
				if(!unify((Ptr)*++h, hl, (Ptr)*++c, clev))
{
					goto fail;
}
				wait =  (wait << 1) + waitflag;
			}
			}
			{ register Ptr w;

			if(!(w = dwait(tdict(call))))
				goto succeed;
			while(w)
				if(!(wait & *(w+1)))
					goto succeed;
				else
					w = (Ptr) *w;
			}
			}
		}
		/* goto delay */

/********/		/* call delays: store more info on stack, make delay */
delay:			/* fudge (mark vars etc.), goto getcall */
/*******/
if(debug & 4) {
printf("DELAY  ");
printt(stdout,call, clev);
printf("\n");
}
		ndelay++;
		if(!resumed(gtop))
			g_nextchild(gparent(gtop)) = (Int) cont;
		else
			nextgoal = gnextwake(gtop);
		if(proc != null)
			c_nlinks(proc)++;
		g_proc(gtop) = (Int) proc;
		delayp(gtop);
		goto getcall;
/********/		/* call succeeded: store more info on stack, wake any */
succeed:		/* delayed procs, goto getcall */
/********/
		if(!resumed(gtop))
			g_nextchild(gparent(gtop)) = (Int) cont;
		wakeprocs(&nextgoal, gtop);
		g_nextchild(gtop) = (Int) f;
		if(proc != null)
			c_nlinks(proc)++;
		g_proc(gtop) = (Int) proc;
		goto getcall;
}

int
onintr()				/* called on interrupts */
{
	extern jmp_buf intrbuf;
	extern int doljump;

	intflag = 1;
	if (doljump)
		longjmp(intrbuf, 1);
}
