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
predu.c
various UNIX functions */

#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "types.h"
#include "dict.h"
#include "pred.h"

extern getpwnam();

p_system(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	int i;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	if(!IsAtom(t1)) {
		plerror(EECONST);
		return(ERROR);
	}
	i = system(dname(atdict(t1)));
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t) == TVAR) {
		nbind(t, l, ConsInt(i));
		return(SUCCEED);
	} else if(IsFunc(t) || tnum(t) != i)
		return(FAIL);
	else
		return(SUCCEED);
}

p_argv(t, l)
	Ptr t;
	levtype l;
{
	Int arg, *r;

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(gargc <= 1)
		addbind(t, l, (Ptr)ConsFunc(Dnil, 0), NULLL);
	else {
		r = rsetend;
		for(arg = 1; arg < gargc; arg++) {
			thead(r) = ConsFunc(Ddot, 2);
			targ(1, r) = ConsFunc(find(1, gargv[arg]), 0);
			targ(2, r) = (Int) (r + 3);
			r = (Int *) (r + 3);
		}
		targ(2, r-3) = ConsFunc(Dnil, 0);
		r = rsetend;
		rsetend += gargc*3 - 2;
		g_rset(gtop) = (Int) rsetend;
		addbind(t, l, r, l);
	}
	return(SUCCEED);
}

/* returns total cpu secs used by process so far */
/* units are 1000th's of a second */
p_cputime(t, l)
	Ptr t;
	levtype l;
{
	struct rusage rbuf;
	long usrtime, systime;
	int cpusecs;

	getrusage(RUSAGE_SELF, &rbuf);
	usrtime = rbuf.ru_utime.tv_sec*1000 + rbuf.ru_utime.tv_usec/1000;
	systime = rbuf.ru_stime.tv_sec*1000 + rbuf.ru_stime.tv_usec/1000;
	cpusecs = (int)usrtime + (int)systime;
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	nbind(t, l, ConsInt(cpusecs));
	return(SUCCEED);
}

p_getuid(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	nbind(t, l, ConsInt(getuid()));
	return(SUCCEED);
}

p_isuser(t, l)
	Ptr t;
	levtype l;
{
#define MAXIDLEN 10
	char buf[MAXIDLEN];

	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttos(t, l, buf, MAXIDLEN) != SUCCEED) {
		plerror(EESTR);
		return(ERROR);
	}
	if(getpwnam(buf) != NULL)
		return(SUCCEED);
	else
		return(FAIL);
}

#ifdef PRV
p_privilege(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	if(testpriv(getuid(), tnum(t)))
		return(SUCCEED);
	else
		return(FAIL);
}

#include <sys/types.h>
#include <udata.h>

testpriv(uid,priv)
int uid;
int priv;
{

switch (priv) {
	case 1:
		return(testupriv(uid,STUDENT));
	case 2:
		return(testupriv(uid,TUTOR));
	case 3:
		return(testupriv(uid,SUPER_TUTE));
	default:
		return(0);
	}
}
#endif

extern onintr();

p_sig(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;

	findbind((Ptr)targ(1, t), l, &t1, &l1);
	findbind((Ptr)targ(2, t), l, &t, &l);
	if(ttype(t1) != TNUM || ttype(t) != TNUM) {
		plerror(EEINT);
		return(ERROR);
	}
	switch(tnum(t)) {
	case 0:
		signal((int)XtrInt(t1), SIG_DFL);
		break;
	case 1:
		signal((int)XtrInt(t1), SIG_IGN);
		break;
	case 2:
		signal((int)XtrInt(t1), onintr);
		break;
	default:
		plerror(EUINT);
		return(ERROR);
	}
	return(SUCCEED);
}

p_fork(t, l)
	Ptr t;
	levtype l;
{
	if(fork())
		return(SUCCEED);
	else
		return(FAIL);
}
