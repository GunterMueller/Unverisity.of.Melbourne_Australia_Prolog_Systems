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

#include "types.h"
#include "dict.h"
#include "pred.h"
#include "init.h"
char *strcat(), *strcpy();

int
nextsch(sp)			/* return next char in string and inc. ptr */
	char **sp;
{
	return(*(*sp)++);
}

Ptr
stot(s)				/* convert a string to a term (return ptr) */
	char *s;
{
	return(getterm(nextsch,(int **) &s));
}



initdict(){			/* initialize dictionary and clauses */
	Ptr a;
	int fd;
	char *rfile, buf[100];

	gtop = goalstk;
	hbot = goalstk + GSTKLEN,
	rsetend = goalstk;	/* needed for getterm, nassert to grab core */
				/* some symbols refered to in sys functs */
				/* must be included here, at the start */
#include "mdict.h"
	fassert(find(2, "restore"), (Ptr) p_restore, -1, 0);
	fassert(find(2,"exit"), (Ptr) p_exit, -1, 0);
	if(gargc > 1) {
		FILE *f;

		rfile = gargv[1];
#ifdef DLOAD
#if (pyr | vax) & !interdata
				/* For restoring files with dynamically */
				/* loaded functions in, it seems to be a */
				/* good idea to allocate an I/O buffer */
				/* or two on some systems. (megafudge) */
		if((f = fopen("/dev/null", "r"))) {
			getc(f);
			fclose(f);
		}
#endif
#endif
	} else {
#ifdef DBASE
		printf("MU-Prolog %gdb\n\n", VERSION/1000.0);
#else
#ifdef NEWDBASE
		printf("MU-Prolog %gddb\n\n", VERSION/1000.0);
#else
		printf("MU-Prolog %g\n\n", VERSION/1000.0);
#endif
#endif
		rfile = PRFILE;
	}
	if((fd = open(rfile, 0)) != -1) {	/* restore saved state */
		close(fd);
		strcpy(buf, ":-($startup, restore('");
		strcat(buf, rfile);
		strcat(buf, "')). ");
		fassert(find(1, "$startup"), stot(buf), 0, 0);
		fassert(find(1, "$startup"),
				stot(":-($startup, exit(1)). "), 0, 1);
		return;
	} else if(gargc > 1) {
		fprintf(stderr, "file not found\n");
		exit(1);
	}
	printf("file plstate not found\n");
	printf("******** bootstrapping ********\n");
			/* the following line, for example, adds nassert */
			/* the 3 means it has 3-1=2 arguments; p_nassert is */
			/* the C function in place of a clause; -1 is the */
			/* nvars field value and signifies a C function; 0 */
			/* means that no loop is put at the end of the proc */
			/* (see repeat below, fassert code) */
	fassert(find(3, "$nassert"), (Ptr) p_nassert, -1, 0);
			/* the stot function converts a string to a term */
			/* it does not "compile" the variables so if more */
			/* than one variable is in the clause use p_asrt */
	p_addop(a=stot("op(1200, xfy, ':-').  "), gbindings(goalstk));
	dispterm(a);
	p_addop(a=stot("op(1000, xfy, ',').  "), gbindings(goalstk));
	dispterm(a);
	p_addop(a=stot("op(1100, xfx, ';').  "), gbindings(goalstk));
	dispterm(a);
	fassert(find(2, "libdirectory"), stot(PLLIBDIR), 0, 0);
	fassert(find(1, "repeat"), stot("repeat :- true.  "), 0, 1);
	fassert(find(3, "$nretract"), (Ptr) p_nretract, -1, 0);
	fassert(find(2, "$retwait"), (Ptr) p_retwait, -1, 0);
	fassert(find(2, "$hide"), (Ptr) p_hide, -1, 0);
	fassert(find(1, "hidden"), (Ptr) p_hidden, -1, 0);
	fassert(find(2, "$protect"), (Ptr) p_protect, -1, 0);
	fassert(find(2, "$unprotect"), (Ptr) p_unprot, -1, 0);
#ifdef CLINDEX
	fassert(find(3, "$clindex"), (Ptr) p_clindex, -1, 0);	/* (jws) */
#endif
	fassert(find(2, "$ret"), (Ptr) p_1retract, -1, 0);
	fassert(find(2, "$ret"), (Ptr) p_2retract, -1, 1);
	fassert(find(3, "$proc"), (Ptr) p_1proc, -1, 0);
	fassert(find(3, "$proc"), (Ptr) p_2proc, -1, 1);
	fassert(find(4, "$nclause"), (Ptr) p_1nclause, -1, 0);
	fassert(find(4, "$nclause"), (Ptr) p_2nclause, -1, 1);
	fassert(find(2, "$wlist"), (Ptr)p_wlist, -1, 0);
	fassert(find(3,"write"), (Ptr) p_write, -1, 0);
	fassert(find(4,"write"), (Ptr) p_write, -1, 0);
	fassert(find(2,"wflags"), (Ptr) p_wflags, -1, 0);
	fassert(find(4, "fprintf"), (Ptr) p_fprintf, -1, 0);
	fassert(find(2,"$pcons"), (Ptr) p_pcons, -1, 0);
	fassert(find(2,"$debug"), (Ptr) p_debug, -1, 0);
	fassert(find(3,"putc"), (Ptr) p_putc, -1, 0);
	fassert(find(3,"getc"), (Ptr) p_getc, -1, 0);
	fassert(find(4,"open"), (Ptr) p_open, -1, 0);
	fassert(find(3,"pipe"), (Ptr) p_pipe, -1, 0);
	fassert(find(3,"$fmove"), (Ptr) p_fmove, -1, 0);
	fassert(find(3,"$fcopy"), (Ptr) p_fcopy, -1, 0);
	fassert(find(2,"close"), (Ptr) p_close, -1, 0);
	fassert(find(1,"fork"), (Ptr) p_fork, -1, 0);
	fassert(find(4,"$addop"), (Ptr) p_addop, -1, 0);
	fassert(find(3,"read"), (Ptr) p_read, -1, 0);
	fassert(find(2,"wait"), (Ptr) p_wait, -1, 0);
	fassert(find(2,"int"), (Ptr) p_int, -1, 0);
	fassert(find(2,"integer"), (Ptr) p_integer, -1, 0);
	fassert(find(2,"var"), (Ptr) p_var, -1, 0);
	fassert(find(2,"$is_"), (Ptr) p_is_, -1, 0);
	fassert(find(2,"nonvar"), (Ptr) p_nonvar, -1, 0);
	fassert(find(2,"atom"), (Ptr) p_atom, -1, 0);
	fassert(find(2,"atomic"), (Ptr) p_atomic, -1, 0);
	fassert(find(3, "occurs"), (Ptr) p_occurs, -1, 0);
	fassert(find(2,"$nground"), (Ptr)p_ngrnd, -1, 0);
	fassert(find(2,"$ndelay"), (Ptr) p_ndelay, -1, 0);
	fassert(find(1,"!"), (Ptr) p_cut, -1, 0);
	fassert(find(1,"$lcut"), (Ptr) p_lcut, -1, 0);
	fassert(find(1,"$soft_cut"), (Ptr) p_soft_cut, -1, 0);
	fassert(find(1,"abort"), (Ptr) p_abort, -1, 0);
	fassert(find(1,"$pasleep"), (Ptr) p_pasleep, -1, 0);
	fassert(find(2, "backtrace"), (Ptr) p_backtrace, -1, 0);
	fassert(find(3, "ancestor"), (Ptr) p_ancestor, -1, 0);
	fassert(find(2, "depth"), (Ptr) p_depth, -1, 0);
	fassert(find(2,"$errnum"), (Ptr) p_errnum, -1, 0);
	fassert(find(3,"~="), (Ptr) p_tne, -1, 0);
	fassert(find(5,"$is_eq"), (Ptr) p_is_eq, -1, 0);
	fassert(Dis, (Ptr) p_is, -1, 0);
	fassert(Deeq, (Ptr) p_relop, -1, 0);
	fassert(Dene, (Ptr) p_relop, -1, 0);
	fassert(Dlt, (Ptr) p_relop, -1, 0);
	fassert(Dle, (Ptr) p_relop, -1, 0);
	fassert(Dgt, (Ptr) p_relop, -1, 0);
	fassert(Dge, (Ptr) p_relop, -1, 0);
	fassert(Dland, (Ptr) p_relop, -1, 0);
	fassert(Dlor, (Ptr) p_relop, -1, 0);
	fassert(find(4, "plus"), (Ptr) p_plus, -1, 0);
	fassert(find(4,"$functor"), (Ptr) p_functor, -1, 0);
	fassert(find(3,"=.."), (Ptr) p_univ, -1, 0);
	fassert(find(3,"name"), (Ptr) p_name, -1, 0);
	fassert(find(2,"save"), (Ptr) p_save, -1, 0);
	fassert(find(3, "system"), (Ptr) p_system, -1, 0);
	fassert(find(3, "sig"), (Ptr) p_sig, -1, 0);
	fassert(find(2,"exit"), (Ptr) p_exit, -1, 0);
	fassert(find(2,"argv"), (Ptr) p_argv, -1, 0);
	fassert(find(2,"getuid"), (Ptr) p_getuid, -1, 0);
	fassert(find(2,"cputime"), (Ptr) p_cputime, -1, 0);
	fassert(find(2,"isuser"), (Ptr) p_isuser, -1, 0);
			/* PROLOG system functions (in C) may be called given*/
			/* a term and a level. Level should be NULL in */
			/* initialization, otherwise bindings will be looked */
			/* for if there are any variables */
	p_nassert(a=stot("nassert((=(X,X) :- true), 1).  "), gbindings(goalstk));
	dispterm(a);
	p_nassert(a=stot("nassert((X, Y :- X, Y), 1).  "), gbindings(goalstk));
	dispterm(a);
	p_nassert(a=stot("nassert((X ; Y :- X), 1).  "), gbindings(goalstk));
	dispterm(a);
	p_nassert(a=stot("nassert((X ; Y :- Y), 2).  "), gbindings(goalstk));
	dispterm(a);
				/* fudge for error handling */
			/* ERROR is used as a procedure so unlike other vars, */
			/* it needs the extra info in the dictionary */
	rfile = dname(DERROR);			/* save ptr to name */
	dispmem(DVNTRYSZ, dict[DERROR]);  /* reclaim memory */
	dict[DERROR] = newmem(DENTRYSZ);	/* grab a bigger chunk */
	d_wait(DERROR) = NULL;			/* initialize it */
	d_proc(DERROR) = (Int) null;
#ifdef CLINDEX
	d_clindex(DERROR) = NULL;	/* (jws) */
#endif
	d_prprec(DERROR) =
	d_inprec(DERROR) =
	d_psprec(DERROR) = -1;
	d_nlinks(DERROR) = 1;
	d_prot(DERROR) = 0;
	d_name(DERROR) = (Int) rfile;
	d_nargs(DERROR) = 0;			/* now assert clause */
	fassert(DERROR, stot("ERRCALL :- $err(ERRCALL).  "), 1, 0);
#ifdef PRV
	fassert(find(2,"privilege"), (Ptr) p_privilege, -1, 0);
#endif
#ifdef DBASE
#ifdef SQL
	fassert(find(4, "$sql_query"), (Ptr) p_sql_query, -1, 0);
	fassert(find(3, "$sql_next"), (Ptr) p_sql_next, -1, 0);
	fassert(find(2, "$sql_end"), (Ptr) p_sql_end, -1, 0);
	fassert(find(3, "$sql_modify"), (Ptr) p_sql_modify, -1, 0);
	fassert(find(2, "$sql_abort"), (Ptr) p_sql_abort, -1, 0);
#endif

	fassert(find(4, "$simc_query"), (Ptr) p_simc_query, -1, 0);
/*	Future History
	==============
	fassert(find(5, "$simc_sfbquery"), (Ptr) p_simc_sfbquery, -1, 0);
*/
	fassert(find(3, "$simc_next"), (Ptr) p_simc_next, -1, 0);
	fassert(find(2, "$simc_end"), (Ptr) p_simc_end, -1, 0);
	fassert(find(3, "$simc_assert"), (Ptr) p_simc_assert, -1, 0);
	fassert(find(2, "$simc_delete"), (Ptr) p_simc_delete, -1, 0);
	fassert(find(2, "$simc_abort"), (Ptr) p_simc_abort, -1, 0);

	fassert(find(4, "$rlhp_query"), (Ptr) p_rlhp_query, -1, 0);
	fassert(find(2, "$rlhp_end"), (Ptr) p_rlhp_end, -1, 0);
	fassert(find(3, "$rlhp_retract"), (Ptr) p_rlhp_retract, -1, 1);
	fassert(find(3, "$rlhp_rall"), (Ptr) p_rlhp_rall, -1, 0);
	fassert(find(3, "$rlhp_assert"), (Ptr) p_rlhp_assert, -1, 0);
	fassert(find(2, "$rlhp_abort"), (Ptr) p_rlhp_abort, -1, 0);

/*	Ancient History
	===============
	fassert(find(2, "$dbnewrel"), stot(PLNEWREL), 0, 0);
	fassert(find(4,"$dbquery"), (Ptr) p_dbquery, -1, 0);
	fassert(find(2, "$db_end"), (Ptr) p_db_end, -1, 0);
	fassert(find(3, "$dbassert"), (Ptr) p_dbassert, -1, 0);
	fassert(find(3, "$dbretract"), (Ptr) p_dbretract, -1, 1);
	fassert(find(3, "$dbretractall"), (Ptr) p_dbrall, -1, 0);
	fassert(find(2, "$rlhp0_abort"), (Ptr) p_mudd_abort, -1, 0);
*/
	fassert(find(1, "database"), stot("database :- true.  "), 0, 0);
#else
#ifdef NEWDBASE
	fassert(find(1, "database"), stot("database :- true.  "), 0, 0);
#else
	fassert(find(1, "database"), stot("database :- fail.  "), 0, 0);
#endif
#endif
#ifdef DLOAD
	fassert(find(5, "dload"), (Ptr) p_dload, -1, 0);
	fassert(find(1, "dynamic_loading"), stot("dynamic_loading :- true.  "), 0, 0);
#else
	fassert(find(1, "dynamic_loading"), stot("dynamic_loading :- fail.  "), 0, 0);
#endif
#ifdef PARAM
	fassert(find(3, "newparam"), (Ptr) p_newparam, -1, 0);
	fassert(find(3, "bind"), (Ptr) p_bind, -1, 0);
	fassert(find(2, "isparam"), (Ptr) p_isparam, -1, 0);
#endif
	fassert(find(1, "$startup"), stot(PLBOOTC), 0, 0);
}

fassert(d, fn, nvars, loop)	/* used to assert system functions and other */
				/* fudges: d = dict index; f = ptr to funct */
				/* (or clause); loop = flag to tell if an */
				/* "infinite loop" is required at the end of */
				/* the proc (eg. repeat) */
	int d;
	int nvars, loop;
	Ptr fn;
{
	Ptr c;

	d_prot(d) |= DPROT;
	if(d1clause(d) ==  null)
		d_1clause(d) = NULL;
	for(c = d0clause(d); cnextc(c); c = cnextc(c))
		;
	c_nextc(c) = (Int) newmem(CCLSZ);
	c = cnextc(c);
	if(loop) {
		c_nextc(c) = (Int) c;
		c_nlinks(c) = 2;
	} else {
		c_nextc(c) = NULL;
		c_nlinks(c) = 1;
	}
	c_nvars(c) = nvars;
	c_clause(c) = (Int) fn;
}

