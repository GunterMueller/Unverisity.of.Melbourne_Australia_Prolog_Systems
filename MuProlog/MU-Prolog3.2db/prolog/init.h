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
init.h
init.c header - includes declarations of all system functions
*/

extern	p_wlist()
	,p_nassert()
	,p_write()
	,p_pcons()
	,p_debug()
	,p_wait()
	,p_getc()
	,p_putc()
	,p_open()
	,p_close()
	,p_read()
	,p_2read()
	,p_int()
	,p_ngrnd()
	,p_tne()
	,p_cut()
	,p_lcut()
	,p_relop()
	,p_is()
	,p_plus()
	,p_addop()
	,p_ndelay()
	,p_errnum()
	,p_nretract()
	,p_1nclause()
	,p_2nclause()
	,p_functor()
	,p_abort()
	,p_backtrace()
	,p_1retract()
	,p_2retract()
	,p_fmove()
	,p_fcopy()
	,p_univ()
	,p_name()
	,p_retwait()
	,p_hide()
	,p_hidden()
	,p_protect()
	,p_unprot()
#ifdef CLINDEX
	,p_clindex()	/* (jws) */
#endif
	,p_1proc()
	,p_2proc()
#ifdef DBASE
#ifdef SQL
	,p_sql_query()
	,p_sql_next()
	,p_sql_end()
	,p_sql_modify()
	,p_sql_abort()
#endif

	,p_simc_query()
	,p_simc_sfbquery()
	,p_simc_next()
	,p_simc_end()
	,p_simc_assert()
	,p_simc_delete()
	,p_simc_abort()

	,p_rlhp_query()
	,p_rlhp_end()
	,p_rlhp_retract()
	,p_rlhp_rall()
	,p_rlhp_assert()
	,p_rlhp_abort()
/*
	,p_dbquery()
	,p_db_end()
	,p_dbassert()
	,p_dbretract()
	,p_dbrall()
	,p_mudd_abort()
*/
#endif
	,p_save()
	,p_restore()
	,p_system()
	,p_argv()
	,p_exit()
	,p_getuid()
	,p_cputime()
	,p_fprintf()
	,p_isuser()
#ifdef PRV
	,p_privilege()
#endif
	,p_integer()
	,p_var()
	,p_is_()
	,p_nonvar()
	,p_atom()
	,p_atomic()
	,p_sig()
	,p_wflags()
	,p_fork()
	,p_pipe()
	,p_ancestor()
	,p_depth()
	,p_soft_cut()	/* for NU-Negation */
	,p_is_eq()
#ifdef PARAM
	,p_newparam()
	,p_bind()
	,p_isparam()
#endif
	,p_occurs()
#ifdef DLOAD
	,p_dload()
#endif
	,p_pasleep()
	;
