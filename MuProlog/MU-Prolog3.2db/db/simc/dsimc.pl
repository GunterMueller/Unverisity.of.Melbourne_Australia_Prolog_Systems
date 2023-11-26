/************************************************************************
*									*
*			MU-PROLOG DATABASE				*
*			========= ========				*
*									*
* (C) Copyright 1985 Lee Naish, James Thom, Melbourne University	*
*									*
*	Written by Lee Naish & James Thom				*
*	Department of Computer Science,					*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

%##$dsimc_dbparams('LIB/db/simc/dbparams dsimc').
$dsimc_dbparams('/usr/lib/prolog/db/bin/dbparams dsimc').

%load the superimposed coding scheme

?-call($db_load(simc)).

?-	$unprotect($db_assert(1, 1, 1)),
	$unprotect($db_retract(1, 1, 1)),
	$unprotect($db_retractall(1, 1, 1)),
	$unprotect($db_define(1, 1, 1, 1)),
	$unprotect($db_undefine(1, 1, 1)),
	$unprotect($db_setup(1, 1, 1)).

?-assert(($db_assert(D, R, dsimc) :-
	$simc_assert(D, R))).

?-assert(($db_retract(D, R, dsimc) :-
	$simc_retract(D, R))).

?-assert(($db_retractall(D, R, dsimc) :-
	$simc_rall(D, R))).

?-assert(($db_define(D, R, N, dsimc(Args)) :-
	$dsimc_define(D, R, N, Args))).

?-assert(($db_undefine(DS, R, dsimc) :-
	$simc_undefine(DS, R))).

?-assert(($db_setup(D, R, dsimc):-
	assert((R :- $simc_retrieve(D, R))))).

% $dsimc_define
%
% call:
% newrel DS Rname dsimc `dbparams dsimc Av Arity Nrecs SegSz Nd` Skel)

$dsimc_define(DS, Rname, Arity, [Av,Nrecs,SegSz,Nd,Skel]) :-
	$argstos([Av,Arity,Nrecs,SegSz,Nd], Dbpargs),
	name(Rname, Rs),
	$simc_newrel(NR),
	$dsimc_dbparams(DBP),
	name(NR, NRS),
	name(DBP, DBPS),
	$app(NRS, 32.DS, S),
	$app(S, 32.Rs, S1),
	$app(S1, " dsimc `", S2),
	$app(S2, DBPS, S3),
	$app(S3, 32.Dbpargs, S4),
	$app(S4, "` ", S5),
	$app(S5, 34.Skel, S6),
	$app(S6, 34.[], Cmd),
	printf("%s%c",[Cmd,10]),
	shell(Cmd).

?- protect([
	$db_assert(3),
	$db_retract(3),
	$db_retractall(3),
	$db_define(4),
	$db_undefine(3),
	$db_setup(3),
	$dsimc_define(4),
	$dsimc_dbparams(1)]).
