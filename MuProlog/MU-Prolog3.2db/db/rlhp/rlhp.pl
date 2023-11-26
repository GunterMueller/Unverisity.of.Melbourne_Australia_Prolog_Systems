/************************************************************************
*									*
*			MU-PROLOG DATABASE				*
*			========= ========				*
*									*
* (C) Copyright 1985 Lee Naish, James Thom, Melborne University		*
*									*
*	Written by Lee Naish & James Thom				*
*	Department of Computer Science,					*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

% specify the location of a binary

?-hide($rlhp_newrel(1)).
%##$rlhp_newrel('LIB/db/rlhp/newrel').
$rlhp_newrel('/usr/lib/prolog/db/rlhp/newrel').

%load the recursive linear hashing scheme (with pipes)

?-use_if dynamic_loading.

%##?-dload(['LIB/dload/rlhp.o','LIB/db/rlhp/dblib'],
?-dload(['/usr/lib/prolog/dload/rlhp.o','/usr/lib/prolog/db/rlhp/dblib'],
	' ',
	['_p_rlhp_query',	'_p_rlhp_end',		'_p_rlhp_retract',
	'_p_rlhp_rall', 	'_p_rlhp_assert',	'_p_rlhp_abort'],
	[$rlhp_query(_,_,_),	$rlhp_end(_),		$rlhp_retract(_,_),
	$rlhp_rall(_,_),	$rlhp_assert(_,_),	$rlhp_abort(_)]).

?-use_end.

?-	$unprotect($db_assert(1, 1, 1)),
	$unprotect($db_retract(1, 1, 1)),
	$unprotect($db_retractall(1, 1, 1)),
	$unprotect($db_define(1, 1, 1, 1)),
	$unprotect($db_undefine(1, 1, 1)),
	$unprotect($db_setup(1, 1, 1)).

?-assert(($db_assert(D, R, rlhp) :-
	$rlhp_assert(D, R))).

?-assert(($db_retract(D, R, rlhp) :-
	$rlhp_retract(D, R))).

?-assert(($db_retractall(D, R, rlhp) :-
	$rlhp_rall(D, R))).

?-assert(($db_define(D, R, N, rlhp(Args)) :-
	$rlhp_define(D, R, N, Args))).

?-assert(($db_undefine(DS, R, rlhp) :-
	$rlhp_undefine(DS, R))).

?-assert(($db_setup(D, R, rlhp):-
	assert((R :- $rlhp_retrieve(D, R))))).

$rlhp_retrieve(D, P) :-		% get answers to a query from external db
	$db_onabort($rlhp_abort(F)),
	$rlhp_query(D, P, F),
	repeat,
	read(F, Q),
	(	Q = (?-E),
		(	E = db_end,
			$rlhp_end(F),
			!,
			fail
%		;		 
%			E = db_end(N),
%			(	rlhp_count(N)
%			;
%				true
%			),
%			$rlhp_end(F),
%			!,
%			fail
		;		 
			E = db_error(N),
			write(N),
			$rlhp_emess(N, M),
			write(' : '),
			writeln(M),
			fail
		;
			E = end,
			writeln('db_end expected'),
			!,
			fail
		)
	;
		P = Q
	).

%rlhp_count(0).				% for taking stats
%?-$ret((rlhp_count(0) :- true)).

$rlhp_emess(exec, 'mudd: cannot execute') :-
	!.
$rlhp_emess(open, 'cannot open for reading') :-
	!.
$rlhp_emess(nkeys, 'wrong number of keys in database') :-
	!.
$rlhp_emess(X, 'Unknown database error').


$rlhp_define(DS, Rname, N, Args) :-		% create new relation
	$argstos(N.Args, Argss),		% convert args to a string
	name(Rname, Rs),
	$rlhp_newrel(NR),
	name(NR, NRS),
	$app(NRS, 32.DS, S),
	$app(S, 32.Rs, S1),
	$app(S1, 32.Argss, Cmd),
	shell(Cmd).			% call newrel with arg string

$rlhp_undefine(DS, R) :-		% remove relation
	$app("rm ", DS, S1),		% rm dbname/relname dbname/.relname
	Sl is "/",
	name(R, RS),
	$app(S1, Sl.RS, S2),
	$app(S2, 32.DS, S3),
	Sd is ".",
	$app(S3, Sl.Sd.RS, S4),
	shell(S4).

?- protect([
	$db_assert(3),
	$db_retract(3),
	$db_retractall(3),
	$db_define(4),
	$db_undefine(3),
	$db_setup(3),
	$rlhp_retrieve(2),
	$rlhp_emess(2),
	$rlhp_define(4),
	$rlhp_undefine(2)]).
?-hidden.
