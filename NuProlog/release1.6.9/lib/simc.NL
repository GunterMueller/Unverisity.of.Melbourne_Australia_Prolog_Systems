/*
 * simc.nl  -  external database interface for SIMC
 *
 * Copyright (C) 1986, The University of Melbourne
 */

%## $simc_newrel('LIB/db.NUVERSION/simc/newrel').
$simc_newrel('/mip/jas/lib/nuprolog/db/simc/newrel').
%## $simc_dbparams('LIB/db.NUVERSION/simc/dbparams simc').
$simc_dbparams('/mip/jas/lib/nuprolog/db/simc/dbparams simc').

%
% get answers to a query, one at a time
%
$simc_retrieve(Db, Query) :-
	termToString([cons,string,quote,prefix], Query, QString),
	$simc_query(Db, QString, 4, QDesc),
	$oncut(2,QDesc),
	$simc_retrieve1(QDesc, Query).

$simc_retrieve1(QDesc, Query):-
	repeat,
	$simc_next(QDesc, Ans),
%	$pstot(Ans, Answer),
	sread(Ans, Answer),
	(	Answer = dbEnd,
		$simc_end(QDesc),
		!,
		fail
	;	% check for valid match (not false match)
		% set up answer bindings
		Query = Answer
	).

%
% $simc_insert:
%	add a new record to the database
%
$simc_insert(D, Fact) :-
	termToString([cons,string,quote,prefix], Fact, Fstring),
	!,
	$simc_assert(D, Fstring).

%
% simc_retract:
%	delete matching records one by one from database
%
$simc_retract(Db, Query) :-
	termToString([cons,string,quote,prefix], Query, Qstring),
	$simc_query(Db, Qstring, 1, Qdesc),
	$oncut(2,Qdesc),
	$simc_retract1(Qdesc, Query).

$simc_retract1(Qdesc, Query) :-
	repeat,
	$simc_next(Qdesc, Ans),
	sread(Ans, Answer),
	(	Answer = dbEnd,
		$simc_end(Qdesc),
		!,
		fail
	;	% check for valid match (not false match)
		Query = Answer,
		$simc_delete(Qdesc)
	).

$simc_rall(D, P) :-
	$simc_retract(D, P), fail.
$simc_rall(_, _).
/*
%
% simc_rall:
%	delete all matching records from database
%	invoked by ``retractall(DB_relation(args)).''
%
$simc_rall(D, P) :-
	termToString([cons,string,quote,prefix], P, P1),
	$simc_query(D, P1, 1, F),
	$oncut(2,F),
	$simc_rall1(F, P).

$simc_rall1(F, P) :-
	repeat,
	$simc_next(F, Q1),
	append(Q2,".",Q1),
	sread(Q2, Q),
	(	Q = dbEnd
	;	P = Q,
		$simc_delete(F),
		fail
	),
	$simc_end(F).
*/

%
% Statistics gathering
%
%simc_count(0).	
%?-$retract((simc_count(0) :- true)).

%
% $simc_define:
%	create a clean, new relation by calling the "newrel" program
%	"newrel Db Rel simc Br Bs Kr Ks Ur Us Nr Nseg Ndata Avrec"
%
$simc_define(Db, Functor, Arity) :-
	dbParam(Db, Functor, Arity, br = Br),
	dbParam(Db, Functor, Arity, bs = Bs),
	dbParam(Db, Functor, Arity, kr = Kr),
	dbParam(Db, Functor, Arity, ks = Ks),
	dbParam(Db, Functor, Arity, ur = Ur),
	dbParam(Db, Functor, Arity, us = Us),
	dbParam(Db, Functor, Arity, nr = Nr),
	dbParam(Db, Functor, Arity, nseg = Nseg),
	dbParam(Db, Functor, Arity, ndata = Ndata),
	dbParam(Db, Functor, Arity, avrec = Avrec),
	dbParam(Db, Functor, Arity, template = TemplateL),
	name(Template, TemplateL),
%	Nseg1 is Nseg + 1,		% BUG!  Unique variable.
	$simc_newrel(Newrel),
	$sprintf(Cmd,
		"% '%' '%' % % % % % % % % % % % '%'",
		[	Newrel, Db, Functor, simc, Br, Bs, Kr, Ks, Ur, Us,
			Nr, Nseg, Ndata, Avrec, Template]
		),
%dbug	writeln(user, Cmd),
	system(Cmd, 0).

%
% $simc_undefine:
%	Remove a relation
%
$simc_undefine(Db, R) :-
	append("rm -rf ", Db, S1),
	name(R, RS),
	append(S1, 0'/.RS, Cmd),
%dbug	writeln(user, Cmd),
	system(Cmd, 0).
