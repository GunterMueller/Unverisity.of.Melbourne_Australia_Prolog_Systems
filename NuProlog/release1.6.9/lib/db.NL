/*
 * db.nl  -  external database interface predicates
 *
 * Copyright (C) 1986,1987, The University of Melbourne
 */

%	BUG!  $ondisc/3 needs to be changed to use the property lists
%	of the predicates involved, rather than being a dynamic relation.
%	See getclause/3.
%?- dynamic $ondisc/3.
?- dynamic $db_loaded/1.

/*
 * The following predicates are not really dynamic,
 * but after examining the .con file for the database,
 * which tells us which indexing schemes are needed,
 * scheme-specific versions of these predicates are `load'ed.
 *
 * dynamic($db_assert/3).
 * dynamic($db_retract/3).
 * dynamic($db_retractall/3).
 * dynamic($db_define/4).
 * dynamic($db_undefine/3).
 * dynamic($db_setup/3).
 */

%	This is not the best way to do this, but I can't be bothered figuring
%	out all the strange uses to which this thing is put.
$onDisc(Db, R, Scheme) :-
	getprop($onDisc, $onDisc, Db.R.Scheme).

$storeOnDisc(Db, R, Scheme) :-
	addprop($onDisc, $onDisc, Db.R.Scheme).

$removeFromDisc(Db, R, Scheme) :-
	remprop($onDisc, $onDisc, Db.R.Scheme).

$removeFromDisc(R) :-
	remprop($onDisc, $onDisc, _.R._).

/*
 * Redefine all of the predicates which manipulate the Prolog
 * database to take account of predicates in external databases.
 */

%
% redefine assert/1
%
assert((X :- Y)) :-
	$onDisc(Db, X, Scheme),
	!,
	(if (Scheme = rule) then
		$db_assert(Db, (X :- Y), Scheme)
	else (
		printf(user_error,
			"can't assert rules in %s external database\n", [Scheme]),
		portraycl(user_error, (X :- Y)),
		fail
	)).
assert(X) :-
	$onDisc(Db, X, Scheme),
	!,
	$db_assert(Db, X, Scheme).
assert(X) :-
	$assertz(X, _).

%
% redefine assert/2
%
assert((X :- Y), Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
assert(X, Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
assert(X, Ref) :-
	$assertz(X, Ref).

%
% redefine asserta/1
%
asserta((X :- Y)) :-
	$onDisc(Db, X, Scheme),
	!,
	(if (Scheme = rule) then
		$db_asserta(Db, (X :- Y), Scheme)
	else (
		printf(user_error, "can't use asserta in %s external database\n", [Scheme]),
		portraycl(user_error, (X :- Y)),
		fail
	)).
asserta(X) :-
	$onDisc(Db, X, Scheme),
	!,
	(if (Scheme = rule) then
		$db_asserta(Db, X, Scheme)
	else (
		printf(user_error, "can't use asserta in %s external database\n", [Scheme]),
		portraycl(user_error, (X :- true)),
		fail
	)).
asserta(X) :-
	$asserta(X, _).

%
% redefine asserta/2
%
asserta((X :- Y), Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
asserta(X, Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
asserta(X, Ref) :-
	$asserta(X, Ref).

%
% redefine assertz/1
%
assertz((X :- Y)) :-
	$onDisc(Db, X, Scheme),
	!,
	(if (Scheme = rule) then
		$db_assertz(Db, (X :- Y), Scheme)
	else (
		printf(user_error, "can't use assertz in %s external database\n", [Scheme]),
		portraycl(user_error, (X :- Y)),
		fail
	)).
assertz(X) :-
	$onDisc(Db, X, Scheme),
	!,
	(if (Scheme = rule) then
		$db_assertz(Db, X, Scheme)
	else (
		printf(user_error, "can't use assertz in %s external database\n", [Scheme]),
		portraycl(user_error, (X :- true)),
		fail
	)).
assertz(X) :-
	$assertz(X, _).

%
% redefine assertz/2
%
assertz((X :- Y), Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
assertz(X, Ref) :-
	$onDisc(Db, X, Scheme),
	!,
	putl(user_error, "can't use assert references with external databases\n"),
	fail.
assertz(X, Ref) :-
	$assertz(X, Ref).

%
% redefine retract/1
%
retract((X :- Y)) :-
	$onDisc(Db, X, rule), !, $db_retract(Db, (X :- Y), rule).
retract((X :- true)) :-
	$onDisc(Db, X, Scheme), !, $db_retract(Db, X, Scheme).
retract(X) :-
	X ~= (_:-_), $onDisc(Db, X, Scheme), !, $db_retract(Db, X, Scheme).
retract(A) :-
	$retract(A).

%
% redefine retractall/1
%
retractall((X :- Y)) :-
	$onDisc(Db, X, rule), !, $db_retractall(Db, (X :- Y), rule).
retractall((X :- true)) :-
	$onDisc(Db, X, Scheme), !, $db_retractall(Db, X, Scheme).
retractall(X) :-
	X ~= (_:-_), $onDisc(Db, X, Scheme), !, $db_retractall(Db, X, Scheme).
retractall(A) :-
	$retract((A :- _)), fail.
retractall(_).

%
% redefine clause/2
% (may need changing for rule db's?)
%
clause(X, true) :-
	$onDisc(_D, X, _S), !, X.
clause(A, B) :-
	$clause(A, B, _Ref),
	(A :- B) = C.

%
% redefine clauses/3
% (may need changing for rule db's?)
%
clauses(P, A, Clauses) :-
	functor(X, P, A),
	$onDisc(_D, X, _S),
	!,
	findall((X :- true), X, Clauses).
clauses(P, A, Clauses) :-
	$clauses(P, A, Clauses).

/*
%
% redefine getclause (this may need changing for rule ?)
%
% BUG!  For this to work, $ondisc/3 should use the property lists
% of the predicates, rather than be a dynamic relation.
getclause(P, A, (X :- true)) :-
	functor(X, P, A),
	$onDisc(_D, X, _S),		% BUG!!!!  Mutual recursion!
	!,
	X.
getclause(P, A, Clause) :-
	$getclause(P, A, Clause).
*/


/*
 * Predicates for creating and consulting external databases
 *
 * dbCons/1
 * dbCreate/1
 * dbRules/1
 *
 */

%
% dbCons/1 - consult external database
%
%	Reads .con file for names and types of database predicates
%	Sets up $onDisc definitions for database predicates
%	Loads .rules file containing database access rules
%
dbCons(Db) :-
	$dbReadConFile(Db),
	$dbLoadRulesFile(Db).

$dbReadConFile(Db) :-
	name(Db, DS),
	append(DS, "/.con", FS),
	name(F, FS),
	open(F, read, N),
	currentInput(Seeing),
	setInput(N),
	repeat,
	read(T),
	$dbSetupPred(Db, T),
	close(N),
	setInput(Seeing),
	!.

$dbLoadRulesFile(Db) :-
	name(Db, DS),
	append(DS, "/.rules.no", RS),
	name(R, RS),
	load(R).

$dbSetupPred(_, end_of_file).
$dbSetupPred(Db, relation(R, Scheme)) :-
	$removeFromDisc(R),			% means that we can't have preds
								% with same name in different db's
	functor(R, Functor, Arity),
	dynamic(Functor/Arity),
	addpropa(Functor, $property(Arity), database),
	retractall(R),			% means that we can't have pred
					% which is part-internal/part-external
	$dbLoadScheme(Scheme),
	$db_setup(Db, R, Scheme),
	$storeOnDisc(Db, R, Scheme),
	fail.

$dbLoadScheme(Scheme) :-
	(if not $db_loaded(Scheme) then
		$autoLoadLibrary(Scheme),
		assert($db_loaded(Scheme))
	).

%
% dbCreate/1 - create new database
%
%	Creates directory + (empty) .rules.nl + .rules.no + .con
%	Also creates $param relation to store details of other relations
%
dbCreate(Db) :-
	(if not mkdir(Db) then
		access(Db, 2'111)
	),
	!,
	chmod(Db, 8'755),
	dbRules(Db, '/dev/null'),
/***
	name(Db, DS),
	append("touch ", DS, C2),
	append(C2, "/.rules.nl", C3),
	system(C3, 0),
	append(C2, "/.con", C4),
	system(C4, 0),
%##	append("BIN/nc -c ", DS, C5),
	append("/mip/jas/bin/nc -c ", DS, C5),
	append(C5, "/.rules.nl", C6),
	system(C6, 0),
***/
	$dbDefine(Db, $param, 4, dsimc).

%
% dbRules/2 - add some rules to a database
%
%	Copy rules file into external db and recompile
%	Overwrites any existing .rules file
%
dbRules(Db, R) :-
	name(Db, DS),
	name(R, RS),
	append("cp ", RS, C1),
	append(C1, 32.DS, C2),
	append(C2, "/.rules.nl", C3),
	system(C3, 0),
%##	append("BIN/nc -c ", DS, C5),
	append("/mip/jas/bin/nc -c ", DS, C5),
	append(C5, "/.rules.nl", C6),
	system(C6, 0).


/*
 * Predicates for creating and changing the file structure
 * of external database predicates
 *
 * dbDefine/3
 * dbDefine/4
 * dbRedefine/2
 * dbRedefine/3
 * dbUndefine/2
 */

%
% dbDefine/4 - create new relation with specified parameters
%
%	Usage: dbDefine(Db, Functor, Arity, [Params]).
%	If Params is an empty list, get *all* default params
%
dbDefine(Db, Functor, Arity, Params) :-
	$dbSetValues(Db, Functor, Arity, Params),
	dbParam(Db, Functor, Arity, scheme = Scheme),
	$dbDefine(Db, Functor, Arity, Scheme).

$dbSetValues(_, _, _, []).
$dbSetValues(Db, Functor, Arity, (Parameter = Value).Params) :-
	$dsimc_insert(Db, $param(Functor, Arity, Parameter, Value)),
	$dbSetValues(Db, Functor, Arity, Params).

%
% dbDefine/3 - create new relation with designer parameters
%
%	Usage: dbDefine(Db, Functor, Arity).
%	Prompts user for all parameters (interactive db designer)
%
dbDefine(Db, Functor, Arity) :-
	(if not dbCons(Db) then
		printf(user, "dbDefine/2: database '%s' does not exist\n", [Db]),
		(if $yesno("Do you wish to create it", []) then
			dbCreate(Db)
		else
			fail
		)
	),
	(if (some R ($onDisc(Db, R, _), functor(R, Functor, _))) then
		printf(user, "dbDefine/3: predicate '%s' already exists in database '%s'\n", [Functor, Db]),
		(if $yesno("Do you wish to redefine it", []) then
			dbRedefine(Db, Functor, Arity)
		else
			exit(0)
		)
	),
	$newparam(Db, Functor, Arity, "Access scheme", atom, scheme, Scheme,
		$scheme(Scheme), "rlhp, simc, rule, sql"),
	$getparam(Db, Functor, Arity, Scheme),
	$dbLoadScheme(Scheme),
	(if $dbDefine(Db, Functor, Arity, Scheme) then
		true
	else
		writeln(user_error, "dbDefine failed")
	).

%--$getparam/4

$getparam(Db, Functor, Arity, simc) :-
	$newparam(Db, Functor, Arity, "Segment size", int, segsize, Ss, $segsize(Ss), "integer expected"),
	$newparam(Db, Functor, Arity, "Average record Length", int, avrec, Av, $avrec(Av, Ss), "integer expected"),
	$newparam(Db, Functor, Arity, "Maximum number of records", int, nrec, Nr, $positive(Nr), "integer expected"),
	$newparam(Db, Functor, Arity, "Number of data files", int, ndata, Nd, $positive(Nd), "integer expected"),
	$template(Db, Functor, Arity).

$getparam(Db, Functor, Arity, dsimc) :-
	$newparam(Db, Functor, Arity, "Segment size", int, segsize, Ss, $segsize(Ss), "integer expected"),
	$newparam(Db, Functor, Arity, "Average record Length", int, avrec, Av, $avrec(Av, Ss), "integer expected"),
	$newparam(Db, Functor, Arity, "Initial number of records", int, nrec, Nr, $positive(Nr), "integer expected"),
	$newparam(Db, Functor, Arity, "Number of data files", int, ndata, Nd, $positive(Nd), "integer expected"),
	$template(Db, Functor, Arity).

$getparam(_Db, _Functor, _Arity, rule).

$getparam(_Db, _Functor, _Arity, sql) :-
	writeln(user_error, "Need to use UNIFY").

%--$scheme/1

$scheme(dsimc).
$scheme(simc).
$scheme(rule).
$scheme(sql).

%--$segsize/1

$segsize(4096).
$segsize(2048).
$segsize(1024).
$segsize(512).

%--$positive/1

$positive(X) :-
	X > 0.

%--$nonneg/1

$nonneg(X) :-
	X >= 0.

%--$avrec/2

$avrec(Av, Ss) :-
	Av > 0,
	Av =< Ss.

%--$template/2

$template(Db, Functor, Arity) :-
	$elements(Db, Functor, Arity, 0, Arity, Elements, 16'7fffff),
	append("g:0:0(", Elements, T1),
	append(T1, ")", Template),
	$dsimc_insert(Db, $param(Functor, Arity, template, Template)).

%--$elements/7

$elements(_, _, _, N, N, [], 0).
$elements(_, _, _, N, N, [], Bits) :-
	Bits =\= 0,
	printf(user_error, "Warning: some bits not specified by template masks: %08x\n",[Bits]).
$elements(Db, F, A, N1, N2, Elements, Bits) :-
	N3 is N1 + 1,
	printf(user, "Template for field [%d]\n", [N3]),
	$element(Db, F, A, N3, Elem, Mask, Bits),
	Bitsleft is Bits ^ Mask,
	$elements(Db, F, A, N3, N2, Elems, Bitsleft),
	(if (Elems = []) then
		Elements = Elem
	else (
		append(",", Elems, E1),
		append(Elem, E1, Elements)
	)).

%--$element/7

$element(Db, F, A, I, Elem, Mask, Bits) :-
	$newparam(Db, F, A, "flag", list, flag(I), Flag, $flag(Flag), "g or n"),
	$newparam(Db, F, A, "nbits", int, weight(I), Nbits, $nonneg(Nbits), "integer expected"),
	$newparam(Db, F, A, "mask", hex, mask(I), Mask, $mask(Mask, Bits), "hex number expected"),
	append(Flag, ":", E1),
	intToString(Nbits, NBS),
	append(E1, NBS, E2),
	append(E2, ":", E3),
	listToHex(MS, Mask),
	append(E3, MS, E4),
	append(E4, MS, Elem).

%--$flag/1

$flag("g").
$flag("n").

%--$mask/2

$mask(Mask, Bits) :-
	Mask /\ Bits =:= Mask.
	
/* output default value (if any) and input new (CR for no change) */

%--$newparam/8

$newparam(Db, Functor, Arity, Mess, Type, Param, New, Test, Emess) :-
	$defaultParam(Db, Functor, Arity, Param, Default),
	repeat,
	(	(Type = list; Type = atom),
		printf(user, "%s <%s>: ", [Mess, Default])
	;	Type = int,
		printf(user, "%s <%d>: ", [Mess, Default])
	;	Type = hex,
		printf(user, "%s <%x>: ", [Mess, Default])
	),
	$getline(Line),
	(	Line = [],
		New = Default
	;	Line ~= [],
		(	Type = list,
			New = Line
		;	Type = atom,
			atomToString(New, Line)
		;	Type = int,
			intToString(New, Line)
		;	Type = hex,
			listToHex(Line, New)
		),
		Test
	;	printf(user, "%s\n", [Emess]),
		fail
	),
	$dsimc_insert(Db, $param(Functor, Arity, Param, New)),
	!.

% conversion between (hexadecimal) integers and lists of hex digits
% hex numbers may include 0-9 a-f A-F

%--listToHex/2

listToHex(S, I) :-	/* uses var, cut beause 'times' doesn't exist yet */
		var(S),
		!,
		$xtol(I, [], S).
listToHex(S, I) :-
		$ltox(S, 0, I).

%--$ltox/3

/*  What is wrong with the following definition?

listToHex(S, I) :-
	( var(S) ->
		$xtol(I, [], S)
	;	$ltox(S, 0, I)
	).

$ltox([], I, I).
$ltox([D|Ds], I0, I) :-
	(   D =< 0'9 -> D >= 0'0, B is 0'0
	;   D =< 0'F -> D >= 0'A, B is 0'A - 10
	;   D =< 0'f -> D >= 0'a, B is 0'a - 10
	;   fail
	),
	I1 is (I0 << 4) - B + D,
	$ltox(Ds, I1, I).
*/

$ltox([D], N, I) :-
		0'0 =< D,
		D =< 0'9,
		I is N*16 + D - 0'0.
$ltox([D], N, I) :-
		0'a =< D,
		D =< 0'f,
		I is N*16 + 10 + D - 0'a.
$ltox([D], N, I) :-
		0'A =< D,
		D =< 0'F,
		I is N*16 + 10 + D - 0'A.
$ltox(D.D1.S, N, I) :-
		0'0 =< D,
		D =< 0'9,
		N1 is N*16 + D - 0'0,
		$ltox(D1.S, N1, I).
$ltox(D.D1.S, N, I) :-
		0'a =< D,
		D =< 0'f,
		N1 is N*16 + 10 + D - 0'a,
		$ltox(D1.S, N1, I).
$ltox(D.D1.S, N, I) :-
		0'A =< D,
		D =< 0'F,
		N1 is N*16 + 10 + D - 0'A,
		$ltox(D1.S, N1, I).

%--$xtol/3

$xtol(I, S, D.S) :-
		I =< 9,
		D is I + 0'0.
$xtol(I, S, D.S) :-
		I >= 10,
		I =< 15,
		D is I - 10 + 0'a.
$xtol(I, S, S1) :-
		I >= 16,
		D is I mod 16 + 0'0,
		0'0 =< D,
		D =< 0'9,
		I2 is I//16,
		$xtol(I2, D.S, S1).
$xtol(I, S, S1) :-
		I >= 16,
		D is I mod 16 + 0'a - 10,
		0'a =< D,
		D =< 0'f,
		I2 is I//16,
		$xtol(I2, D.S, S1).

%--$yesno/2

$yesno(Mess, Default) :-
	repeat,
	(if Default ~= [] then
		printf(user, "%s ? <%s>: ", [Mess, Default])
	else
		printf(user, "%s ? ", [Mess])
	),
	$getline(Line),
	(	% yes answer - succeed
		(	Line = [],
			Default = 0'y._
		;	Line = 0'y._
		)
	;	% no answer - fail
		(	Line = [],
			Default = 0'n._
		;	Line = 0'n._
		),
		!,
		fail
	;	% otherwise - try again
		(	putl(user, "Type 'y' or 'n'\n"),
			fail
		)
	),
	!.

%--$getline/1

$getline(X) :-
	get0(Y),
	(	Y = 10,
		X = []
	;	Y ~= 10,
		X = Y.Z,
		$getline(Z)
	).

$dbDefine(Db, Functor, Arity, Scheme) :-
					% check args and convert as needed
	name(Db, DbL),
	functor(R, Functor, Arity),
					% retract any existing defn of R
%	retractall(R),			% if we're about to do newrel ...
					%  why bother retracting everything?
					% load Scheme if not already loaded
	$dbLoadScheme(Scheme),
					% define Db, R, Scheme
	$db_define(Db, Functor, Arity, Scheme),
	$db_setup(Db, R, Scheme),
	addpropa(Functor, $property(Arity), database),
	$storeOnDisc(Db, R, Scheme),
						% update .con file
	append(DbL, "/.con", CS),
	name(C, CS),
	not $db_exists(C, Functor),		% reln should not exist already
	open(C, append, N1),			% append new reln to db contents
	write(N1, relation(R,Scheme)),
	write(N1, '.'),
	nl(N1),
	close(N1).



%
% change a relation
%

dbRedefine(_Db, _Functor,_Arity) :-
	printf(user_error, "dbRedefine/2: interactive form not yet implemented", []),
	fail.
dbRedefine(_Db, _Functor, _Arity, _Params) :-
	printf(user_error, "dbRedefine/3: not yet implemented", []),
	fail.

%
% remove relation
%

dbUndefine(Db, Functor, Arity) :-
	remprop($db_tmp, Db),
	name(Db, DS),
	append(DS, "/.con", CS),
	name(C, CS),
	open(C, read, F),
	repeat,		% read current db contents into $db_tmp property list
	read(F, T),
	(
		isEof(T)
	;
		addprop($db_tmp, Db, T),
		fail
	),
	!,
	close(F),
	getprop($db_tmp, Db, relation(R1, Scheme)),
	functor(R1, Functor, Arity),
	remprop($db_tmp, Db, relation(R1, Scheme)),	% remove relation from $db_tmp
	!,
	$removeFromDisc(Db, R1, Scheme),		% in case we have done a dbCons
	retractall(R1),
	retractall($param(Functor, Arity, _, _)),
	$db_undefine(DS, Functor, Scheme),
	$db_write_con(Db, C).

%
% write back updated list of relations to .con file
%
$db_write_con(Db, C) :-
	open(C, write, F),
	(
		getprop($db_tmp, Db, R),
		write(F, R),
		write(F, '.'),
		nl(F),
		fail
	;
		close(F)
	).

%
% does R already exist in .con file C ?
%
$db_exists(C, R) :-
	open(C, read, F),
	repeat,
	read(F, T),
	(
		eof(T)
	;
		T = relation(P, _),
		functor(P, R, _)
	),
	!,
	close(F),
	T = relation(P, _),
	functor(P, R, _),
	write(user_error, 'A relation of that name already exists'),
	nl(user_error).
/*
 *
 *
 * predicates to backup and restore an external database
 *
 * dbBackup/2
 * dbRestore/2
 *
 *
 */

dbBackup(Db, File) :-
	atomToString(Db, DbL),
	atomToString(File, FileS),
	append("(cd ", DbL, S1),
	append(S1, "; tar cf - .) >", S2),
	append(S2, FileS, Cmd),
	system(Cmd, 0).

dbRestore(Db, File) :-
	atomToString(Db, DbL),
	atomToString(File, FileS),
	(if mkdir(Db) then
		append("(cd ", DbL, S1),
		append(S1, DbL, S2),
		append(S2, "; tar xf - .) <", S3),
		append(S3, FileS, Cmd),
		system(Cmd, 0)
	else (
		printf(user_error, "Failed to open: %s\n", [DbL])
	)).
/*
 *
 *
 * predicate for accessing database parameters
 *
 * dbParam/4
 *
 */


dbParam(Db, Functor, Arity, Parameter = Value) :-
	(	Functor ~= $param,
		$dsimc_retrieve(Db, $param(Functor, Arity, Parameter, V)),
		!,
		Value = V
	;	$defaultParam(Db, Functor, Arity, Parameter, V),
		!,
		Value = V
	;	format(user_error,
			"~NdbParam/4: no value for parameter ~w.~n", [Parameter]),
		fail
	).

%
% We need to map the generic database predicates into
% versions specific for the various indexing schemes
%
% The best place to define these things would be along
% with the definitions for the indexing scheme, but if
% you do that, they tend to clobber one another when
% you load the various indexing schemes in ... maybe
% someday you will be able to split predicate defins
% across files
%

%
% Map general DB preds to SIMC-specific versions
% Map general DB preds to DSIMC-specific versions
% Map general DB preds to SQL-specific versions
% Map general DB preds to RULE-specific versions
%
$db_assert(Db, R, simc) :- $simc_insert(Db, R).
$db_assert(Db, R, dsimc) :- $dsimc_insert(Db, R).
$db_assert(Db, R, sql) :- $sql_assert(Db, R).
$db_assert(Db, R, rule) :- $rule_assertz(Db, R).
$db_assertz(Db, R, rule) :- $rule_assertz(Db, R).
$db_asserta(Db, R, rule) :- $rule_asserta(Db, R).
$db_retract(Db, R, simc) :- $simc_retract(Db, R).
$db_retract(Db, R, dsimc) :- $dsimc_retract(Db, R).
$db_retract(Db, R, sql) :- $sql_retract(Db, R).
$db_retract(Db, R, rule) :- $rule_retract(Db, R).
$db_retractall(Db, R, simc) :- $simc_rall(Db, R).
$db_retractall(Db, R, dsimc) :- $dsimc_rall(Db, R).
$db_retractall(Db, R, sql) :- $sql_rall(Db, R).
$db_retractall(Db, R, rule) :- $rule_rall(Db, R).
$db_define(Db, R, N, simc) :- $simc_define(Db, R, N).
$db_define(Db, R, N, dsimc):- $dsimc_define(Db, R, N).
$db_define(Db, R, N, sql) :- $sql_define(Db, R, N).
$db_define(Db, R, N, rule) :- $rule_define(Db, R, N).
$db_undefine(Db, R, simc) :- $simc_undefine(Db, R).
$db_undefine(Db, R, dsimc) :- $dsimc_undefine(Db, R).
$db_undefine(Db, R, sql) :- $sql_undefine(Db, R).
$db_undefine(Db, R, rule) :- $rule_undefine(Db, R).
$db_setup(Db, R, simc) :- assert((R :- $simc_retrieve(Db, R))).
$db_setup(Db, R, dsimc) :- assert((R :- $dsimc_retrieve(Db, R))).
$db_setup(Db, R, sql) :- assert((R :- $sql_retrieve(Db, R))).
$db_setup(Db, R, rule) :- $rule_setup(Db, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%--default/4

?- $defaultParam(Db, Functor, Arity, Param, Value) when
	ground(Db) and ground(Functor) and ground(Arity) and Param.

%
% at present this is all done in integer arithmetic
% should be converted to floating point where appropriate
%
%	ln(X) is log to base e of X
%

%
% scheme - indexing scheme
%
%	possible values = dsimc, simc, sql, rule
%
$defaultParam(_, _, _, scheme, dsimc).
%
%
% segsize - segment size	(dsimc, simc)
%	
$defaultParam(_, _, _, segsize, 4096).
%
% avrec - average record size	(dsimc, simc)
%
$defaultParam(_, _, Arity, avrec, Avrec) :-
	Avrec is Arity * 8.
%
% nrec	- initial number of records	(dsimc)
%	- maximum number of records	(simc)
%
$defaultParam(_, _, _, nrec, 0).
%
% ndata - number of data files	(dsimc, simc)
%
$defaultParam(_, _, _, ndata, 1).
%
% nr - number of records per segment	(dsimc, simc)
%
%	Nr is maximum of
%	
%		SliceWidth * 32
%
%		((Segsize-SizeSegPtr) - (Br_est + 1) * (SliceWidth + 1) * 4)) /
%			(SizeRecPtr + Avrec)
%
%	where
%		SliceWidth (an estimate of width in words of each slice) is
%			(Segsize - SizeSegPtr) /
%			(4 * (Br_est + 1) + 32 * (SizeRecPtr + Avrec))
%
%		SizeSegPtr (size of Segptr in bytes) = 8
%
%		SizeRecPtr (size of Recptr in bytes) = 12
%
%		Br_est (estimate of record codeword size in bits) is
%			Arity * 16 = (Arity * ln(1 / Fr)) / (ln(2) * ln(2))
%
%		Fr (false match probabilty within segment) is approx 1/2180
%
%
$defaultParam(Db, F, A, nr, Nr) :-
	dbParam(Db, F, A, segsize = Segsize),
	dbParam(Db, F, A, avrec = Avrec),
	SliceWidth is (Segsize - 8) // (64 * A + 32 * Avrec + 388),
	Nr1 is SliceWidth * 32,
	Nr2 is ((Segsize - 8) - (16 * A + 1) * (SliceWidth + 1) * 4) //
		(12 + Avrec),
	(if Nr1 > Nr2 then Nr is Nr1 else Nr is Nr2).
%
% lc - load control	(dsimc)
%
%	Lc is Nr * load factor
%
%	default uses a load factor of 80%
%
$defaultParam(Db, F, A, lc, Lc) :-
	dbParam(Db, F, A, nr = Nr),
	Lc is (Nr * 8)//10.
%
% nseg	- maximum number of segments	(simc)
%
$defaultParam(Db, F, A, nseg, Nseg) :-
	dbParam(Db, F, A, nrec = Nrec),
	dbParam(Db, F, A, nr = Nr),
	Nseg is Nrec//Nr.
%
% br	- number of bits in record codewords	(dsimc, simc)
%	  (that is number of slices)
%
%	Br is EstBr + (Segsize - Segused) / (Slicewidth * 4)
%
%	where
%		Segused (segment used for records + Estbr + overheads) is
%			(EstBr + 1) * SliceWidth * 4 +
%				Nr * SizeRecPtr +
%				SizeSegPtr +
%				Nr * Arity
%
%		SliceWidth (width in words of each slice) is
%			Nr / 32 rounded up
%
%		EstBr (estimate of record codeword size in bits) is
%			Arity * 16 = (Arity * ln(1 / Fr)) / (ln(2) * ln(2))
%
%		Fr (false match probabilty within segment) is approx 1/2180
%
%		SizeSegPtr (size of Segptr in bytes) = 8
%
%		SizeRecPtr (size of Recptr in bytes) = 12
%
%
$defaultParam(Db, F, A, br, Br) :-
	dbParam(Db, F, A, nr = Nr),
	dbParam(Db, F, A, segsize = Segsize),
	dbParam(Db, F, A, avrec = Avrec),
	SliceWidth is (Nr + 31) // 32,
	Segused is (A * 16 + 1) * SliceWidth * 4 + Nr * 12 + 8 + Nr * Avrec,
	Br is A * 16 + (Segsize - Segused) // (SliceWidth * 4).
%
% bs	- number of bits in segment codewords	(dsimc, simc)
%
%	dsimc
%		Bs is Segsize / 4
%
%	simc
%		Bs is Ks * Arity * Nr * (Nseg ** (1 / Ks))
%
%
$defaultParam(Db, F, A, bs, Bs) :-
	dbParam(Db, F, A, scheme = dsimc),
	dbParam(Db, F, A, segsize = Segsize),
	Bs is Segsize//4.
$defaultParam(Db, F, A, bs, Bs) :-
	dbParam(Db, F, A, scheme = simc),
	dbParam(Db, F, A, nr = Nr),
	dbParam(Db, F, A, nseg = Nseg),
	dbParam(Db, F, A, ks = Ks),
%	$power(Ks, Ks, KsKs),
%	Y is KsKs * Nseg,
%	$root(Y, Ks, RootY),
%	Bs is RootY * A * Nr.
	X is integer(Ks * (Nseg ** (1 / Ks)) * A * Nr + 0.5),	% (jws)
	(X > 0 ->
		Bs = X
	;	Bs = 1
	).
%
% kr	- number of bits to be set in record codeword	(dsimc, simc)
%
%	Kr is ln(1 / Fr) / ln(2)
%
%		where Fr = false match probabilty within segment
%
%	default uses Fr as approx 1/2180
%
$defaultParam(_, _, _, kr, 11).
%
% ks	- number of bits to be set in segment codeword	(dsimc, simc)
%
%	dsimc
%		Ks is Bs * ln(2) / (Nr * Arity) rounded up
%
%	simc
%		Ks is 4
%
$defaultParam(Db, F, A, ks, 4) :-
	dbParam(Db, F, A, scheme = simc).
$defaultParam(Db, F, A, ks, Ks) :-
	dbParam(Db, F, A, scheme = dsimc),
	dbParam(Db, F, A, bs = Bs),
	dbParam(Db, F, A, nr = Nr),
	Ks is (Bs * 69 + 99 * Nr * A) // (100 * Nr * A).
%
% ur	- number of record bit-slices to be examined at query time (dsimc, simc)
%
%	Ur is Kr
%
$defaultParam(Db, F, A, ur, Ur) :-
	dbParam(Db, F, A, kr = Ur).
%
% us	- number of segment bit-slices to be examined at query time (dsimc,simc)
%
%	Us is Ks
%
$defaultParam(Db, F, A, us, Us) :-
	dbParam(Db, F, A, ks = Us).
%
% flag(0) - flag for functor name			(dsimc, simc)
% flag(N) - flag for Nth parameter of predicate		(dsimc, simc)
%
%	dsimc
%		flag(N) must be "g"
%
%	simc
%		flag(N) may be "g" or "n"
%
$defaultParam(_, _, _, flag(_), "g").
%
% weight(0) - weight for functor name			(dsimc, simc)
% weight(N) - weight for Nth parameter of predicate	(dsimc, simc)
%
$defaultParam(_, _, _, weight(0), 0).
$defaultParam(_, _, Arity, weight(N), 1) :-
	N > 0, N =< Arity.
%
% mask(0) - mask for functor name			(dsimc, simc)
% mask(N) - mask for Nth parameter of predicate		(dsimc, simc)
%
%	sum of masks should be 16'7fffff (23 bits)
%
%	actually duplicate masks in template (duplicate bottom 8 bits from top)
%
$defaultParam(_, _, _, mask(0), 0).
$defaultParam(_, _, 1, mask(1), 16'7fffff).
$defaultParam(_, _, 2, mask(1), 16'555555).
$defaultParam(_, _, 3, mask(1), 16'492492).
$defaultParam(_, _, 4, mask(1), 16'444444).
$defaultParam(_, _, 5, mask(1), 16'421084).
$defaultParam(_, _, 6, mask(1), 16'410410).
$defaultParam(_, _, 7, mask(1), 16'408102).
$defaultParam(_, _, 8, mask(1), 16'404040).
$defaultParam(_, _, 9, mask(1), 16'402010).
$defaultParam(_, _, 10, mask(1), 16'401004).
$defaultParam(_, _, 11, mask(1), 16'400801).
$defaultParam(_, _, 12, mask(1), 16'400400).
$defaultParam(_, _, 13, mask(1), 16'400200).
$defaultParam(_, _, 14, mask(1), 16'400100).
$defaultParam(_, _, 15, mask(1), 16'400080).
$defaultParam(_, _, 16, mask(1), 16'400040).
$defaultParam(_, _, 17, mask(1), 16'400020).
$defaultParam(_, _, 18, mask(1), 16'400010).
$defaultParam(_, _, 19, mask(1), 16'400008).
$defaultParam(_, _, 20, mask(1), 16'400004).
$defaultParam(_, _, 21, mask(1), 16'400002).
$defaultParam(_, _, 22, mask(1), 16'400001).
$defaultParam(_, _, Arity, mask(1), 16'400000) :-
	Arity >= 23.
$defaultParam(Db, F, Arity, mask(N), Mask) :-
	N > 1,
	N =< Arity,
	$defaultParam(Db, F, Arity, mask(1), Mask1),
	Mask is Mask1 >> (N - 1).
%
% element(0) - element of template for functor name		  (dsimc, simc)
% element(N) - element of template for Nth parameter of predicate (dsimc, simc)
%
%	element(X) is "flag(X):weight(X):mask(X)"
%
$defaultParam(Db, F, A, element(X), Element) :-
	dbParam(Db, F, A, flag(X) = Flag),
	dbParam(Db, F, A, weight(X) = Weight),
	dbParam(Db, F, A, mask(X) = Mask),
	intToString(Weight, WeightL),
	listToHex(MaskL, Mask),
	Mask1 is Mask >> 19 /\ 16'f,	% second last hex digit
	Mask2 is Mask >> 15 /\ 16'f,	% last hex digit
	listToHex(Mask1L, Mask1),
	listToHex(Mask2L, Mask2),
	$sprintf(E, "%:%:%%%", [Flag, WeightL, MaskL, Mask1L, Mask2L]),
	atomToString(Element, E).
%
% template - template of predicate		(dsimc, simc)
%
%	Template is "element0(element1,...)"
%
$defaultParam(Db, F, A, template, Template) :-
	dbParam(Db, F, A, element(0) = Element0),
	bagof(
		Element,
		X ^ (iota(1,A,X), dbParam(Db, F, A, element(X) = Element)),
		Set
	),
	T =.. Element0.Set,
	termToString(T, Template).
%
% key - a key				(dsimc, simc ?)
%
%
% 	key(0) is list of 0's (length of list is Arity), which corresponds
%	all fields being part of key 
%
%
$defaultParam(Db, F, A, key(0), List) :-
	bagof(0, (N ^ iota(1, A, N)), List).

%
% solns - number of solutions for a particular instantiation List	(dsimc)
%
%	Solns is 1 if List is a superset of a key
%
%		otherwise product of domains of uninstatiated args
%
$defaultParam(Db, F, A, solns(List), 1) :-
	dbParam(Db, F, A, key(_) = Key),
	FK =.. f.Key,
	FL =.. f.List,
	bagof(	L,
		(	iota(1, A, N),
			arg(N, FK, K),
			arg(N, FL, L),
			(	K =:= 0,
				L =:= 0
			;	K =:= 1
			)
		),
		List).		% BUG!  I don't believe this!  (jws)
$defaultParam(Db, F, A, solns(List), Solns) :-
	dbParam(Db, F, A, nrec = Nrec),
% I bet someone somewhere is really proud of this one! (jws)
	sum(Elem, SubList, member(Elem, List, SubList), Sum),
% They should be shot!
%	$root(Nrec, A, Root),
%	$power(Root, Sum, Solns).
	Solns is integer(Nrec ** (Sum / A) + 0.5).		% (jws)

/* JUNK!
%
% calculate Nth root of M
%
$root(M, N, Result) :-
	$root(0, M, M, N, Result).

$root(Result, Result, _, _, Result) :- !.
$root(A, B, M, N, Result) :- 
	B =:= A + 1,
	!,
	Tmp is A * 2 + 1,
	$power(Tmp, N, TmpN),
	$power(2, N, TwoN),
	(TmpN//TwoN > M ->
		Result is A
	;	Result is B
	).
$root(A, B, M, N, Result) :-
	C is (A + B) // 2,
	$power(C, N, CN),
	(CN > M ->
		$root(A, C, M, N, Result)
	;	$root(C, B, M, N, Result)
	).
	
$power(N, 0, 1).
$power(N, Power, Result) :-
	Power > 0,
	HalfPower is Power // 2,
	RemPower is Power mod 2,
	(RemPower =:= 0 ->
		$power(N, HalfPower, NHP),
		$mult(NHP, NHP, Result)
	;	Power1 is Power - 1,
		$power(N, Power1, NP1),
		$mult(N, NP1, Result)
	).


$mult(X, Y, 0) :-
	X =:= 0 or Y =:= 0.
$mult(X, Y, Z) :-
	X > 0 and Y > 0,
	maxint(MaxInt),
	((MaxInt//X < Y) or (MaxInt//Y < X) ->
		Z is MaxInt
	;	Z is X * Y
	).
*/

/*
 * sprintf:
 *	Formatted print routine
 *
 *	written by John Shepherd (jas@mulga.oz)
 */

$sprintf(Output, Format, ValueList) :-
	$sprintf(Output, [list,string,noquote,ops], Format, ValueList).

$sprintf(Output, Flags, Format, ValueList) :-
	$doSprintf(Flags, Format, ValueList, [], Output).

%
% stop when format string is exhausted; ignore excess values
%
$doSprintf(Flags, [], ValueList, Output, Output).
%
% if values are exhausted, ignore further % chars
%
$doSprintf(Flags, 0'%.MoreFormat, [], SoFar, Output) :-
	$doSprintf(Flags, MoreFormat, [], SoFar, Output).
%
% replace any %'s in format by the corresponding term
%
$doSprintf(Flags, 0'%.MoreFormat, (Str.Value).MoreValues, SoFar, Output) :-
	!,
	termToString(Flags, Str.Value, ValString0),
	append("""", ValString1, ValString0),
	append(ValString, """", ValString1),
	append(SoFar, ValString, MoreSoFar),
	$doSprintf(Flags, MoreFormat, MoreValues, MoreSoFar, Output).
%
% replace any %'s in format by the corresponding term
%
$doSprintf(Flags, 0'%.MoreFormat, Value.MoreValues, SoFar, Output) :-
	!,
	termToString(Flags, Value, ValString),
	append(SoFar, ValString, MoreSoFar),
	$doSprintf(Flags, MoreFormat, MoreValues, MoreSoFar, Output).
%
% otherwise, poke next format character unchanged into output
%
$doSprintf(Flags, Char.MoreFormat, ValueList, SoFar, Output) :-
	append(SoFar, [Char], MoreSoFar),
	$doSprintf(Flags, MoreFormat, ValueList, MoreSoFar, Output).

%
% determine cluster value of a term
%
simc_hash(Db, Term, Hash) :-
	termToString([cons,string,quote,prefix], Term, TString),
	$simc_hash(Db, TString, 4, Hash).

