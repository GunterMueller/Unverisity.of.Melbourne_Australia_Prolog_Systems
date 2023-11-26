%	Copyright (C) 1986, The University of Melbourne
%
%	nit - nuProlog incompetence tester
%	      Looks for common errors in nuProlog programs.
%
%	Author: J. Zobel, November 1986
%	Revised December 87: Justin Zobel, Philip Dart, Giles Lean


	% Check all predicates in domain.
nit :-
	init_db,
	setDefaultOptions,
	dPreds(PredList),
	do_nit(PredList),
	forall(getprop($debug, $dConsulted, File), addprop($nit, $file, File)),
	(option_set(strat_check)
	->	strat_check		% Use tree to see if program stratified.
	),
	report,
	dAbolishPred($goal, 0).

	% Check all predicates in File.  For interactive use.
?- nit(File) when File.
nit(File) :-
	( \+ getprop($nit, $file, File) -> addprop($nit, $file, File) ),
	applySuffix(File, ".nl", _, FName),
	findall(F/A, dFile(F, A, FName), PredList),
	do_nit(PredList),
	(option_set(strat_check)
	->	strat_check(FName)	% Use tree to see if program stratified.
	),
	report.

	% Check predicate.  For interactive use.
?- nit(F, A) when F and A.
nit(F, A) :-
	( dFile(F, A, File) -> true ; File = user ),
	( \+ getprop($nit, $file, File) -> addprop($nit, $file, File) ),
	do_nit([F/A]),
	(option_set(strat_check)
	->	strat_check(F, A)	% Use tree to see if program stratified.
	),
	report.

	% Run checks on each predicate in Preds.
?- do_nit(Preds) when Preds.
do_nit([]).
do_nit(F/A.Preds) :-
	(F/A \= $goal/0
	->	( option_set(call_check)
		->	call_check(F, A),	% Find undefined calls.
			used_check(F, A)	% Find unused predicates.
		), ( option_set(arity_check)
		->	arity_check(F, A)	% Find same functor, diff arity.
		)
	), ( option_set(var_check)
	->	scope_check(F, A),	% Find same var in multiple scopes.
		used_var(F, A),		% Var should occur more than once.
		name_check(F, A),	% Variable names within pred should
					% not be too similar.
		pos_check(F, A)		% Check that all vars occur positively.
	),
	do_nit(Preds).

init_db :-
	dGoals(GoalList),
	forall(member(goal(G, N, V), GoalList), (
	 	process_goal(G),
		dAddClause(cl(($goal :- G), N, V))
	)).

	% Process a goal.
?- process_goal(X) when X.
process_goal((A, B)) :-
	process_goal(A), process_goal(B).
process_goal((A ; B)) :-
	process_goal(A), process_goal(B).
process_goal(lib(Library)) :-
	(libraryPredicate(Library, _, _)
	->	addprop($nit_lib, $lib, Library)
	;	format("~a: unknown library\n", [Library])
	).
process_goal(dbCons(DB)) :-
		% Consult .con file for database.
	applySuffix(DB, "/.con", _, ConPath),
	open(ConPath, read, S),
	repeat,
	read(S, T),
	(isEof(T)
	->	close(S)
	;	T = relation(Pred, _),
		functor(Pred, F, A),
		addprop($nit, $dbClause, (F/A)),
		fail
	),
		% Consult .rules file for database.
	applySuffix(DB, "/.rules.nl", _, RulesPath),
	open(RulesPath, read, S1),
	repeat,
	read(S1, T1),
	(isEof(T1)
	->	close(S1)
	;	( T1 = ( H :- _ ) ->
			H1 = H
		;	T1 \= (?- _), T1 \= (:- _),
			T1 = H1
		),
		functor(H1, F, A),
		addprop($nit, $dbClause, (F/A)),
		fail
	),
	!.
process_goal(_).

%-- report -- Print all error messages.

%FIX I guess we get some new types of errors...

?- printArities(_.A) when A.
printArities(A.[]) :-
	format(" and ~d.\n", [A]).
printArities(A.L.L1) :-
	format(", ~d", [A]),
	printArities(L.L1).

report :-
	putl("\n---- general ----\n\n"),
		% Show arity errors.
	forall(rem_error(arity - (F / (A.Arities))),
		( format("~a has arities ~d", [F, A]), printArities(Arities) )
	),
		% Show errors relating to all goals (can't happen?).
	show_pred_errors($goal/0),
		% Show errors for individual goals.
	forall((
		dClause($goal, 0, cl(($goal :- G), N, V)),
		has_clause_error($goal, G, N, V)
	), (
		format("?- ~w:\n", G),
		show_clause_errors($goal, G)
	)),
	forall((getprop($nit, $file, File), remprop($nit, $file, File)), (
		format("\n---- ~a ----\n\n", [File]),
		report(File)
	)),
		% Clean up.
	remprop($nit, _).
		
report(File) :-
		% Show errors relating to predicates.
	forall((dFile(F, A, File), F/A \= $goal/0),
		show_pred_errors(F/A)
	),
	nl,
		% Show errors for individual clauses.
	forall((
		dFile(F, A, File), F/A \= $goal/0,
		dClause(F, A, cl((H :- B), N, V)),
		has_clause_error(H, B, N, V)
	), (
		( B = true
		->	dPortraycl(cl(H, N, V))
		;	dPortraycl(cl((H :- B), N, V))
		),
		putl(".\n"),
		show_clause_errors(H, B),
		nl
	)).

has_clause_error(H, B, N, V) :-
	get_error(_ - (H :- B) - _),
	name_vars(N, V),
		% All remaining vars correspond to underscore.
	listOfVars((H, B), UVs),
	make_uscore(UVs),
	!.

	% Show and remove errors relating to whole predicate.
?- show_pred_errors(Pred) when Pred.
show_pred_errors(F/A) :-
	( (getprop($nit, $modified, F/A), dDecs(F, A, dynamic, []))
	->	format("~a/~d is modified but not declared dynamic.\n", [F, A])
	),
	( (option_set(call_check), rem_error(unused - [F, A]))
	->	(getprop($nit, $modified, F/A)
		->	format("~a/~d is modified but not called.\n", [F, A])
		;	format("~a/~d is not called.\n", [F, A])
		)
	),
	forall(rem_error(negative - F.A.Anc), (
		write(Anc), putl(" contains negative recursion.\n")
	)).

	% Show and remove errors relating to clause.
?- show_clause_errors(X, Y) when X and Y.
show_clause_errors(H, B) :-
	forall(rem_error(undefined - (H :- B) - List),
		format("~a/~d is not defined.\n", List)
	),
	forall(rem_error(nonlog_pure - (H :- B) - List), (
		format("Non-logical predicate ~a/~d", List),
		putl(" called by pure predicate.\n")
	)),
	forall(rem_error(notpos - (H :- B) - $VAR(Name)),
		format("~s cannot be ground.\n", [Name])
	),
	forall(rem_error(too_often - (H :- B) - $VAR(Name)),
		format("~s should occur once only.\n", [Name])
	),
	forall(rem_error(distinct - (H :- B) - $VAR(Name)),
		format("~s occurs in distinct scopes.\n", [Name])
	),
	forall(rem_error(same_name - (H :- B) - [$VAR(N1), $VAR(N2)]),
		format("~s and ~s are suspiciously similar.\n", [N1, N2])
	),
	forall(rem_error(single_occur - (H :- B) - Vars1),
		format("The variable(s) ~w occur once only.\n", [Vars1])
	).

%-- misc --

	% Remove variable from list (non-logical delete).
?- rm(_, L, _) when L.
rm(V, V1.L, L) :-
	V == V1.
rm(V, V1.L, V1.L1) :-
	V \== V1,
	rm(V, L, L1).

	% Bind all vars in list to name term for underscore.
?- make_uscore(A) when A.
make_uscore([]).
make_uscore($VAR("_").L) :-
	make_uscore(L).

	% _1 is not an underscore variable if its name does not start with
	% _.  Variable is assumed underscore if not in list.
?- not_uscore(_, L, _) when L.
not_uscore(V, (C._)._, V1._) :-
	V == V1,
	C \= 0'_.
not_uscore(V, _.L1, V1.L2) :-
	V \== V1,
	not_uscore(V, L1, L2).

option_on(Opt) :- addprop($nit_opt, $opt, Opt).

option_off(Opt) :- remprop($nit_opt, $opt, Opt).

option_set(Opt) :- getprop($nit_opt, $opt, Opt).

add_error(E) :- addprop($nit, $error, E).

get_error(E) :- getprop($nit, $error, E).

rem_error(E) :- getprop($nit, $error, E), remprop($nit, $error, E).

%-- options --

?- is_opt(O, _) when O.
is_opt(0'a, arity_check).	% arities
is_opt(0'p, pure_check).	% pure predicates use pure constructs
is_opt(0's, strat_check).	% program is stratified
is_opt(0'v, var_check).		% scope, naming of variables, use, occur pos.
is_opt(0'x, call_check).	% subgoals are defined, predicates are called

%FIX some new options to go in here....

	% Set given options.  For interactive use.
?- setOptions(Opts) when Opts.
setOptions(Opts) :-
	atomToString(Opts, OptList),
	set_opts(OptList, set).

	% Unset given options.  For interactive use.
?- unsetOptions(Opts) when Opts.
unsetOptions(Opts) :-
	atomToString(Opts, OptList),
	set_opts(OptList, unset).

setDefaultOptions :-
	option_on(arity_check),
	option_on(call_check),
	option_on(pure_check),
	option_on(strat_check),
	option_on(var_check).
