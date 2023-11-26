%	Copyright (C) 1986, The University of Melbourne
%
%	nit - nuProlog incompetence tester
%	      Looks for common errors in nuProlog programs.
%
%	Author: J. Zobel, November 1986
%	Revised December 87: Justin Zobel, Philip Dart, Giles Lean

%FIX this - new options - static loops, flounder, types
usage :-
	putl(
"Usage : nit [-apsvx] [-d p/a] [-e p/a] [-F flag] [-l lib] [-u p/a] file ...\n"
).

main(_.Args) :-
	setDefaultOptions,
	process_args(Args),
		% Consult user if no file has been specified by args.
	( \+ getprop($nit, $file, _)
	->	dConsult(user), addprop($nit, $file, user)
	),
		% main/1 is entry predicate if not otherwise specified.
	( getprop($nit, $entry, Main) -> true ; Main = main/1 ),
	addprop($nit, $called, Main),
	dPreds(PredList),
	do_nit(PredList),
	(option_set(strat_check)
	->	strat_check		% Use tree to see if program stratified.
	),
	report.
	%exit(0).

%-- process_args -- analyse command line.
%FIX here handle new args

?- process_args(X) when X.
process_args('-d'.Pred.Args) :-		% Pred is defined elsewhere.
	!,
	atomToString(Pred, PredArity),
	((append(Func, 0'/.Arity, PredArity), intToString(A, Arity))
	->	atomToString(F, Func),
		addprop($nit, $defined, F/A),
		process_args(Args)
	;	putl("-d: argument should be Pred/Arity\n"),
		exit(1)
	).
process_args('-e'.Pred.Args) :-		% Pred is entry predicate.
	!,
	atomToString(Pred, PredArity),
	((append(Func, 0'/.Arity, PredArity), intToString(A, Arity))
	->	atomToString(F, Func),
		putprop($nit, $entry, F/A),
		process_args(Args)
	;	putl("-e: argument should be Pred/Arity\n"),
		exit(1)
	).
process_args('-F'.Flag.Args) :-		% Flag is set (as per nc).
	!,
	assert(option(Flag)),
	process_args(Args).
process_args('-l'.Library.Args) :-	% Library is used (as per nc).
	!,
	(libraryPredicate(Library, _, _)
	->	addprop($nit_lib, $lib, Library),
		process_args(Args)
	;	putl("-l: unknown library\n"),
		exit(1)
	).
process_args('-u'.Pred.Args) :-		% Pred is used elsewhere.
	!,
	atomToString(Pred, PredArity),
	((append(Func, 0'/.Arity, PredArity), intToString(A, Arity))
	->	atomToString(F, Func),
		addprop($nit, $called, F/A),
		process_args(Args)
	;	putl("-u: argument should be Pred/Arity\n"),
		exit(1)
	).
process_args('-'.Args) :-
	!,
	dConsult(user),
	addprop($nit, $file, user),
	process_args(Args).
process_args(Opts.Args) :-		% Process options.
	atomToString(Opts, 0'-.C.List),
	set_opts(C.List, unset),
	!,
	process_args(Args).
process_args(File.Args) :-
	( dConsult(File)
	->	applySuffix(File, ".nl", _, FName),
		addprop($nit, $file, FName)
	;	format("~a not found\n", [File]),
		usage,
		exit(1)
	),
	process_args(Args).
process_args([]).
process_args(_) :-
	usage,
	exit(1).

	% Unset any options in option list.
?- set_opts(L, T) when L and T.
set_opts([], _).
set_opts(O.List, set) :-
	is_opt(O, Opt),
	!,
	option_on(Opt),
	set_opts(List, set).
set_opts(O.List, unset) :-
	is_opt(O, Opt),
	!,
	option_off(Opt),
	set_opts(List, unset).
set_opts(_._List, _T) :-
	usage,
	exit(1).

processGoal(_File, G, N, V) :-
	process_goal(G),
	dAddClause(cl(($goal :- G), N, V)).
