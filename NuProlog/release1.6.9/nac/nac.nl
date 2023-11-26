/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

% LOGIC PRE-PROCESSOR (NOW CALLED NAC)
%
%	 nac < infile > outfile
%
%	adds when declarations
%	reorders clause bodies
%	warns about possible infinite loops
%
%	does not detect mutual recursion (yet)
%
%	Originally written by Lee Naish (for MU-Prolog),
%	additions for Extended Prolog and non-logical procedures 
%	put in and subsequently removed
%	further additions and efficiency mods by Lee Naish
%
%	THIS CODE DESPERATELY NEEDS CLEANING UP - IT IS A MESS

main(_) :-
 	lpp,
 	exit(0).
main(_) :-
 	writeln(user_error, error),
 	exit(1).

lpp :-
	writeheading,
	readin,		% read clauses from file F
	%defncheck,     % checks for undefined procedures - use nit!
	%reordercheck,   % checks for procedures which can't be reordered
	addwhens,	% add whens for all procedures
	calc_cost,	% find tests and calculate costs
	ordergoals,	% put generates after tests etc.
	writeout.	% write new program to file G

% read and assert clauses from file F (handles extended form, if necessary)
readin :-
	repeat,
	read(T),
	(	isEof(T)
	;	expandTerm(T, T1),
		process(T1),
		fail
	),
	!.

% process a clause
process((?-G)) :-
	!,
	processgoal(G).
process((H:-T)) :-
	!,
	assert((H:-T)),
	functor(H, F, N),
	functor(H1, F, N),
	note(isproc(H1)),
	callcheck((H1:-T)).
process(F) :-
	assert(F),
	functor(F, F1, N),
	functor(F2, F1, N),
	note(isproc(F2)).

processgoal(op(X, Y, Z)) :-
	!,
	assert(isproc($goal(?-op(X, Y, Z)))),
	op(X,Y,Z).
processgoal(pure(F/N)) :-
	!,
	functor(Y,F,N),
	note(is_pure(Y)),
	assert(isproc($goal(?-pure(F/N)))).
processgoal(when(X, Y)) :-
	!,
	functor(X,F,N),
	functor(Z,F,N),
	note(noaddwhen(Z)),
	% assert(whens(X when Y)),
	assert(isproc($goal(?-when(X,Y)))).
processgoal(G) :-
	assert(isproc($goal(?-G))).

%rdin([X|Y]) :-
%	readin(X),
%	rdin(Y).
%rdin([]).

%libreadin(F) :-
%	seeing(I),
%	see(F),
%	repeat,
%	read(T),
%	(	eof(T)
%	;
%		libprocess(T)
%	),
%	seen,
%	see(I),
%	!.
%
%% process a library clause
%libprocess((?-G)) :-
%	!,
%	libprocessgoal(G),
%	fail.
%libprocess((H:-T)) :-
%	!,
%	assert((H:-T)),
%	functor(H,F,N),
%	functor(H1,F,N),
%	note(islibproc(H1)),
%	callcheck((H1:-T)),
%	fail.
%
%libprocess(F) :-
%	functor(F,F1,N),
%	functor(F2,F1,N),
%	note(islibproc(F2)),
%	assert(F),
%	fail.
%
%libprocessgoal(op(X,Y,Z)) :-
%	!,
%	op(X,Y,Z).
%
%libprocessgoal(hide(X)) :-
%	!,
%	hide(X).
%
%libprocessgoal(hidden) :-
%	!,
%	hidden.
%
%libprocessgoal(lib(X)) :-
%	!,
%	name(X, S),
%	libdirectory(LC),
%	name(LC, LS),
%	$app(LS, 47.S, S1),
%	name(N, S1),
%	libreadin(N).
%
%libprocessgoal(X).

note(X) :-
	X,
	!.
note(X) :-
	assert(X).

	% YUK - make this two args and use meta_call
?- callcheck(X) when X.
callcheck((H:-B)) :-
	var(B),
	%note(nonreorderable(H)),
	!.
callcheck((H:- (A,B))) :-
	!,
	callcheck((H:-A)),
	callcheck((H:-B)).
callcheck((H:- (A;B))) :-
	!,
	callcheck((H:-A)),
	callcheck((H:-B)).
callcheck((H:- (if X then Y))) :-
	!,
	callcheck((H:-X)),
	callcheck((H:-Y)).
callcheck((H:- (X -> Y))) :-
	!,
	callcheck((H:-X)),
	callcheck((H:-Y)).
callcheck((H:- (if X then Y else Z))) :-
	!,
	callcheck((H:-X)),
	callcheck((H:-Y)),
	callcheck((H:-Z)).
callcheck((H:- (X -> Y ; Z))) :-
	!,
	callcheck((H:-X)),
	callcheck((H:-Y)),
	callcheck((H:-Z)).
callcheck((H:-call(X))) :-
	!,
	callcheck((H:-X)).
callcheck((H:- ~X)) :-
	!,
	callcheck((H:-X)).
callcheck((H:-gnot(X,Y))) :-
	!,
	note(calls(H,gnot(_,_))),
	callcheck((H:-Y)).
callcheck((H:-A)) :-
	functor(A,F,N),
	functor(A1,F,N),
	note(calls(H,A1)),
	(non_det(A1) ->
		not(non_det(H)),
		asserta(non_det(H))
	).

%defncheck :-
%	calls(_,Y),
%	processcall(Y),
%	fail.
%defncheck.
%
%processcall(Y) :-
%	isproc(Y),
%	!.
%processcall(Y) :-
%	islibproc(Y),
%	!.
%processcall(Y) :-
%	system(Y),
%	!.
%processcall(Y) :-
%	\+ undefined(Y),
%	note(undefined(Y)),
%	functor(Y,F,N),
%	write('% procedure '),
%	write(F),
%	write(/),
%	write(N),
%	writeln(' is not defined').
%
%reordercheck :-
%	noreorder(N),
%	calls(P, N),
%	asnoreorder(P),
%	fail.
%reordercheck.
%
%asnoreorder(P) :-
%	nonreorderable(P),
%	!.
%asnoreorder(P) :-
%	asserta(nonreorderable(P)),
%	calls(Q, P),
%	asnoreorder(Q),
%	fail.
%asnoreorder(P).

	% SHOULD
	%	use consistent names
	%	use (Functor, Arity), not (F(....))
	%	rationalise some of these tables
% procedure names are asserted here
?- dynamic isproc/1.

% pure procedure names are asserted here
?- dynamic is_pure/1.

% nonreorderable procedure names are asserted here
% no more, since we olny deal with pure code!

% library procedure names are asserted here

% undefined procedures are asserted here

% what calls what is asserted here
?- dynamic calls/2.

% whens asserted here
?- dynamic whens/1.

% procedures which already have when declarations are asserted here
?- dynamic noaddwhen/1.

% add all when declarations and calculate costs
addwhens :-
	isproc(P),
	P \= $goal(G),
	\+ noaddwhen(P),
	is_pure(P),
	% calls(P, P),	% wastes a bit of time but determinsm must be checked
	addwhen(P),
	fail.
addwhens.

% find which procs are (probably) tests and calculate costs
% put this code in addwhen?
calc_cost :-
	isproc(P),
	P \= $goal(G),
	functor(P, F, N),
	( poss_loop(P) ->
		Cost = 10000
	;	( \+ non_det(P), is_pure(P) ->	% fix code for det with existing whens
			format("% procedure ~a/~d is deterministic.~n", [F, N]),
			C = 100
		;	C = 500
		),
		( nocons(P) ->
			Cost = C
		;	Cost is C + 200
		)
	),
	asserta(cost(P, Cost)),
	fail.
calc_cost.

% reorder RHS of all clauses
ordergoals :-
	isproc(P),
	P \= $goal(G),
	\+ noaddwhen(P),
	is_pure(P),
	reorderproc(P),
	fail.
ordergoals.


% reorder each clause in a procedure
reorderproc(P) :-
	asserta(cost(P, 5000)),	/* so recursive calls are put last */
	% clause(P, G),		% temp fix due to retract bug
	retract((P :- G)),
	reorderg(G, G1),
	(	G \= G1,
		write('% clause altered: '),
		numberVars(P, 0, _),
		writev([quote, cons], P),
		writeln(' :-  . . .'),
		fail
	;	true
	),
	asserta(tmp((P :- G1))),
	fail.
reorderproc(P) :-			/* copy tmp to old */
	retract(cost(P, 5000)),
	retract(tmp(C)),
	asserta(C),
	fail.
reorderproc(P).

	% reorderg should handle call, ;, = etc. too 
	% maybe use Philip's stuff
% reorder RHS of clause - sort wrt costs
reorderg(G, G) :-
	var(G),
	!.
reorderg((A, G), G1) :-
	!,
	reorderg(G, G2),
	hascost(A, C),
	insert(A, C, G2, G1).
reorderg(A, A).

% insert call with given cost into goal
insert(A, C, (A1, G), (A, A1, G)) :-
	hascost(A1, C1),
	C =< C1,
	!.
insert(A, C, (A1, G), (A1, G1)) :-
	insert(A, C, G, G1),
	!.
insert(A, C, A1, (A, A1)) :-
	hascost(A1, C1),
	C =< C1,
	!.
insert(A, C, A1, (A1, A)).

writeheading :-
	format("% the following code has been nac'ed~n% ~n", []).

% write all procedures, including whens, to current output
writeout :-
	nl,
	isproc(P),
	functor(P, F, N),
	(	P = $goal(?-(pure P1/N1)),
		format("?- pure ~a/~d.~n", [P1, N1])
	;	P = $goal(G),
		G \= ?-(pure P1/N1),
		portraygoals(G),
		writeln(.)
	;	P \= $goal(G),
		(	whens((P when B)),	% write out whens
			portraygoals((?- P when B)),
			writeln(.),
			fail
		;	true
		),
		(	clause(P, Q),		% write out clauses
			portraycl((P :- Q)),
			writeln(.),
			fail
		;	nl
		)
	),
	fail.
writeout.

system_pred(P) :-
	functor(P, F, N),
	systemPredicate(F, N).
