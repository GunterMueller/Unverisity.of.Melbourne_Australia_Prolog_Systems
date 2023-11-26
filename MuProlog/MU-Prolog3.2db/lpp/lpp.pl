% LOGIC PRE-PROCESSOR
%
% usage: plsave lpp.pl		(creates lpp save file)
%	 lpp < in > out
% ( for non-Berekeley UN*X:
%	 prolog plsave lpp.pl
%	 prolog lpp < in > out )
%
%	adds wait declarations
%	reorders clause bodies
%	expands Extended Prolog clauses
%	warns about undefined procedures and possible infinite loops
%
%	does not detect mutual recursion (yet)
%	does not handle hide/hidden (tricky)
%
%	Originally written by Lee Naish,
%	additions for Extended Prolog and non-logical procedures by John Lloyd,
%	further additions and efficiency mods by Lee Naish

% fudge so we can consult and run the system 'interactively' for testing
% or plsave it for normal use

	% load listing code if we are running plsave
	% path may need to be changed
?- argv(A), A=Plsave.Lpp.Dir, ['/home/munta/staff/jws/src/mup/prolog/listing.pl'].

	% If running interactively, hide the hide predicate and define
	% new version.  (fudge factor 90%)
?- argv(A), A ~= _._._, hide(hide(1)).
?- argv(A), A ~= _._._, assert(hide(_)).

?- hide([lpp(2), readin(1), process(1), processgoal(1), rdin(1),
	libreadin(1), libprocess(1), libprocessgoal(1), note(1), writeheading,
	callcheck(1), defncheck, processcall(1), reordercheck, isproc(1),
	islibproc(1), undefined(1), calls(2), asnoreorder(1), nonreorderable(1),
	addwaits, ordergoals, reorderproc(1), reorderg(2), insert(4), writeout,
	addwait(1), calc_cost, hascost(2), cost(2), non_det(1), nocons(1),
	poss_loop(1), system(1), noreorder(1),
	term_vars(3), list_vars(3), transform(2), free_vars(2), noaddwait(1)]).

%?- op(920, xfy, ->). 	% stuffs this program!
?- op(920, xfy, <-).
?- op(910, xfy, v).
?- op(905, xfy, &).
%?- op(900, fy, ~).	% potentially likewise
?- op(1100, xfy, (:)).
?- op(1100, fx, (:)).

main(_) :-
 	lpp(user, user),
 	exit(0).
main(_) :-
 	writeln(2, error),
 	exit(1).

lpp(F,G) :-
        retractall(main(_)),	% clean up - everything is now hidden!
	op(920, xfy, (->)),
	op(900, fy, (~)),
        telling(T),
        tell(G),
        writeheading,
	readin(F),	% read clauses from file F
        defncheck,      % checks for undefined procedures
        reordercheck,   % checks for procedures which can't be reordered
	addwaits,	% add waits for all procedures
	calc_cost,	% find tests and calculate costs
	ordergoals,	% put generates after tests etc.
	writeout,	% write new program to file G
% (isproc(X), writeln(isproc(X)),fail ; true),
% (cost(X,Y), writeln(cost(X,Y)),fail ; true),
        told,
        tell(T).

% read and assert clauses from file F (handles extended form, if necessary)
readin(F) :-
        seeing(I),
        see(F),
	repeat,
	read(T),
	(	eof(T)
	;
		process(T),
                fail
	),
        seen,
        see(I),
        !.

% process a clause
process((?-G)) :-
	!,
	processgoal(G).

process((H:-T)) :-
	!,
	assert((H:-T)),
        functor(H,F,N),
        functor(H1,F,N),
        note(isproc(H1)),
        callcheck((H1:-T)).

process((H <- T)) :-
        !,
        transform(T, T1),
	assert((H:-T1)),
        functor(H,F,N),
        functor(H1,F,N),
        note(isproc(H1)),
        callcheck((H1:-T1)).

process(F) :-
	assert(F),
        functor(F,F1,N),
        functor(F2,F1,N),
	note(isproc(F2)).

transform(~(V & W), (V1 ; W1)) :-
       transform(~V, V1), transform(~W, W1).

transform(all(Vars, W), W1) :-
       transform(~some(Vars, ~W), W1).

transform(~all(Vars, W), W1) :-
       transform(some(Vars, ~W), W1).

transform((V <- W), (V1 ; W1)) :-
       transform(V, V1), transform(~W, W1).

transform((V -> W), (W1 ; V1)) :-
       transform(W, W1), transform(~V, V1).

transform(~(V <- W), (W1, V1)) :-
       transform(W, W1), transform(~V, V1).

transform(~ (V -> W), (V1, W1)) :-
       transform(V, V1), transform(~W, W1).

transform((V v W), (V1 ; W1)) :-
       transform(V, V1), transform(W, W1).

transform((V & W), (V1, W1)) :-
       transform(V,V1), transform(W, W1).

transform(~(V v W), (V1, W1)) :-
       transform(~V, V1), transform(~W, W1).

transform(~(~(W)), W1) :-
       transform(W, W1).

transform(some(Vars, W), W1) :-
       transform(W, W1).

transform(~some(Vars, W), gnot(G, W1)) :-
       free_vars(some(Vars, W), G),
       transform(W, W1). 

transform(~V, gnot(G, V)) :- 
       V =.. [F | X],
       not occurs(F, [&, v, <-, (->), ~, all, some]),
       free_vars(V, G).

transform(V, V) :-
       V =.. [F | X],
       not occurs(F, [&, v, <-, (->), ~, all, some]).


/*	Find free vars in first order formula		*/

free_vars(T, L) :- term_vars(T, [], L).

/*
 *	term_vars : term, bound_vars -> free_vars_in_term
 */

term_vars(T, _, []) :- var(T), $is_(T), !. 
    
term_vars(T, L, []) :- var(T), occurs(T,L), !.

term_vars(T, _, T) :- var(T), !.
    
term_vars(T, _, []) :- atomic(T), !.

term_vars(T, L, Z) :-
	T =.. [F | A],
	not occurs(F, (all, some)), !,
	list_vars(A, L, Z).

term_vars(all(V, T), L, Z) :- !, term_vars(T, (V, L), Z).

term_vars(some(V, T), L, Z) :- !, term_vars(T, (V, L), Z).

/*
 *	list_vars : (non-empty) term_list, bound_vars -> free_vars_in_term_list
 */

list_vars([T], L, Z) :- term_vars(T, L, Z), !.

list_vars([T | Ts], L, Z) :-
	term_vars(T, L, X),
	list_vars(Ts, L, Y),
	(
	  X == [], Z = Y, !
	;
	  Y == [], Z = X, !
	;
	  Z = (X, Y), !
	).


processgoal(op(X,Y,Z)) :-
        !,
        assert(isproc($goal(?-op(X,Y,Z)))),
        op(X,Y,Z).

processgoal([X|Y]) :-
        !,
        rdin([X|Y]).

processgoal(lib(X)) :-
        !,
        assert(isproc($goal(?-lib(X)))),
	name(X, S),
	libdirectory(LC),
	name(LC, LS),
	$app(LS, 47.S, S1),
	name(N, S1),
        libreadin(N).

processgoal(wait(X)) :-
        !,
        functor(X,F,N),
        functor(Y,F,N),
        note(noaddwait(Y)),
        assert(isproc($goal(?-wait(X)))).

processgoal(G) :-
        assert(isproc($goal(?-G))).

rdin([X|Y]) :-
        readin(X),
        rdin(Y).

rdin([]).

libreadin(F) :-
        seeing(I),
        see(F),
	repeat,
	read(T),
	(	eof(T)
	;
		libprocess(T)
	),
        seen,
        see(I),
	!.

% process a library clause
libprocess((?-G)) :-
	!,
	libprocessgoal(G),
	fail.

libprocess((H:-T)) :-
	!,
	assert((H:-T)),
        functor(H,F,N),
        functor(H1,F,N),
        note(islibproc(H1)),
        callcheck((H1:-T)),
	fail.

libprocess(F) :-
        functor(F,F1,N),
        functor(F2,F1,N),
	note(islibproc(F2)),
	assert(F),
	fail.

libprocessgoal(op(X,Y,Z)) :-
        !,
        op(X,Y,Z).

libprocessgoal(hide(X)) :-
        !,
        hide(X).

libprocessgoal(hidden) :-
        !,
        hidden.

libprocessgoal(lib(X)) :-
        !,
	name(X, S),
	libdirectory(LC),
	name(LC, LS),
	$app(LS, 47.S, S1),
	name(N, S1),
        libreadin(N).

libprocessgoal(X).

note(X) :-
	X,
	!.

note(X) :-
	assert(X).

callcheck((H:-B)) :-
        var(B),
        note(nonreorderable(H)),
        !.

callcheck((H:-(A,B))) :-
        !,
        callcheck((H:-A)),
        callcheck((H:-B)).

callcheck((H:-(A;B))) :-
        !,
        callcheck((H:-A)),
        callcheck((H:-B)).

callcheck((H:-(if X then Y))) :-
        !,
        callcheck((H:-X)),
        callcheck((H:-Y)).

callcheck((H:-(X -> Y))) :-
        !,
        callcheck((H:-X)),
        callcheck((H:-Y)).

callcheck((H:-(if X then Y else Z))) :-
        !,
        callcheck((H:-X)),
        callcheck((H:-Y)),
        callcheck((H:-Z)).

callcheck((H:-(X -> Y ; Z))) :-
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

defncheck :-
        calls(_,Y),
        processcall(Y),
        fail.

defncheck.

processcall(Y) :-
        isproc(Y),
        !.
        
processcall(Y) :-
        islibproc(Y),
        !.
        
processcall(Y) :-
        system(Y),
        !.

processcall(Y) :-
        not undefined(Y),
        note(undefined(Y)),
        functor(Y,F,N),
        write('% procedure '),
        write(F),
	write(/),
	write(N),
        writeln(' is not defined').

reordercheck :-
	noreorder(N),
	calls(P, N),
	asnoreorder(P),
	fail.
reordercheck.

asnoreorder(P) :-
	nonreorderable(P),
	!.
asnoreorder(P) :-
	asserta(nonreorderable(P)),
	calls(Q, P),
	asnoreorder(Q),
	fail.
asnoreorder(P).

% procedure names are asserted here
isproc(true).
?- retract(isproc(true)).

% nonreorderable procedure names are asserted here
nonreorderable(true).
?- retract(nonreorderable(true)).

% library procedure names are asserted here
islibproc(true).
?- retract(islibproc(true)).

% undefined procedures are asserted here
undefined(true).
?- retract(undefined(true)).

% what calls what is asserted here
calls(true, true).
?- retract(calls(true, true)).

% procedures which already have wait declarations are asserted here
noaddwait(true).
?- retract(noaddwait(true)).

% add all wait declarations and calculate costs
addwaits :-
	isproc(P),
        not noaddwait(P),
	% calls(P, P),	% wastes a bit of time but determinsm must be checked
	addwait(P),
	fail.
addwaits.

% find which procs are (probably) tests and calculate costs
% put this code in addwait?
calc_cost :-
	isproc(P),
	functor(P, F, N),
	( poss_loop(P) ->
		Cost = 10000
	;
		( not non_det(P) ->
			write('% procedure '),
			write(F),
			write(/),
			write(N),
			writeln(' is deterministic'),
			C = 100
		;
			C = 500
		),
		( nocons(P) ->
			Cost = C
		;
			Cost is C + 200
		)
	),
	asserta(cost(P, Cost)),
	fail.
calc_cost.
	

% reorder RHS of all clauses
ordergoals :-
	isproc(P),
        P \= $goal(G),
        not nonreorderable(P),
	reorderproc(P),
	fail.
ordergoals.


% reorder each clause in a procedure
reorderproc(P) :-
	asserta(cost(P, 5000)),	/* so recursive calls are put last */
	retract((P :- G)),
	reorderg(G, G1),
	(	G \= G1,
		write('% clause altered: '),
		writef(P, 2'110110),
		writeln(' :-  . . .'),
		fail
	;
		true
	),
	asserta(newproc((P :- G1))),
	fail.
reorderproc(P) :-			/* copy newproc to old */
	retract(cost(P, 5000)),
	retract(newproc(C)),
	asserta(C),
	fail.
reorderproc(P).

		/* reorderg should handle call, ;, = etc. too */
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
        writeln('% output from logic preprocessor'),
        writeln('% ').

% write all procedures, including waits, to file G
writeout :-
        nl,
	isproc(P),
        (
               P=$goal(G),
               nl,
               portraygoals(G),
               writeln(.)
        ;
               P\=$goal(G),
	       functor(P, F, N),
	       F1 =.. [F,N],
	       listing(F1)		/* lists clauses and waits of P */
        ),
	fail.
writeout :-
	calls(_, gnot(_,_)),
	!,
	nl,
        portraygoals(?-lib(gnot)),
        writeln(.).
writeout.

?- [system].
?- [noreorder].
?- [addwait].

?- hidden.
