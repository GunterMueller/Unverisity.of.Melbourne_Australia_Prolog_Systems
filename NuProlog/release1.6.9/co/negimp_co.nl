/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Lee Naish
 */

%	Flag true when compiler negation is being used.
$compilerNegation.

	% Negation implementation (compiler version).

	% not (could just use special case of if)
	% G=vars which should be ground
	% NA=all global vars
	% C=negated call
$imp_not([], _, C, \+ C, []).
$imp_not(G, NA, C, CNew, [Defn]) :-
	term(G),
	$new_pred(G, NA, (\+ C), CNew, Defn).

	% if-then-else with no shared quantified vars (-> hard cut)
	% G=vars which should be ground
	% SV=all global vars
	% C,T,E=condition,then,else
$imp_if([], _SV, C, T, E, (C -> T ; E), []).
$imp_if(G, SV, C, T, E, CNew, [Defn]) :-
	term(G),
	$new_pred(G, SV, (C -> T ; E), CNew, Defn).

	% if-then-else with shared quantified vars (-> soft cut)
	% G=vars which should be ground
	% SV=all global vars
	% C,T,E=condition,then,else
$imp_if_soft([], _SV, C, T, E, ($label(L), C, $softCut(L), T ; E), []).
$imp_if_soft(G, SV, C, T, E, CNew, [Defn]) :-
	term(G),
	$new_pred(G, SV, ($label(L), C, $softCut(L), T ; E), CNew, Defn).

	% $is_eq
	% A=local (universally quantified) var
	% G_A= gAll vars
	% T1,T2=terms to be compared
	% R=result of comparison (true or fail)
	% last arg should probably be removed
$imp_is_eq(_G, G_A, T1, T2, R, (true, (ISEQ ; EQ)), []) :-
	$is_eq_header(G_A, $is_eq(T1, T2, R), ISEQ),
	( R == fail ->
		EQ = fail
	;	EQ = (R=true)
	).

	% some (not actually needed at the moment?)
	% G=vars to be tested for groundness
	% C=call
	% (maybe should produce auxilliary pred like not -
	%  would need extra arg like $imp_not)
%$imp_some([], C, once(C), []).
%$imp_some(G, C, (ground(GS) -> once(C) ; C), []) :-
%	term(G),
%	$squash_list(G, GS).

	% solutions (not needed at the moment either)
	% should be here for unsound nondelaying negpp
	% which translates into setof.
%	Note that $solutions, $limit, and $sum must be passed a simple goal,
%	both for efficiency and to prevent compiler-style transformations'
%	being run by the interpreter.
$imp_solutions(G, L, T, C, S, $solutions(G, T, CNew, S), [Defn]) :-
	$new_pred([], L, C, CNew, Defn).

$imp_limit(G, NA, Sign, T, C, S, $limit(G, Sign, T, CNew, S), [Defn]) :-
	$new_pred([], NA, C, CNew, Defn).

$imp_sum(G, NA, Count, T, C, S, $sum(G, Count, T, CNew, S), [Defn]) :-
	$new_pred([], NA, C, CNew, Defn).

	% G=var which should be bound
	% NA=all global vars
	% C=goal
$imp_freeze(G, _NA, C0, C, D) :-
	nonvar(G),
	C0 = C,
	D = [].
$imp_freeze(G, NA, C0, C, D) :-
	var(G),
	D = [Defn],
	$new_freeze_pred(G, NA, C0, C, Defn).
