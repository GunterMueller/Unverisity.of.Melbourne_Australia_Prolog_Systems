/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 *
 * All rights are reserved.
 */

%   Flag true when compiler negation is being used.
$compilerNegation :-
	fail.

	% Negation implementation (interpreter/call version).

	% not (could just use special case of if)
	% G=vars which should be ground
	% NA=all global vars
	% C=negated call
$imp_not([], _, C, \+ C, []) :-
	!.
$imp_not(G, _NA, C, $if(GS, C, fail, true), []) :-
	$squash_list(G,GS).

	% if-then-else with no shared quantified vars (-> hard cut)
	% G=vars which should be ground
	% SV=all global vars
	% C,T,E=condition,then,else
$imp_if(G, _SV, C, T, E, $if(GS, C, T, E), []):-
	$squash_list(G,GS).

	% if-then-else with shared quantified vars (-> soft cut)
	% G=vars which should be ground
	% SV=all global vars
	% C,T,E=condition,then,else
$imp_if_soft(G, _SV, C, T, E, $if_soft(GS, C, T, E), []):-
	$squash_list(G,GS).

	% $is_eq
	% A=local (universally quantified) var
	% G_A= gAll vars
	% T1,T2=terms to be compared
	% R=result of comparison (true or fail)
	% last arg should probably be removed
$imp_is_eq(G, G_A, T1, T2, R, $call_is_eq(GS,T1,T2,R), []) :-
	append(G, G_A, G1),
	$squash_list(G1,GS).

	% some (not actually needed at the moment?)
	% G=vars to be tested for groundness
	% C=call
	% (maybe should produce auxilliary pred like not -
	%  would need extra arg like $imp_not)
%$imp_some([], C, once(C), []) :-
%	!.
%$imp_some(G, C, (ground(GS) -> once(C) ; C), []) :-
%	$squash_list(G, GS).

	% solutions (not needed at the moment either)
	% should be here for unsound nondelaying negpp
	% which translates into setof.
%	Needed to prevent compiler-style transformations' being run by
%	the interpreter.
$imp_solutions(G, _L, T, C, S, $solutions(GS, T, C, S), []) :-
	$squash_list(G, GS).

%	Needed to prevent compiler-style transformations' being run by
%	the interpreter.
$imp_limit(G, _L, Sign, T, C, S, $limit(GS, Sign, T, C, S), []) :-
	$squash_list(G, GS).

%	Needed to prevent compiler-style transformations' being run by
%	the interpreter.
$imp_sum(G, _L, Count, T, C, S, $sum(GS, Count, T, C, S), []) :-
	$squash_list(G, GS).

%	Needed to prevent compiler-style transformations' being run by
%	the interpreter.
$imp_freeze(G, _L, C, freeze(G, C), []).
