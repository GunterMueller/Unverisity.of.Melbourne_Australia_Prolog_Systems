/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

%	LOGIC PREPROCESSOR
%
% checks if a procedure with wait declarations
%		a) cannot construct a particular argument (set)
%		b) is locally deterministic
% other code checks if it is deterministic
% This is used for detecting tests, for reordering clause bodies
%
%	Written by Lee Naish

	% no construct test - well, almost
	% each when declaration does not have 'or' in body
	%%% FIX this - should look at heads also and make sure
	% no calls can construct anything - ie, for each clause head,
	% if it was any less general it would delay
	% this is just a heuristic anyway
no_cons((Head when Body).Whens) :-
	no_cons1((Head when Body).Whens).

no_cons1([]).
no_cons1((Head when Body).Whens) :-
	no_or(Body),
	no_cons1(Whens).

	% when body contains no 'or'
no_or(V) :-
	var(V),
	!.
no_or(A) :-
	atomic(A),
	!.
no_or(A and B) :-
	no_or(A),
	no_or(B).

	% is proc locally deterministic (only one clause can match)?
	% this is really horrible code - meta terms and cuts everywhere,
	% but what do you expect for half an hours work when I got
	% hardly any sleep last night?  Come on, give me a break will you.
is_local_det(Whens, [Clause]).			% only one clause
is_local_det(Whens, Clauses) :-
	Clauses = _._,
	Whens = _._,
	\+ local_nondet(Whens, Clauses).

	% see if a call matching two clause heads can satisfy whens
local_nondet(Whens, Clauses) :-
	duplicate(Whens, WhensC),	% needed later an most efficient here
	two_heads(Clauses, H1, H2),
	sat_whens(H1, H2, Whens, WhensC),
	!.

	% return a two heads from a list of clauses
two_heads(Clauses, H1, H2) :-
	append(_, (H1 :- _).ClausesR, Clauses),
	member((H2 :- _), ClausesR).

	% see if any calls matching H1 and H2 will satisfy whens:
	% either some instance matches with no when heads or
	% they matche a when and some instances which are the same
	% for the nonvar terms satisfy the body
sat_whens(H1, H1, Whens, WhensC) :-
	make_ground(H1),
	\+ member((H1 when _), Whens).
sat_whens(H1, H2, Whens1, Whens2) :-
	nth_member(N, W1, Whens1),
	nth_member(N, W2, Whens2),
	W1 = (H1 when B1),
	W2 = (H2 when B2),
	body_sat(B1, T),
	body_sat(B2, T),
	!.

	% bind all vars in A to a funny constant (which will
	% never appear in any when heads!)
make_ground($fuNnY) :-
	!.
make_ground(A) :-
	atomic(A),
	!.
make_ground(A) :-
	A =.. F.Args,
	make_ground_l(Args).

	% as above for list
make_ground_l([]).
make_ground_l(A.Args) :-
	make_ground(A),
	make_ground_l(Args).

	% SHOULD DEAL WITH GROUND/nonvar ??
	% T is the list of terms which need to be ground
	% for the body of a when to be satisfied
body_sat(A, [A]) :-
	var(A),
	!.
body_sat(A and B, T) :-
	!,
	body_sat(A, T1),
	body_sat(B, T2),
	append(T1, T2, T).
body_sat(A or B, T) :-
	!,
	(	body_sat(A, T)
	;
		body_sat(B, T)
	).
body_sat(A, [A]).
	% A ~= (_ and _),
	% A ~= (_ or _).

	% get nth member of list
?- nth_member(A, B, C) when A or C.
nth_member(1, A, A._B).
nth_member(N, A, _B.C) :-
	N > 1,
	plus(M, 1, N),
	nth_member(M, A, C).
