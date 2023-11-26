%	Copyright (C) 1986, The University of Melbourne
%
%	pos.nl
%	Check that each variable occurs positively.  Negative variables are
%	variables which occur in negated predicates only.
%
%	Author: J. Zobel, November 1986
%	Revised December 1987: Justin Zobel, Philip Dart, Giles Lean.


%-- pos_check -- Find clauses, check that variables occur positively.

pos_check(F, A) :-
	forall(dClause(F, A, cl((H :- B), _, _)),
		once (
			find_unique_vars([], (H :- B), [], UVars),
			pos_check(UVars, (H :- B), B, Pos, Neg),
			listOfVars(H, V),
			add_pos(V, Pos, _, Neg, NegVars),
			neg_error(UVars, (H :- B), NegVars)
		)
	).

%-- neg_error -- Note error for each variable in _3 occuring more than once
%		 in original expression.

neg_error(_, _, []).
neg_error(UVars, C, V.Vars) :-
	occurs(V, UVars),
	neg_error(UVars, C, Vars).
neg_error(UVars, (H :- B), V.Vars) :-
	add_error(notpos - (H :- B) - V),
	neg_error(UVars, (H :- B), Vars).

%-- pos_check -- Analyse clause, finding positive and negative vars.

	% Find all -ve vars (_4) in _2. _3 is +ve vars, _1 is the clause
	% being analysed.
pos_check(_, _, B, [], [B]) :-
	var(B),
	!.
pos_check(U, C, B, P, N) :-
	pos_check1(U, C, B, P, N),
	!.
pos_check(_, _, W, V, []) :-
	listOfVars(W, V).

?- pos_check1(_, _, B, _, _) when B.
pos_check1(UVars, C, (V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, not (V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, (V ; W), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, not (V ; W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, (V <= W), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, not (V <= W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, (V => W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, not (V => W), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, (V <=> W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, not (V <=> W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, P1, N1),
	pos_check(UVars, C, not W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN), add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, some(V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, W, P, N),
	listOfVars(V, VList),
	rm_pos(VList, P, Pos, N, Neg, Odd),
	neg_error(UVars, C, Odd).
pos_check1(UVars, C, all(V, not W), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(V, W), Pos, Neg).
pos_check1(UVars, C, all(V, X ~= Y), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(V, X = Y), Pos, Neg).
pos_check1(UVars, C, all(V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not some(V, not W), Pos, Neg).
		% Should complain about oddities here, but they are probably
		% deliberate.
pos_check1(UVars, C, gSome(_, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, W, Pos, Neg).
pos_check1(UVars, C, gAll(V, not W), Pos, Neg) :-
	!,
	pos_check(UVars, C, gSome(V, W), Pos, Neg).
pos_check1(UVars, C, gAll(V, X ~= Y), Pos, Neg) :-
	!,
	pos_check(UVars, C, gSome(V, X = Y), Pos, Neg).
pos_check1(UVars, C, gAll(V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not gSome(V, not W), Pos, Neg).
pos_check1(UVars, C, forall(V, W), Pos, Neg) :-
	!,
	pos_check(UVars, C, gAll(V, V => W), Pos, Neg).
pos_check1(UVars, C, solutions(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, findall(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, gSome(Term, W), Pos, Neg).
pos_check1(UVars, C, count(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, max(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, min(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, sum(Term, V, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some V.Term W, Pos, Neg).
pos_check1(UVars, C, setof(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, bagof(Term, W, _), Pos, Neg) :-
	!,
	pos_check(UVars, C, some(Term, W), Pos, Neg).
pos_check1(UVars, C, (if some(V, A) then W), Pos, Neg) :-
	!,
		% Quantified variables in V may occur positively in A and
		% negatively in W. This is allowed.
	pos_check(UVars, C, not some(V, A), [], N1),
	pos_check(UVars, C, some(V, (A, W)), Pos, N2),
	add_neg(N1, Pos, N2, Neg).
pos_check1(UVars, C, (if V then W), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, [], N1),
	pos_check(UVars, C, W, Pos, N2),
	add_neg(N1, Pos, N2, Neg).
pos_check1(UVars, C, (if some(V, A) then W else X), Pos, Neg) :-
	!,
	pos_check(UVars, C, not some(V, A), [], N1),
	pos_check(UVars, C, (some(V, (A, W)), X), Pos, N2),
	add_neg(N1, Pos, N2, Neg).
pos_check1(UVars, C, (if V then W else X), Pos, Neg) :-
	!,
	pos_check(UVars, C, not V, [], N1),
	pos_check(UVars, C, (W, X), Pos, N2),
	add_neg(N1, Pos, N2, Neg).
pos_check1(UVars, C, (V -> W), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	add_pos(P1, P2, Pos, N2, NN),
	add_neg(N1, Pos, NN, Neg).
pos_check1(UVars, C, (V -> W ; X), Pos, Neg) :-
	!,
	pos_check(UVars, C, V, P1, N1),
	pos_check(UVars, C, W, P2, N2),
	pos_check(UVars, C, X, P3, N3),
	add_pos(P1, P2, PTmp, N2, NTmp1),
	add_neg(N1, PTmp, NTmp1, NTmp2),
	add_pos(P3, PTmp, Pos, NTmp2, NTmp3),
	add_neg(N3, Pos, NTmp3, Neg).
%pos_check1(UVars, C, call(V), Pos, Neg) :-
%	!,
%	pos_check(UVars, C, V, Pos, Neg).
pos_check1(UVars, C, \+ W, [], Neg) :-
	!,
	pos_check(UVars, C, W, P, N),
	add_neg(P, [], N, Neg).
pos_check1(UVars, C, once W, Pos, Neg) :-
	!,
	pos_check(UVars, C, W, Pos, Neg).
pos_check1(_, _, (X ~= Y), [], V) :-
	!,
	listOfVars((X, Y), V).
pos_check1(_, _, not (X ~= Y), V, []) :-
	!,
	listOfVars((X, Y), V).
pos_check1(UVars, C, (not not W), Pos, Neg) :-
	!,
	pos_check(UVars, C, W, Pos, Neg).
pos_check1(UVars, C, not W, [], Neg) :-
	pos_check(UVars, C, W, P, N),
	add_neg(P, [], N, Neg).

%-- add_pos -- Add positive vars to current list of positive, negative vars,
%	       add negative vars similarly.

	% Modify +ve vars _2 and -ve vars _4 with new +ve vars _1. _2 and _4
	% are returned in _3 and _5.
add_pos(V.Vars, Pos, NPos, Neg, NNeg) :-
	(occurs(V, Neg) ->
		rm(V, Neg, NN),
		PP = V.Pos
	; occurs(V, Pos) ->
		PP = Pos, NN = Neg
	;
		PP = V.Pos, NN = Neg
	),
	add_pos(Vars, PP, NPos, NN, NNeg).
add_pos([], Pos, Pos, Neg, Neg).

	% Modify -ve vars _3 with new -ve vars _1 and current +ve vars _2.
	% _3 is returned in _4.
add_neg(V.Vars, Pos, Neg, NNeg) :-
	((occurs(V, Pos), occurs(V, Neg)) ->
		rm(V, Neg, NNeg)
	; (occurs(V, Pos) ; occurs(V, Neg)) ->
		NN = Neg
	;
		NN = V.Neg
	),
	add_neg(Vars, Pos, NN, NNeg).
add_neg([], _, Neg, Neg).

	% _1 are quantified vars which are deleted from +ve vars _2 and -ve
	% vars _4 to give _3 and _5. Any -ve vars in _4 not in _5 are put
	% in _6 as quantified vars should appear positively somewhere.
rm_pos(V.Vars, Pos, NPos, Neg, NNeg, NOdd) :-
	(rm(V, Pos, PP) ->
		NN = Neg,
		NOdd = Odd
	; rm(V, Neg, NN) ->
		PP = Pos,
		NOdd = V.Odd
	;
		PP = Pos, NN = Neg
	),
	rm_pos(Vars, PP, NPos, NN, NNeg, Odd).
rm_pos([], Pos, Pos, Neg, Neg, []).

%-- find_unique_vars -- Find variables which occur once only given expression.

find_unique_vars(Exp, Var, Vars, Var.Vars) :-
	var(Var),
	\+ occurs(Var, Exp),	% Uniquely occuring variable.
	!.
find_unique_vars(_, Var, Vars, Vars) :-
	(	var(Var)
	;	const(Var)
	),
	!.
find_unique_vars(Exp, Term, Vars, NVars) :-
	Term =.. _.Args,	% needed in this form - don't use arg/3
	find_unique_vars_list(Exp, Args, Vars, NVars).

find_unique_vars_list(_, [], Vars, Vars).
find_unique_vars_list(Exp, Term.Terms, Vars, NVars) :-
	find_unique_vars(Terms.Exp, Term, Vars, Tmp),
	find_unique_vars_list(Term.Exp, Terms, Tmp, NVars).
