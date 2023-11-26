%	Copyright (C) 1986, The University of Melbourne
%
%	varcheck.nl
%	Check that the same variable name is not used in distinct scopes,
%	and that all variables occur more than once in each clause.
%
%	Author: J. Zobel, November 1986
%	Revised December 1987: Justin Zobel, Philip Dart, Giles Lean.


%-- scope_check -- Ensure that same variable name not used in distinct scopes.

?- scope_check(F, A) when F and A.
scope_check(F, A) :-
	forall(dClause(F, A, cl((H :- B), _, _)),
		once find_scope_vars(B, H, (H :- B))
	).

	% _1 is the subgoal being checked; _2 is the remainder of the
	% clause. _3 is the clause. (_3 is used for reporting errors).
%	BUG!  Our old friends if(_), else(_), and else(if(_)) stuff this up?
?- find_scope_vars(_, X, Y) when X and Y.
find_scope_vars(B, _, _) :-
	var(B),
	!.
find_scope_vars(some(V, W), Exp, C) :-
	find_error_vars(V, Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars(all(V, W), Exp, C) :-
	find_error_vars(V, Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars(solutions(Term, some(Vars, W), _), Exp, C) :-
	find_error_vars(Term.Vars, Exp, C),
	find_scope_vars(W, Term.Vars.Exp, C).
find_scope_vars(solutions(Term, W, _), Exp, C) :-
	find_error_vars(Term, Exp, C),
	find_scope_vars(W, Term.Exp, C).
find_scope_vars((if some(V, A) then W), Exp, C) :-
	find_error_vars(V, Exp, C),
	find_scope_vars(A, V.W.Exp, C),
	find_scope_vars(W, V.A.Exp, C).
find_scope_vars((if some(V, A) then W else X), Exp, C) :-
	find_error_vars(V, X.Exp, C),
	find_scope_vars(A, V.W.X.Exp, C),
	find_scope_vars(W, V.A.X.Exp, C),
	find_scope_vars(X, V.A.W.Exp, C).
find_scope_vars((V, W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((V ; W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((V <= W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((V => W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((V <=> W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars(gSome(V, W), Exp, C) :-
	find_scope_vars(W, V.Exp, C).
find_scope_vars(gAll(V, W), Exp, C) :-
	find_scope_vars(W, V.Exp, C).
%	BUG!  When did this get into NU-Prolog?
find_scope_vars(forall(V, W), Exp, C) :-
	find_scope_vars((V, W), Exp, C).
find_scope_vars(findall(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars(count(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars(min(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars(max(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars(sum(T1, T2, W, _), Exp, C) :-
	find_scope_vars(W, T1.T2.Exp, C).
find_scope_vars(setof(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars(bagof(Term, W, _), Exp, C) :-
	find_scope_vars(W, Term.Exp, C).
find_scope_vars((if V then W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((if V then W else X), Exp, C) :-
	find_scope_vars(V, W.X.Exp, C),
	find_scope_vars(W, V.X.Exp, C),
	find_scope_vars(X, V.W.Exp, C).
find_scope_vars((V -> W), Exp, C) :-
	find_scope_vars(V, W.Exp, C),
	find_scope_vars(W, V.Exp, C).
find_scope_vars((V -> W ; X), Exp, C) :-
	find_scope_vars(V, W.X.Exp, C),
	find_scope_vars(W, V.X.Exp, C),
	find_scope_vars(X, V.W.Exp, C).
find_scope_vars(call(V), Exp, C) :-	 % ??
	find_scope_vars(V, Exp, C).
find_scope_vars(not W, Exp, C) :-
	find_scope_vars(W, Exp, C).
find_scope_vars(\+ W, Exp, C) :-
	find_scope_vars(W, Exp, C).
find_scope_vars(once W, Exp, C) :-
	find_scope_vars(W, Exp, C).
find_scope_vars(_, _, _).

find_error_vars(V, Exp, C) :-
	listOfVars(V, Vars),
	find_scope_errors(Vars, Exp, C).

	% If the given variable occurs in the expression, note an error.
find_scope_errors([], _, _).
find_scope_errors(V.Vars, Exp, (H :- B)) :-
	occurs(V, Exp),
	add_error(distinct - (H :- B) - V),
	find_scope_errors(Vars, Exp, (H :- B)).
find_scope_errors(V.Vars, Exp, C) :-
	\+ occurs(V, Exp),
	find_scope_errors(Vars, Exp, C).

%-- used_var -- Check that each variable is used more than once.

?- used_var(F, A) when F and A.
used_var(F, A) :-
	forall(dClause(F, A, cl((H :- B), N, V)),
		once used_var(N, V, H, B)
	).

used_var(N, V, H, B) :-
	list_of_all_vars((H, B), Vs),
	once_only_vars(N, V, Vs, [], [], Once),
	(Once = _._ ->
		add_error(single_occur - (H :- B) - Once)
	),
	more_than_once(N, V, Vs, [], [], Res),
	(Res = _._ ->
		add_error(too_often - (H :- B) - Res)
	).

	% Like listOfVars, but don't remove duplicates.
list_of_all_vars(T, V) :- once list_of_all_vars(T, [], V).

list_of_all_vars(V, L, V.L) :-
	var(V).
list_of_all_vars(T, L, L) :-
	ground(T).
list_of_all_vars(T, LIn, LOut) :-
	functor(T, _, A),
	list_of_all_vars_args(A, T, LIn, LOut).

list_of_all_vars_args(0, _, L, L).
list_of_all_vars_args(A, T, LIn, LOut) :-
	arg(A, T, SubT),
	list_of_all_vars(SubT, LIn, L),
	A1 is A - 1,
	list_of_all_vars_args(A1, T, L, LOut).

	% _3 is a list of variables, _4 is a list of variables which have
	% occurred once, _5 is a list of variables which have occurred
	% more than once. _6 is the final version of _4.
	% _1 and _2 are names and variables (not underscore) of _3.
once_only_vars(_, _, [], Res, _, Res).
once_only_vars(Names, Vars, V.Vs, First, Ok, Res) :-
	\+ not_uscore(V, Names, Vars),
	!,
	once_only_vars(Names, Vars, Vs, First, Ok, Res).
once_only_vars(Names, Vars, V.Vs, First, Ok, Res) :-
	rm(V, First, NFirst),
	!,
	once_only_vars(Names, Vars, Vs, NFirst, V.Ok, Res).
once_only_vars(Names, Vars, V.Vs, First, Ok, Res) :-
	occurs(V, Ok),
	!,
	once_only_vars(Names, Vars, Vs, First, Ok, Res).
once_only_vars(Names, Vars, V.Vs, First, Ok, Res) :-
	once_only_vars(Names, Vars, Vs, V.First, Ok, Res).

	% Like above, but _6 is a copy of the final version of _5, and
	% only variables whose names begin with _ are considered.
more_than_once(_, _, [], _, Res, Res).
more_than_once(Names, Vars, V.Vs, First, Ok, Res) :-
%	Ignore '_' variables in case expandTerm duplicates some of them.  (JWS)
%	BUG?  Is this correct?
	\+ occurs(V, Vars),
	!,
	more_than_once(Names, Vars, Vs, First, Ok, Res).
more_than_once(Names, Vars,  V.Vs, First, Ok, Res) :-
	not_uscore(V, Names, Vars),		% ignore if not u_score
	!,
	more_than_once(Names, Vars, Vs, First, Ok, Res).
more_than_once(Names, Vars, V.Vs, First, Ok, Res) :-
	rm(V, First, NFirst),
	!,
	more_than_once(Names, Vars, Vs, NFirst, V.Ok, Res).
more_than_once(Names, Vars, V.Vs, First, Ok, Res) :-
	occurs(V, Ok),
	!,
	more_than_once(Names, Vars, Vs, First, Ok, Res).
more_than_once(Names, Vars, V.Vs, First, Ok, Res) :-
	more_than_once(Names, Vars, Vs, V.First, Ok, Res).

%-- name_check --

?- name_check(F, A) when F and A.
name_check(F, A) :-
	forall((dClause(F, A, cl(C, N, V)), flatten(N, V, N1, V1)),
		find_name_errors(N1, V1, C)
	).

?- flatten(L1, L2, _, _) when L1 and L2.
flatten([], [], [], []).
flatten(N.IL1, V.IL2, N1.OL1, V.OL2) :-
	upper_to_lower(N, S1),
	atomToString(N1, S1),
	flatten(IL1, IL2, OL1, OL2).

?- upper_to_lower(L, _) when L.
upper_to_lower([], []).
upper_to_lower(0'_.L, L1) :-
	upper_to_lower(L, L1).
upper_to_lower(C.L, C.L1) :-
	(C < 0'A or C > 0'Z),
	upper_to_lower(L, L1).
upper_to_lower(C.L, C1.L1) :-
	C >= 0'A and C =< 0'Z,
	C1 is C - 0'A + 0'a,
	upper_to_lower(L, L1).

?- find_name_errors(N, V, _) when N and V.
find_name_errors([], [], _).
find_name_errors(N.L1, V.L2, C) :-
	similar(N, V, V1, L1, L2),
	add_error(same_name - C - [V, V1]),
	find_name_errors(L1, L2, C).
find_name_errors(_.L1, _.L2, C) :-
	find_name_errors(L1, L2, C).

similar(N, V, V1, N._, V1._) :-
	V \== V1.
similar(N, V, Res, N1.L1, _.L2) :-
	N \= N1,
	similar(N, V, Res, L1, L2).
