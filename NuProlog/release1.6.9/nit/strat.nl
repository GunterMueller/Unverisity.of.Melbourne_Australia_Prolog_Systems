%	Copyright (C) 1986, The University of Melbourne
%
%	stratcheck.nl
%	Check that program is stratified, that is, that there are no cases
%	of negative (mutual) recursion. First phase is to construct a graph
%	of what calls what, with negations and universal quantifications
%	marked and other system facilities ignored. This graph is then
%	searched for negative recursions.
%
%	Author: J. Zobel, November 1986

%-- strat_check --

% Because a complete graph must be constructed before any
% stratification check is done, three separate versions of
% strat_check are required.

	% Do stratification check for all predicates in domain.
strat_check :-
	strat_calls,
	forall((dPred(F, A), F/A \= $goal/0),
		do_strat_check([F/A], F/A)
	).

	% Do stratification check for all predicates in File.
?- strat_check(File) when File.
strat_check(File) :-
	strat_calls(File),
	forall((dFile(F, A, File), F/A \= $goal/0),
		do_strat_check([F/A], F/A)
	).

	% Do stratification check on F/A.
?- strat_check(F, A) when F and A.
strat_check(F, A) :-
	strat_calls(F, A),
	do_strat_check([F/A], F/A).

?- do_strat_check(Anc, Pred) when Anc and Pred.
do_strat_check(Anc, not F/A) :-
	forall(dProp(F, A, $strat_not, P), (
		dRmProp(F, A, $strat_not, P),
		check_strat_goal(Anc, P)
	)).
do_strat_check(Anc, F/A) :-
	forall(dProp(F, A, $strat, P), (
		dRmProp(F, A, $strat, P),
		check_strat_goal(Anc, P)
	)).

?- check_strat_goal(Anc, Pred) when Anc and Pred.
check_strat_goal(Anc, P) :-
	( (P = F/A, split(P, Anc, A1), member(not _, A1))
	->	delete_all_not(A1, A2),
		add_error(negative - F.A.A2)
	;	( \+ member(P, Anc)
		->	do_strat_check(P.Anc, P)
		)
	).

	% Split list _2 at _1, giving head _3. _1 is left at the end of _3.
?- split(Pred, List, _) when Pred and List.
split(P, P._, [P]).
split(P, Q.L, Q.L1) :-
	P \= Q,
	split(P, L, L1).

delete_all_not(List, Res) :-
	(delete(not _, List, R1)
	->	delete_all_not(R1, Res)
	;	Res = List
	).

%-- strat_calls -- Construct a graph in strat_call/2 of what calls what.

strat_calls :-
	forall(dClause(F, A, cl((_ :- B), _, _)),
		once do_strat_calls(F/A, B)
	).

strat_calls(File) :-
	forall((dFile(F, A, File), dClause(F, A, cl((_ :- B), _, _))),
		once do_strat_calls(F/A, B)
	).

strat_calls(F, A) :-
	forall(dClause(F, A, cl((_ :- B), _, _)),
		once do_strat_calls(F/A, B)
	).

	% Analyse the body of the given clause.
?- do_strat_calls(X, _) when X.
do_strat_calls(_, B) :-
	var(B).
do_strat_calls(H, P) :-
	do_strat_calls1(H, P).
do_strat_calls(_, P) :-
	functor(P, F, A),
	systemPredicate(F, A).
do_strat_calls(_, P) :-
	functor(P, F, A),
	getprop($nit_lib, $lib, Library),
	libraryPredicate(Library, F, A).
do_strat_calls(not F1/A1, P) :-
	functor(P, F, A),
	dAddProp(F1, A1, $strat_not, F/A).
do_strat_calls(F1/A1, P) :-
	functor(P, F, A),
	dAddProp(F1, A1, $strat, F/A).

?- do_strat_calls1(X, Y) when X and Y.
do_strat_calls1(H, (A => B)) :-
	do_strat_calls(H, not A), do_strat_calls(H, B).
do_strat_calls1(H, (A <= B)) :-
	do_strat_calls(H, A), do_strat_calls(H, not B).
do_strat_calls1(H, (A <=> B)) :-
	do_strat_calls(H, not A), do_strat_calls(H, not B).
do_strat_calls1(H, (A, B)) :-
	do_strat_calls(H, A), do_strat_calls(H, B).
do_strat_calls1(H, (A ; B)) :-
	do_strat_calls(H, A), do_strat_calls(H, B).
do_strat_calls1(H, (if A then B)) :-
	do_strat_calls(H, not A), do_strat_calls(H, B).
do_strat_calls1(H, (if A then B else C)) :-
	do_strat_calls(H, not A), do_strat_calls(H, B),
	do_strat_calls(H, C).
do_strat_calls1(H, (A -> B)) :-
	do_strat_calls(H, not A), do_strat_calls(H, B).
do_strat_calls1(H, (A -> B ; C)) :-
	do_strat_calls(H, not A), do_strat_calls(H, B),
	do_strat_calls(H, C).
%do_strat_calls1(H, call(A)) :-
%	do_strat_calls(H, A).
do_strat_calls1(H, once A) :-
	do_strat_calls(H, A).
do_strat_calls1(not F1/A1, not A) :-
	dAddProp(F1, A1, $strat_not, not F1/A1),
	do_strat_calls(not F1/A1, A).
do_strat_calls1(F1/A1, not A) :-
	dAddProp(F1, A1, $strat, not F1/A1),
	do_strat_calls(not F1/A1, A).
do_strat_calls1(H, \+ A) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, some(_, A)) :-
	do_strat_calls(H, A).
do_strat_calls1(H, gSome(_, A)) :-
	do_strat_calls(H, A).
do_strat_calls1(H, all(_, A)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, gAll(_, A)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, forall(A, B)) :-
	do_strat_calls(H, not (A, B)).
do_strat_calls1(H, solutions(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, findall(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, max(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, min(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, count(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, sum(_, _, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, setof(_, A, _)) :-
	do_strat_calls(H, not A).
do_strat_calls1(H, bagof(_, A, _)) :-
	do_strat_calls(H, not A).
