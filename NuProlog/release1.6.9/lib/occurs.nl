/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus-like occurs library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	These predicates are non-logical.  At some time, it's
%	probable that some of them will become logical in much the same
%	way that if_then_else is a logical ->_;.  If you really mean the
%	non-logical operations, use library(quintus/occurs).

%	Two terms have no variable in common.
%	This is not present in quintus/occurs.nl.
%	Non-logical.
noCommonVariable(Term1, Term2) :-
	var(Term1),
	\+ occurs(Term1, Term2).
noCommonVariable(Term1, _) :-
	const(Term1).
noCommonVariable(Term1, Term2) :-
	compound(Term1),
	functor(Term1, _, Arity),
	$noCommonVariable(Arity, Term1, Term2).

$noCommonVariable(N, Term1, Term2) :-
	( N > 0 ->
		arg(N, Term1, X),
		noCommonVariable(X, Term2),
		N1 is N - 1,
		$noCommonVariable(N1, Term1, Term2)
	).

%	ContainsTerm/2 tests if one term unifies with some sub-term of another.
%	It binds SubTerm to each sub-term of Term to cause any constraints to
%	run, but leaves none of these trial bindings on success.
%	Non-logical.
containsTerm(SubTerm, Term) :-
	( const(SubTerm), occurs(SubTerm, Term) ->
		true					% Note that vars in Term may have constraints.
%	;	$containsTerm(SubTerm, Term)
	;	\+ freeOfTerm(SubTerm, Term)
	).

%$containsTerm(SubTerm, Term) :-
%	( SubTerm \= Term ->
%		functor(Term, _, Arity),
%		between(1, Arity, N),
%		arg(N, Term, X),
%		$containsTerm(SubTerm, X)
%	).

%	ContainsVar/2 tests if one term is identical to some sub-term of another.
%	Non-logical.
containsVar(SubTerm, Term) :-
	( compound(SubTerm) ->
%		$containsVar(SubTerm, Term)
		once (
			subTerm(X, Term),
			X == SubTerm
		)
	;	occurs(SubTerm, Term)
	).

%$containsVar(SubTerm, Term) :-
%	( compound(Term), SubTerm \== Term ->
%		functor(Term, _, Arity),
%		between(1, Arity, N),
%		arg(N, Term, X),
%		$containsVar(SubTerm, X)
%	).

%	FreeOfTerm/2 checks that one term unifies with no sub-term of another.
%	Non-logical.
freeOfTerm(SubTerm, Term) :-
	\+ subTerm(SubTerm, Term).

%	FreeOfVar/2 checks that one term is identical to no sub-term of another.
%	Non-logical.
freeOfVar(SubTerm, Term) :-
	( compound(SubTerm) ->
%		$freeOfVar(SubTerm, Term)
		\+ (
			subTerm(X, Term),
			X == SubTerm
		)
	;	\+ occurs(SubTerm, Term)
	).

%$freeOfVar(SubTerm, Term) :-
%	( compound(Term) ->
%		SubTerm \== Term,
%		functor(Term, _, Arity),
%		$freeOfVar(Arity, SubTerm, Term)
%	).
%
%$freeOfVar(N, SubTerm, Term) :-
%	( N > 0 ->
%		arg(N, Term, X),
%		$freeOfVar(SubTerm, X),
%		N1 is N - 1,
%		$freeOfVar(N1, SubTerm, Term)
%	).

%	OccurrencesOfTerm(SubTerm, Term, Count) counts the number of sub-terms 
%	of Term that unify with SubTerm.
%	Non-logical.
occurrencesOfTerm(SubTerm, Term, Count) :-
	occurrencesOfMatch(=(SubTerm), Term, Count).

%	OccurrencesOfVar(SubTerm, Term, Count) counts the number of sub-terms 
%	of Term that are identical to SubTerm.
%	Non-logical.
occurrencesOfVar(SubTerm, Term, Count) :-
	occurrencesOfMatch(==(SubTerm), Term, Count).

%	OccurrencesOfMatch(Pred, Term, Count) counts the number of sub-terms 
%	SubTerm of Term for which call(Pred, SubTerm) succeeds.
%	It ensures that the Pred doesn't permanently bind anything.
%	Non-logical.
occurrencesOfMatch(Pred, Term, Count) :-
	$occurrencesOfMatch(Pred, Term, 0, Count).

$occurrencesOfMatch(Pred, Term, Count0, Count) :-
	( \+ call(Pred, Term) ->
		Count1 = Count0
	;	Count1 is Count0 + 1
	),
	( compound(Term) ->
		functor(Term, _, Arity),
		$occurrencesOfMatch(Arity, Pred, Term, Count1, Count)
	;	Count = Count1
	).

$occurrencesOfMatch(N, Pred, Term, Count0, Count) :-
	( N > 0 ->
		arg(N, Term, X),
		$occurrencesOfMatch(Pred, X, Count0, Count1),
		N1 is N - 1,
		$occurrencesOfMatch(N1, Pred, Term, Count1, Count)
	;	Count0 = Count
	).

%	ContainsMatch(Pred, Term) checks that no sub-term SubTerm of Term has
%	call(Pred, SubTerm) true.
%	This is not present in quintus/occurs.nl.
%	Non-logical.
containsMatch(Pred, Term) :-
	\+ freeOfMatch(Pred, Term).

%	FreeOfMatch(Pred, Term) checks that no sub-term SubTerm of Term has
%	call(Pred, SubTerm) true.
%	This is not present in quintus/occurs.nl.
%	Non-logical.
freeOfMatch(Pred, Term) :-
	\+ (
		subTerm(SubTerm, Term),
		call(Pred, SubTerm)
	).

%	SubTerm/2 enumerates the sub-terms of a term, much as member/2
%	enumerates the elements of a list.  The order in which sub-terms are
%	found is not defined.
%	Non-logical.
subTerm(SubTerm, Term) :-
	$subTerm(Term, SubTerm).

$subTerm(Term, Term).
$subTerm(Term, SubTerm) :-
	compound(Term),
	functor(Term, _, Arity),
	between(1, Arity, N),
	arg(N, Term, X),
	$subTerm(X, SubTerm).
