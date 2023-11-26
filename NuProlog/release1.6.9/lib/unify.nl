/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus-like unify library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	Unify two terms with the occur check.
unify(Term1, Term2) :-
	var(Term1),
	( nonvar(Term2) ->
		\+ occurs(Term1, Term2)
	),
	Term1 = Term2.
unify(Term1, Term2) :-
	const(Term1),
	Term1 = Term2.
unify(Term1, Term2) :-
	compound(Term1),
	( var(Term2) ->
		\+ occurs(Term2, Term1),
		Term1 = Term2
	;	functor(Term1, Pred, Arity),
		functor(Term2, Pred, Arity),
		$unify(Arity, Term1, Term2)
	).

$unify(N, Term1, Term2) :-
	( N > 0 ->
		arg(N, Term1, Arg1),
		arg(N, Term2, Arg2),
		unify(Arg1, Arg2),
		N1 is N - 1,
		$unify(N1, Term1, Term2)
	).
