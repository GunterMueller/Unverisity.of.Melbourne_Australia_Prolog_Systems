/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible termdepth library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

:- initializing, ensureLoaded(library(termdepth)).

%	Term_depth/2 computes the depth of a term.
%
%	Non-logical.
term_depth(X, 0) :-
	var(X).
term_depth(X, 0) :-
	const(X).
term_depth(X, D) :-
	compound(X),
	functor(X, _, Arity),
	$term_depth(Arity, X, 0, D).

$term_depth(N, Term, D0, D) :-
	( N > 0 ->
		arg(N, Term, X),
		term_depth(X, D1),
		N1 is N - 1,
		( D0 > D1 ->
			$term_depth(N1, Term, D0, D)
		;	$term_depth(N1, Term, D1, D)
		)
	;	D is D0 + 1
	).

%	Depth_bound/2 checks that the depth of a term is no greater than a
%	given maximum.
%
%	Non-logical.
depth_bound(X, D) :-
	var(X),
	D >= 0.
depth_bound(X, D) :-
	const(X),
	D >= 0.
depth_bound(X, D) :-
	compound(X),
	D > 0,
	functor(X, _, Arity),
	D1 is D - 1,
	$depth_bound(Arity, X, D1).

$depth_bound(N, Term, D) :-
	( N > 0 ->
		arg(N, Term, X),
		depth_bound(X, D),
		N1 is N - 1,
		$depth_bound(N1, Term, D)
	).

%	Term_size/2 computes the size of a term.
%
%	Non-logical.
term_size(X, 0) :-
	var(X).
term_size(X, 1) :-
	const(X).
term_size(X, S) :-
	compound(X),
	functor(X, _, Arity),
	$term_size(Arity, X, 1, S).

$term_size(N, Term, S0, S) :-
	( N > 0 ->
		arg(N, Term, X),
		term_size(X, XS),
		S1 is S0 + XS,
		N1 is N - 1,
		$term_size(N1, Term, S1, S)
	;	S = S0
	).

%	SizeBound/2 checks that the size of a term is no greater than a
%	given maximum.
%
%	Non-logical.
size_bound(X, B) :-
	$size_bound(X, B, _).

$size_bound(X, B, B) :-
	var(X),
	B >= 0.
$size_bound(X, B0, B) :-
	const(X),
	B0 >= 1,
	B is B0 - 1.
$size_bound(X, B0, B) :-
	compound(X),
	B0 > 1,
	B1 is B0 - 1,
	functor(X, _, Arity),
	$size_bound(Arity, X, B1, B).

$size_bound(N, Term, B0, B) :-
	( N > 0 ->
		arg(N, Term, X),
		$size_bound(X, B0, B1),
		N1 is N - 1,
		$size_bound(N1, Term, B1, B)
	;	B = B0
	).

%	Length_bound/2 checks that a list has length no greater than a given
%	maximum.  If possible, it will instantiate the list to lengths up to
%	that maximum.
length_bound(L, N) :-
	lengthBound(L, N).
