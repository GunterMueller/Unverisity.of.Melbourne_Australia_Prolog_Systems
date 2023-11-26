/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus-like termdepth library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	TermDepth/2 computes the depth of a term.
%
%	Unlike the Quintus definition, termDepth/2 is logical, but note that
%	termDepth(f(_), 0) delays rather than failing immediately.  Use
%	depthBound/2 for this effect, possibly combined with termDepth/2 if
%	exact depth is required.
:- termDepth(X, _) when X.
termDepth(X, 0) :-
	const(X).
termDepth(X, D) :-
	compound(X),
	functor(X, _, Arity),
	$termDepth(Arity, X, 0, D).

$termDepth(N, Term, D0, D) :-
	( N > 0 ->
		arg(N, Term, X),
		termDepth(X, D1),
		N1 is N - 1,
		( D0 > D1 ->
			$termDepth(N1, Term, D0, D)
		;	$termDepth(N1, Term, D1, D)
		)
	;	D is D0 + 1
	).

%	DepthBound/2 checks that the depth of a term is no greater than a
%	given maximum.
%
%	Unlike the Quintus definition, depthBound/2 is logical.
:- depthBound(X, D) when X and D.
depthBound(X, D) :-
	const(X),
	D >= 0.
depthBound(X, D) :-
	compound(X),
	D > 0,
	functor(X, _, Arity),
	D1 is D - 1,
	$depthBound(Arity, X, D1).

$depthBound(N, Term, D) :-
	( N > 0 ->
		arg(N, Term, X),
		depthBound(X, D),
		N1 is N - 1,
		$depthBound(N1, Term, D)
	).

%	TermSize/2 computes the size of a term.
%
%	Unlike the Quintus definition, termSize/2 is logical, but note that
%	termSize(f(_), 0) delays rather than failing immediately.
:- termSize(X, _) when X.
termSize(X, 1) :-
	const(X).
termSize(X, S) :-
	compound(X),
	functor(X, _, Arity),
	$termSize(Arity, X, 1, S).

$termSize(N, Term, S0, S) :-
	( N > 0 ->
		arg(N, Term, X),
		termSize(X, XS),
		S1 is S0 + XS,
		N1 is N - 1,
		$termSize(N1, Term, S1, S)
	;	S = S0
	).

%	SizeBound/2 checks that the size of a term is no greater than a
%	given maximum.
%
%	Unlike the Quintus definition, sizeBound/2 is logical, 
%	but it may delay rather than fail immediately in cases like
%	sizeBound(f(a, b, _), 2).
sizeBound(X, B) :-
	$sizeBound(X, B, _).

:- $sizeBound(X, _, _) when X.
$sizeBound(X, B0, B) :-
	const(X),
	B0 >= 1,
	B is B0 - 1.
$sizeBound(X, B0, B) :-
	compound(X),
	B0 > 1,
	B1 is B0 - 1,
	functor(X, _, Arity),
	$sizeBound(Arity, X, B1, B).

$sizeBound(N, Term, B0, B) :-
	( N > 0 ->
		arg(N, Term, X),
		$sizeBound(X, B0, B1),
		N1 is N - 1,
		$sizeBound(N1, Term, B1, B)
	;	B = B0
	).

%	LengthBound/2 checks that a list has length no greater than a given
%	maximum.  If possible, it will instantiate the list to lengths up to
%	that maximum.
:- lengthBound(L, N) when ground(N).
lengthBound([], N) :-
	N >= 0.
lengthBound(_.L, N) :-
	N > 0,
	N1 is N - 1,
	lengthBound(L, N1).
