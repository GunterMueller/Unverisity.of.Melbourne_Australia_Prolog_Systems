/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus-like arg library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	Arg0/3 is like arg/3 except that arg(0, f(_, . . ., _), f).
%	The Quintus definition reports instantiation errors rather than
%	failing silently.
:- arg0(N, T, _) when N and T.
arg0(N, T, X) :-
	( N == 0 ->
		functor(T, X, _)
	;	arg(N, T, X)
	).

%	Genarg/3 is like arg/3 except that it backtracks through the arguments
%	if the index isn't given.
:- genarg(_, T, _) when T.
genarg(N, T, X) :-
	functor(T, _, Arity),
	between(1, Arity, N),
	arg(N, T, X).

%	Genarg0/3 is like arg0/3 except that it backtracks through the arguments
%	if the index isn't given.
:- genarg0(_, T, _) when T.
genarg0(N, T, X) :-
	functor(T, _, Arity),
	between(0, Arity, N),
	arg0(N, T, X).

%	Args(N, Terms, Args) is equivalent to mapList(genarg(N), Terms, Args).
:- args(_, T, A) when T or A.
args(_, [], []).
args(N, T0.T, A0.A) :-
	genarg(N, T0, A0),
	args(N, T, A).

%	Args0(N, Terms, Args) is equivalent to mapList(genarg0(N), Terms, Args).
:- args0(_, T, A) when T or A.
args0(_, [], []).
args0(N, T0.T, A0.A) :-
	genarg0(N, T0, A0),
	args0(N, T, A).

%	PathArg/3 is an extension of genarg/3 to a list of integer indices.
%	PathArg(Path, Term, SubTerm) finds the SubTerm of Term reached by
%	repeatedly taking the arg of Term corresponding to the next element
%	of Path.
%
%	PathArg/3 will generate Path given Term, but care is needed if Term
%	is not ground because, unlike the Quintus implementation, pathArg/3
%	will suspend on variable sub-terms.
:- pathArg(_, T, _) when T.
pathArg([], T, T).
pathArg(P0.P, T0, S) :-
	genarg(P0, T0, T),
	pathArg(P, T, S).
