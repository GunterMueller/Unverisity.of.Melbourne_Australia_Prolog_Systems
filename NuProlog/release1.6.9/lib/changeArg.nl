/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus-like change-arg library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

:- initializing, ensureLoaded(library(arg)).			% For genarg/3.

%	ChangeArg(N, Term1, Arg1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg(N, Term1, Arg1), but arg(N, Term2, Arg2).
:- changeArg(_, Term1, _, Term2, _) when Term1 or Term2.
changeArg(N, Term1, Arg1, Term2, Arg2) :-
	functor(Term1, Pred, Arity),
	functor(Term2, Pred, Arity),
	genarg(N, Term1, Arg1),
	genarg(N, Term2, Arg2),
	N0 is N - 1,
	copyArgs(1, N0, Term1, Term2),
	N1 is N + 1,
	copyArgs(N1, Arity, Term1, Term2).

%	CopyArgs(M, N, Term1, Term2) unifies args I : M =< I =< N of Term1 and
%	Term2.
:- copyArgs(M, N, _, _) when ground(M) and ground(N).
copyArgs(M, N, Term1, Term2) :-
	( M =< N ->
		arg(M, Term1, Arg),
		arg(M, Term2, Arg),
		M1 is M + 1,
		copyArgs(M1, N, Term1, Term2)
	).

%	ChangeArg(N, Term1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg(N, Term2, Arg2).
changeArg(N, Term1, Term2, Arg2) :-
	changeArg(N, Term1, _, Term2, Arg2).

%	ChangeFunctor(Term1, Symbol1, Term2, Symbol2, Arity) is true when
%	Term1 and Term2 are identical except that functor(Term1, Symbol1, Arity),
%	but functor(Term2, Symbol2, Arity).
changeFunctor(Term1, Symbol1, Term2, Symbol2, Arity) :-
	functor(Term1, Symbol1, Arity),
	functor(Term2, Symbol2, Arity),
	copyArgs(1, Arity, Term1, Term2).

%	SwapArgs(N1, N2, Term1, Arg1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that the Arg1 at N1 and Arg2 at N2 in Term1 are
%	exchanged in Term2.
:- swapArgs(_, _, Term1, _, Term2, _) when Term1 or Term2.
swapArgs(N1, N2, Term1, Arg1, Term2, Arg2) :-
	functor(Term1, Pred, Arity),
	functor(Term2, Pred, Arity),
	genarg(N1, Term1, Arg1),
	genarg(N2, Term2, Arg1),
	arg(N2, Term1, Arg2),
	arg(N1, Term2, Arg2),
	( N1 =< N2 ->
		I0 is N1 - 1,
		I1 is N1 + 1,
		I2 is N2 - 1,
		I3 is N2 + 1
	;	I0 is N2 - 1,
		I1 is N2 + 1,
		I2 is N1 - 1,
		I3 is N1 + 1
	),
	copyArgs(1, I0, Term1, Term2),
	copyArgs(I1, I2, Term1, Term2),
	copyArgs(I3, Arity, Term1, Term2).

%	SwapArgs(N1, N2, Term1, Term2) is true when Term1 and Term2
%	are identical except that the args at N1 and N2 in one are exchanged
%	in the other.
swapArgs(N1, N2, Term1, Term2) :-
	swapArgs(N1, N2, Term1, _, Term2, _).

%	ChangePathArg(Path, Term1, SubTerm1, Term2, SubTerm2) is true when
%	Term1 and Term2 are identical except that pathArg(Path, Term1, SubTerm1),
%	but pathArg(Path, Term2, SubTerm2).
:- changePathArg(_Path, Term1, _, Term2, _) when Term1 or Term2.
changePathArg([], Term1, Term1, Term2, Term2).
changePathArg(P0.P, Term1, SubTerm1, Term2, SubTerm2) :-
	changeArg(P0, Term1, Term10, Term2, Term20),
	changePathArg(P, Term10, SubTerm1, Term20, SubTerm2).

%	ChangePathArg(Path, Term1, Term2, SubTerm2) is true when
%	Term1 and Term2 are identical except that pathArg(Path, Term2, SubTerm2).
changePathArg(Path, Term1, Term2, SubTerm2) :-
	changePathArg(Path, Term1, _, Term2, SubTerm2).
