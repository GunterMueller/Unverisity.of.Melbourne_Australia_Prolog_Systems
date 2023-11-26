/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible changearg library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

:- initializing, ensureLoaded(library(changeArg)).

%	Change_arg(N, Term1, Arg1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg(N, Term1, Arg1), but arg(N, Term2, Arg2).
change_arg(N, Term1, Arg1, Term2, Arg2) :-
	changeArg(N, Term1, Arg1, Term2, Arg2).

%	Change_arg(N, Term1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg(N, Term2, Arg2).
change_arg(N, Term1, Term2, Arg2) :-
	change_arg(N, Term1, _, Term2, Arg2).

%	Change_arg0(N, Term1, Arg1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg0(N, Term1, Arg1), but arg(N, Term2, Arg2).
%
%	This is retained only for compatibility.  Use change_functor/5 instead.
change_arg0(0, Term1, Arg1, Term2, Arg2) :-
	change_functor(Term1, Arg1, Term2, Arg2).
change_arg0(N, Term1, Arg1, Term2, Arg2) :-
	N > 0,
	change_arg(N, Term1, Arg1, Term2, Arg2).

%	Change_arg0(N, Term1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that arg0(N, Term2, Arg2).
%
%	This is retained only for compatibility.  Use change_functor/4 instead.
change_arg0(N, Term1, Term2, Arg2) :-
	change_arg0(N, Term1, _, Term2, Arg2).

%	Change_functor(Term1, Symbol1, Term2, Symbol2, Arity) is true when
%	Term1 and Term2 are identical except that functor(Term1, Symbol1, Arity),
%	but functor(Term2, Symbol2, Arity).
change_functor(Term1, Symbol1, Term2, Symbol2, Arity) :-
	changeFunctor(Term1, Symbol1, Term2, Symbol2, Arity).

%	Swap_args(N1, N2, Term1, Arg1, Term2, Arg2) is true when Term1 and Term2
%	are identical except that the Arg1 at N1 and Arg2 at N2 in Term1 are
%	exchanged in Term2.
swap_args(N1, N2, Term1, Arg1, Term2, Arg2) :-
	swapArgs(N1, N2, Term1, Arg1, Term2, Arg2).

%	Swap_args(N1, N2, Term1, Term2) is true when Term1 and Term2
%	are identical except that the args at N1 and N2 in one are exchanged
%	in the other.
swap_args(N1, N2, Term1, Term2) :-
	swap_args(N1, N2, Term1, _, Term2, _).

%	Change_path_arg(Path, Term1, SubTerm1, Term2, SubTerm2) is true when
%	Term1 and Term2 are identical except that path_arg(Path, Term1, SubTerm1),
%	but path_arg(Path, Term2, SubTerm2).
change_path_arg(P0.P, Term1, SubTerm1, Term2, SubTerm2) :-
	changePathArg(P, Term10, SubTerm1, Term20, SubTerm2).

%	Change_path_arg(Path, Term1, Term2, SubTerm2) is true when
%	Term1 and Term2 are identical except that path_arg(Path, Term2, SubTerm2).
change_path_arg(Path, Term1, Term2, SubTerm2) :-
	change_path_arg(Path, Term1, _, Term2, SubTerm2).
