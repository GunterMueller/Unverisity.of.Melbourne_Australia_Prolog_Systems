/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible arg library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

:- initializing, ensureLoaded(library(arg)).

%	Arg0/3 is like arg/3 except that arg(0, f(_, . . ., _), f).
%	The Quintus definition reports instantiation errors rather than
%	failing silently.
%
%	Defined in library(arg).

%	Genarg/3 is like arg/3 except that it backtracks through the arguments
%	if the index isn't given.
%
%	Defined in library(arg).

%	Genarg0/3 is like arg0/3 except that it backtracks through the arguments
%	if the index isn't given.
%
%	Defined in library(arg).

%	Args(N, Terms, Args) is equivalent to mapList(genarg(N), Terms, Args).
%
%	Defined in library(arg).

%	Args0(N, Terms, Args) is equivalent to mapList(genarg0(N), Terms, Args).
%
%	Defined in library(arg).

%	Project(Terms, N, Args) is equivalent to args0(N, Terms, Args).
%	It is retained for backwards compatibility only.
project(Terms, N, Args) :-
	args0(N, Terms, Args).

%	Path_arg/3 is an extension of genarg/3 to a list of integer indices.
%	Path_arg(Path, Term, SubTerm) finds the SubTerm of Term reached by
%	repeatedly taking the arg of Term corresponding to the next element
%	of Path.
%
%	Path_arg/3 will generate Path given Term, but care is needed if Term
%	is not ground because, unlike the Quintus implementation, path_arg/3
%	will suspend on variable sub-terms.
path_arg(P, T, S) :-
	pathArg(P, T, S).
