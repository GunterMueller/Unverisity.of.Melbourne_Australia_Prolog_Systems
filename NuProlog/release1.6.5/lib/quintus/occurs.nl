/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible occurs library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

:- initializing, ensureLoaded(library(occurs)).

%	These predicates are non-logical.  Logical versions will eventually
%	appear in library(occurs).

%	Contains_term/2 tests if one term unifies with some sub-term of another.
%	Non-logical.
contains_term(SubTerm, Term) :-
	containsTerm(SubTerm, Term).

%	Contains_var/2 tests if one term is identical to some sub-term of another.
%	Non-logical.
contains_var(SubTerm, Term) :-
	containsVar(SubTerm, Term).

%	Free_of_term/2 checks that one term unifies with no sub-term of another.
%	Non-logical.
free_of_term(SubTerm, Term) :-
	freeOfTerm(SubTerm, Term).

%	Free_of_var/2 checks that one term is identical to no sub-term of another.
%	Non-logical.
free_of_var(SubTerm, Term) :-
	freeOfVar(SubTerm, Term).

%	Occurrences_of_term(SubTerm, Term, Count) counts the number of sub-terms 
%	of Term that unify with SubTerm.
%	Non-logical.
occurrences_of_term(SubTerm, Term, Count) :-
	occurrencesOfTerm(SubTerm, Term, Count).

%	Occurrences_of_var(SubTerm, Term, Count) counts the number of sub-terms 
%	of Term that are identical to SubTerm.
%	Non-logical.
occurrences_of_var(SubTerm, Term, Count) :-
	occurrencesOfVar(SubTerm, Term, Count).

%	Sub_term/2 enumerates the sub-terms of a term, much as member/2
%	enumerates the elements of a list.  The order in which sub-terms are
%	found is not defined.
%	Non-logical.
sub_term(SubTerm, Term) :-
	subTerm(SubTerm, Term).
