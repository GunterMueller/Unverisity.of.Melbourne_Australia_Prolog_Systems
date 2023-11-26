/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible same_functor library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.
%
% These routines are not needed in NU-Prolog because functor/3 suspends
% until its arguments are sufficiently instantiated.

same_functor(T1, T2, Pred, Arity) :-
	functor(T1, Pred, Arity),
	functor(T2, Pred, Arity).

same_functor(T1, T2, Arity) :-
	functor(T1, Pred, Arity),
	functor(T2, Pred, Arity).

same_functor(T1, T2) :-
	functor(T1, Pred, Arity),
	functor(T2, Pred, Arity).
