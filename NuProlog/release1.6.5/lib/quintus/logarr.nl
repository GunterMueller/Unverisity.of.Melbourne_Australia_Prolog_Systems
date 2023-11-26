/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1990 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */


% Nepolog Quintus compatible logarithmic access time array library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	These routines use a similar interface to the original one described
%	below, but are implemented differently.  The restriction on terms
%	with principal function symbol $ or that are variables in that
%	implementation does not exist here, but we use about 20% more memory.
%
%	Arefa/3 is not implemented, because I really can't see how it can be
%	used for anything useful.  Its code is commented out.

%=========================================================================
%
% LOGARITHMIC ARRAYS (Extendable arrays with logarithmic access time)
% by David Warren (somewhat modified by Fernando Pereira)
%
% Array extends from 0 to 2**Size - 1, where Size is a multiple of 2.
% Note that 2**Size = 1<<Size.
%
% External interface
%
% new_array(A) returns an empty new array A.
%
% is_array(A) checks whether A is an array.
%
% aref(Index,Array,Element) unified Element to Array[Index], or fails
%   if Array[Index] has not been set.
%
% arefa(Index,Array,Element) is as aref/3, except that it unifies
%   Element with a new array if Array[Index] is undefined. This
%   is useful for multidimensional arrays implemented as arrays of
%   arrays.
%
% arefl(Index,Array,Element) is as aref/3, except that Element appears
%   as '[]' for undefined cells.
%
% aset(Index,Array,Element,NewArray) unifies NewArray with the result
%   of setting Array[Index] to Element.
%
% alist(Array,List) returns a list of pairs Index-Element
%   of all the elements of Array that have been set.

?- initializing, ensureLoaded(library(logarr)).

new_array(Array) :-
	newArray(Array).

new_array(N, Array) :-
	newArray(N, Array).

is_array(Array) :-
	isArray(Array).

is_array(N, Array) :-
	isArray(N, Array).
