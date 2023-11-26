/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1990 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

%	These routines use a similar interface to the original one described
%	below, but are implemented differently.  The restriction on terms
%	with principal function symbol $ or that are variables in that
%	implementation does not exist here, but we use about 20% more memory.
%
%	Arefa/3 is not implemented, because I really can't see how it can be
%	used for anything useful.  Its code is commented out.
%
%	In addition to the predicates described in the original comment below,
%	the predicate asize/2 gives an upper bound for the index to an array.
%
%	The arrays are implemented as trees of chunks.  The best chunk size is
%	usually 4, which is the default chosen by newArray/1, but 2, 8, and 16
%	are also implemented.  If most accesses are lookups rather than
%	modifications, 8 or 16 may be a better choice.
%
%	Significant optimization is possible if only the most recently modified
%	version of an array is accessed.  This kind of array is made by
%	newLastUseArray/[1,2].  Accesses to elements other than the last are
%	detected and warn then fail.
%
%	The original Prolog code for this library has been largely replaced
%	with C routines $aref/4 and $aset/5.  Note that the C routines do not
%	delay if the array is not sufficiently instantiated.  This unlikely to
%	be a problem if arrays are only manipulated using the routines in this
%	library.

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
% newArray(A) returns an empty new array A.
%
% isArray(A) checks whether A is an array.
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

newArray(array([], [], [], [], 0)).

newArray(2, array([], [], 0)).
newArray(4, array([], [], [], [], 0)).
newArray(8, array([], [], [], [], [], [], [], [], 0)).
newArray(16, array([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[], 0)).

newLastUseArray(array([], [], [], [], 16'10000)).
newLastUseArray(2, array([], [], 16'10000)).
newLastUseArray(4, array([], [], [], [], 16'10000)).
newLastUseArray(8, array([], [], [], [], [], [], [], [], 16'10000)).
newLastUseArray(16,
		array([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[], 16'10000)).

isArray(array(_, _, _)).
isArray(array(_, _, _, _, _)).
isArray(array(_, _, _, _, _, _, _, _, _)).
isArray(array(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_, _)).

isArray(2, array(_, _, _)).
isArray(4, array(_, _, _, _, _)).
isArray(8, array(_, _, _, _, _, _, _, _, _)).
isArray(16, array(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_, _)).

:- asize(Array, _) when Array.
asize(Array, Size) :-
	isArray(N, Array),
	N1 is N + 1,
	arg(N1, Array, L),
	integer(L),
	( (L >> 17) /\ 1 =\= 0 ->
		sys$error(warning,
			"Attempt to use old version of current-version-only array."),
		fail
	),
	Size is 4 << (L >> 18) - 1.

:- aref(_, Array, _) when Array.
aref(Index, Array, Element) :-
	( var(Index) ->
		asize(Array, Size),
		between(0, Size, Index),
		$aref(Index, Array, Element, 0)
	;	$aref(Index, Array, Element, 0)
	).

:- arefl(Index, Array, _) when Index and Array.
arefl(Index, Array, Element) :-
	$arefl(Index, Array, Element, 1).

/* REPLACED WITH $aref/4
:- aref(_, Array, _) when Array.
aref(Index, Array, Element) :-
	Array = array(_, _, _, _, L),
	integer(L),
	(L >> 17) /\ 1 =:= 0,
	S is L >> 18,
	Size is 4 << S - 1,
	between(0, Size, Index),
	logarr$aref(Index, S, Array, Element).

:- logarr$aref(_, _, Array, _) when Array.
logarr$aref(Index, S, Array0, Element) :-
	I is (Index >> S) /\ 3,
	I1 is I + 1,
	( S > 0 ->
		arg(I1, Array0, Array),
		S2 is S - 2,
		logarr$aref(Index, S2, Array, Element)
	;	Array0 = array(_, _, _, _, L),
		integer(L),
		(L >> I) /\ 1 =\= 0,
		arg(I1, Array0, Element)
	).

:- arefl(Index, _, _) when Index.
arefl(Index, Array, Element) :-
	integer(Index),
	logarr$arefl(Index, Array, Element).

:- logarr$arefl(_, Array, _) when Array.
logarr$arefl(_, [], []).
logarr$arefl(Index, Array0, Element) :-
	Array0 = array(_, _, _, _, L),
	integer(L),
	(L >> 17) /\ 1 =:= 0,
	S is L >> 18,
	I is ((Index >> S) /\ 3) + 1,
	( S > 0 ->
		arg(I, Array0, Array),
		logarr$arefl(Index, Array, Element)
	;	arg(I, Array0, Element)
	).
REPLACED WITH $aref/4 */

/* I can't see the point of this.
:- arefa(Index, Array, _) when Index and Array.
arefa(Index, Array, Element) :-
	aref(Index, Array, Element),
	!.
arefa(Index, Array, Element) :-
	newArray(Element).
*/

:- aset(Index, Array, _, _) when Index and Array.
aset(Index, Array, Element, NewArray) :-
	$aset(Index, Array, Element, NewArray, 0).

/* REPLACED WITH $aset/5
:- aset(Index, Array, _, _) when Index and Array.
aset(Index, Array, Element, NewArray) :-
	Array = array(_, _, _, _, L),
	integer(Index),
	(L >> 17) /\ 1 =:= 0,
	S is L >> 18,
	Size is 4 << S,
	( Index >= Size ->							% Lazy
		L2 is ((S + 2) << 18) + 1,
		aset(Index, array(Array, [], [], [], L2), Element, NewArray)
	;	Index >= 0,
		logarr$aset(Index, Array, Element, NewArray)
	).

:- logarr$aset(Index, Array, _, _) when Array.
logarr$aset(Index, Array0, Element, NewArray0) :-
	Array0 = array(_, _, _, _, L),
	S is L >> 18,
	I is (Index >> S) /\ 3,
	NewL is L \/ (1 << I),
	( (L >> I) /\ 1 =:= 0 ->
		logarr$changeArrayElement(I, Array0, _, NewArray0, NewL, NewArray),
		S2 is S - 2,
		logarr$abuild(S2, Index, Element, NewArray)
	; S > 0 ->
		logarr$changeArrayElement(I, Array0, Array, NewArray0, NewL, NewArray),
		logarr$aset(Index, Array, Element, NewArray)
	;	logarr$changeArrayElement(I, Array0, _, NewArray0, NewL, Element)
	).

logarr$abuild(S, Index, Element, NewArray) :-
	I is (Index >> S) /\ 3,
	( S >= 0 ->
		L is (S << 18) + (1 << I),
		S2 is S - 2,
		logarr$abuild(I, Index, Element, NewArray, S2, L)
	;	Element = NewArray
	).

logarr$abuild(0, Index, Element, array(A0, [], [], [], L), S, L) :-
	logarr$abuild(S, Index, Element, A0).
logarr$abuild(1, Index, Element, array([], A1, [], [], L), S, L) :-
	logarr$abuild(S, Index, Element, A1).
logarr$abuild(2, Index, Element, array([], [], A2, [], L), S, L) :-
	logarr$abuild(S, Index, Element, A2).
logarr$abuild(3, Index, Element, array([], [], [], A3, L), S, L) :-
	logarr$abuild(S, Index, Element, A3).

logarr$changeArrayElement(
		0, array(A0, A1, A2, A3, _), A0,
		array(New, A1, A2, A3, NewL), NewL, New).
logarr$changeArrayElement(
		1, array(A0, A1, A2, A3, _), A1,
		array(A0, New, A2, A3, NewL), NewL, New).
logarr$changeArrayElement(
		2, array(A0, A1, A2, A3, _), A2,
		array(A0, A1, New, A3, NewL), NewL, New).
logarr$changeArrayElement(
		3, array(A0, A1, A2, A3, _), A3,
		array(A0, A1, A2, New, NewL), NewL, New).
REPLACED WITH $aset/5 */

alist(Array, List) :-
	Array = array(_, _, _, _, L),
	(L >> 17) /\ 1 =:= 0,
	logarr$alist(0, Array, List, []).

:- logarr$alist(_, Array, _, _) when Array.
logarr$alist(Base0, array(A0, A1, A2, A3, L), List0, List) :-
	Step is 1 << (L >> 18),
	L0 is L /\ 1,
	logarr$alist(L0, Step, Base0, A0, List0, List1),
	L1 is (L >> 1) /\ 1,
	Base1 is Base0 + Step,
	logarr$alist(L1, Step, Base1, A1, List1, List2),
	L2 is (L >> 2) /\ 1,
	Base2 is Base1 + Step,
	logarr$alist(L2, Step, Base2, A2, List2, List3),
	L3 is (L >> 3) /\ 1,
	Base3 is Base2 + Step,
	logarr$alist(L3, Step, Base3, A3, List3, List).

:- logarr$alist(Set, _, _, _, _, _) when Set.
logarr$alist(0, _, _, _, List, List).
logarr$alist(1, Step, Base, X, List0, List) :-
	( Step == 1 ->
		List0 = (Base - X).List
	;	logarr$alist(Base, X, List0, List)
	).
