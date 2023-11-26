/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart
 */

%	sort(List, Res) : sort List into Res removing duplicates.
%		List should be bound.
%	sorted(List) : List is sorted.
%	merge(List1, List2, NewList) : merge the lists to give NewList
%		removing duplicates. The lists should be bound.
%	keySort/3: sort a list of Key-Data by Key in ascending
%		or descending order.
%		Ordering is determined by the first argument:
% 			(+) is ascending, (-) is descending
%	keySort/2 sorts a list of Key-Data by Key, in ascending order.
%
%	multiKeySort/4 sorts a list on multiple terms in ascending or
%	descending order.  The first argument is a list of terms to use as keys.
%	Ordering is determined by top level (unary) functor: 
% 		+ is ascending, - is descending, default is ascending
%	The second argument is the pattern of the list elements.
%	eg: multiKeySort([A, -B], A-B, [2-3, 4-5, 2-6], [2-6, 2-3, 4-5]).
%
%	duplicate/2 creates the duplicate of a term with 
%	new variables, but equivalent bindings within the term.

?- $isList(X, _) when X.
$isList([], []).
$isList(_.X, Y) :-
	$isList(X, Y).

sort(List, Res) :-
	$isList(List, S),
	$sort(1, S, List, Sorted),
	$rmDups(Sorted, Res).

?- $rmDups(X, _) when X.
$rmDups([], []).
$rmDups(X.XT, X.YT) :-
	$rmDups(X, XT, YT).

?- $rmDups(_, X, _) when X.
$rmDups(_, [], []).
$rmDups(X1, X2.XT, Y) :-
	(X1 == X2 ->
		$rmDups(X1, XT, Y)
	;	Y = X2.YT,
		$rmDups(X2, XT, YT)
	).	

%	Check to see if given list is sorted.  May contain duplicates.
?- sorted([]) when ever.
?- sorted(_.X) when X.
sorted([]).
sorted([_]).
sorted(H1.H2.List) :-
	H1 @=< H2,
	sorted(H2.List).

%	Merge two sorted lists. Behaviour is unpredictable if not sorted.
?- merge(L1, L2, _) when L1 and L2.
merge([], List2, List2).
merge(Head1.Tail1, [], Head1.Tail1).
merge(Head1.Tail1, Head2.Tail2, Merged) :-
	compare(C, Head1, Head2),
	$merge(C, Head1, Tail1, Head2, Tail2, Merged).

$merge((<), Head1, Tail1, Head2, Tail2, Head1.Merged) :-
	merge(Tail1, Head2.Tail2, Merged).
$merge((=), Head1, Tail1, _, Tail2, Head1.Merged) :-
	merge(Tail1, Tail2, Merged).
$merge((>), Head1, Tail1, Head2, Tail2, Head2.Merged) :-
	merge(Head1.Tail1, Tail2, Merged).

keySort(List, Res) :-
	keySort(+, List, Res).

keySort(+, List, Res) :-
	$isList(List, S),
	$sort(2, S, List, Res).
keySort(-, List, Res) :-
	$isList(List, S),
	$sort(-2, S, List, Res).

?- $sort(_, S, _, _) when S.
$sort(K, _, List, Res) :-
	$sort(K, List, Res).

%	Make the raw sort available without duplicate removal
%	and waiting for the list to be proper.
$sort(List, Res) :-
	$sort(1, List, Res).

?- multiKeySort(L, _, _, _) when L.
multiKeySort([], _P, S, S).
multiKeySort(H.L, P, S, S1) :-
	multiKeySort(L, P, S, S2),
	$order(H, C, K),
	$keyList(K, P, S2, S3),
	keySort(C, S3, S4),
	$stripKeys(S4, S1).

$order(K, (+), K) :-
	var(K),
	!.
$order(+K, (+), K) :-
	!.
$order(-K, (-), K) :-
	!.
$order(K, (+), K).

?- $keyList(_, _, L, _) when L.
$keyList(_, _, [], []).
$keyList(K, P, H.T, (K1 - H).T1) :-
	duplicate(K - P, (K1 - H)),
	$keyList(K, P, T, T1).

?- $stripKeys(L, _) when L.
$stripKeys([], []).
$stripKeys((_ - H).T, H.T1) :-
	$stripKeys(T, T1).

duplicate(Term, Term1) :-
	once (
		putprop($duplicate, $term, Term),
		getprop($duplicate, $term, Term2)
	),
	Term1 = Term2.
