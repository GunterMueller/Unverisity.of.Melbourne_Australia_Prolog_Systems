%	In this library, sets are represented by ordered lists with no
%	duplicates.  The ordering is defined by compare/3.
%
%	The benefit of the ordered representation is that the elementary
%	set operations can be done in time proportional to the Sum of the
%	argument sizes rather than their Product.  Some of the unordered
%	set routines, such as member/2, length/2, delete/3 can be used
%	unchanged.
%
%				PREDICATES
%
% listToSet(List, Set) : true when Set is the ordered representation of
%	the set represented by the unordered representation List. List should
%	be bound.
% addElement(Elem, Set1, Set2) : true when Set1 and Set2 are sets represented
%	as ordered lists and Set2 = Set1 U {Elem}. Elem and Set1 should be
%	bound.
% delElement(Elem, Set1, Set2) : true when Set1 and Set2 are sets represented
%	as ordered lists and Set2 = Set1 \ {Elem}. Elem and Set1 should be
%	bound.
% setMember(Element, Set) : used to test whether a bound Element occurs in a
%	bound Set.
% disjoint(Set1, Set2) : true when the two ordered sets have no element in
%	common.
% intersect(Set1, Set2) : true when the two ordered, bound sets have at least
%	one element in common.
% intersect(Set1, Set2, Intersection) : true when Intersection is the ordered
%	representation of Set1 ^ Set2, provided that both sets are ordered.
% setEq(Set1, Set2) : true when the two arguments represent the same set.
% subset(Set1, Set2) : true when every element of ordered Set1 appears in
%	ordered Set2.
% subtract(Set1, Set2, Difference) : true when Difference contains all and
%	only the elements of ordered Set1 which are not in ordered Set2.
% symdiff(Set1, Set2, Difference) : true when Difference is the symmetric
%	difference of Set1 and Set2.
% union(Set1, Set2, Union) : true when Union is the union of ordered sets Set1
%	and Set2.

%	listToSet(+List, ?Set)
%	is true when Set is the ordered representation of the set represented
%	by the unordered representation List.
listToSet(List, Set) :-
	sort(List, Set).

%	addElement(Elem, Set1, Set2)
%	is true when Set1 and Set2 are sets represented as ordered lists,
%	and Set2 = Set1 U {Elem}.  It may only be used to calculate Set2
%	given Elem and Set1.
?- addElement(Elem, Set, _) when Elem and Set.
addElement(Elem, [], [Elem]).
addElement(Elem, Head.Tail, Set) :-
	compare(Comp, Elem, Head),
	( Comp = (<),
		Set = Elem.Head.Tail
	; Comp = (=),
		Set = Head.Tail
	; Comp = (>),
		Set = Head.Set1,
		addElement(Elem, Tail, Set1)
	).

%	delElement(Elem, Set1, Set2)
%	is true when Set1 and Set2 are sets represented as ordered lists,
%	and Set2 = Set1 \ {Elem}.  It may only be used to calculate Set2
%	given Elem and Set1.  If Set1 does not contain Elem, Set2 and Set1
%	will be equal. 
?- delElement(Elem, Set, _) when Elem and Set.
delElement(_, [], []).
delElement(Elem, Head.Tail, Set) :-
	compare(Comp, Elem, Head),
	( Comp = (<),
		Set = Head.Tail
	; Comp = (=),
		Set = Tail
	; Comp = (>),
		Set = Head.Set1,
		delElement(Elem, Tail, Set1)
	).

%	setMember(+Element, +Set)
%	means the same thing, but may only be used to test whether a known
%	Element occurs in a known Set.  In return for this limited use, it
%	is more efficient when it is applicable.
?- setMember(Element, List) when Element and List.
setMember(Element, First.Rest) :-
	compare(C, Element, First),
	$setMember(C, Element, Rest).

?- $setMember(C, _, _) when C.	% For clause indexing
$setMember(=, _, _).
$setMember(>, Element, Rest) :-
	setMember(Element, Rest).

%	disjoint(+Set1, +Set2)
%	is true when the two ordered sets have no element in common.
?- disjoint(Set1, Set2) when Set1 and Set2.
disjoint([], _).
disjoint(_._, []).
disjoint(Head1.Tail1, Head2.Tail2) :-
	compare(Comp, Head1, Head2),
	( Comp = (<),
		disjoint(Tail1, Head2.Tail2)
	; Comp = (>),
		disjoint(Head1.Tail1, Tail2)
	).

%	intersect(+Set1, +Set2)
%	is true when the two ordered sets have at least one element in common.
?- intersect(Set1, Set2) when Set1 and Set2.
intersect(Head1.Tail1, Head2.Tail2) :-
	compare(Comp, Head1, Head2),
	( Comp = (<),
		intersect(Tail1, Head2.Tail2)
	; Comp = (=)
	; Comp = (>),
		intersect(Head1.Tail1, Tail2)
	).

%	intersect(+Set1, +Set2, ?Intersection)
%	is true when Intersection is the ordered representation of Set1
%	and Set2, provided that Set1 and Set2 are ordered sets.
?- intersect(Set1, Set2, _) when Set1 and Set2.
intersect(_, [], []).
intersect([], _._, []).
intersect(Head1.Tail1, Head2.Tail2, Intersection) :-
	compare(Comp, Head1, Head2),
	( Comp = (<),
		intersect(Tail1, Head2.Tail2, Intersection)
	; Comp = (=),
		Intersection = Head1.Intersection1,
		intersect(Tail1, Tail2, Intersection1)
	; Comp = (>),
		intersect(Head1.Tail1, Tail2, Intersection)
	).

%	setEq(+Set1, +Set2)
%	is true when the two arguments represent the same set.  Since they
%	are assumed to be ordered representations, they must be identical.
setEq(Set, Set).

%	subset(+Set1, +Set2)
%	is true when every element of the ordered set Set1 appears in the
%	ordered set Set2.
?- subset(Set1, Set2) when Set1 and Set2.
subset([], _).
subset(Head1.Tail1, Head2.Tail2) :-
	compare(Comp, Head1, Head2),
	( Comp = (=),
		subset(Tail1, Tail2)
	; Comp = (>),
		subset(Head1.Tail1, Tail2)
	).

%	subtract(+Set1, +Set2, ?Difference)
%	is true when Difference contains all and only the elements of Set1
%	which are not also in Set2.
?- subtract(Set1, Set2, _) when Set1 and Set2.
subtract(Set1, [], Set1).
subtract([], _._, []).
subtract(Head1.Tail1, Head2.Tail2, Difference) :-
	compare(Comp, Head1, Head2),
	( Comp = (<),
		Difference = Head1.Difference1,
		subtract(Tail1, Head2.Tail2, Difference1)
	; Comp = (=),
		subtract(Tail1, Tail2, Difference)
	; Comp = (>),
		subtract(Head1.Tail1, Tail2, Difference)
	).

%	symdiff(+Set1, +Set2, ?Difference)
%	is true when Difference is the symmetric difference of Set1 and Set2.
?- symdiff(Set1, Set2, _) when Set1 and Set2.
symdiff(Set1, [], Set1).
symdiff([], H.Set2, H.Set2).
symdiff(Head1.Tail1, Head2.Tail2, Difference) :-
	compare(Comp, Head1, Head2),
	( Comp = (<),
		Difference = Head1.Difference1,
		symdiff(Tail1, Head2.Tail2, Difference1)
	; Comp = (=),
		symdiff(Tail1, Tail2, Difference)
	; Comp = (>),
		Difference = Head2.Difference1,
		symdiff(Head1.Tail1, Tail2, Difference1)
	).

%	union(+Set1, +Set2, ?Union)
%	is true when Union is the union of Set1 and Set2.  Note that when
%	something occurs in both sets, only one copy is be retained.
union(Set1, Set2, Set) :-
	merge(Set1, Set2, Set).
