/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Lee Naish
 */

%	Logical all solutions pred solutions/3 is transformed (negtrf.pl)
%	into $solutions/4, the first arg being the global vars.
%	This file contains defns of setof and bagof also.  Findall
%	and other aggregates should be around somewhere.
%
%	This could be cleaned up a bit more.
%	Sorting is done efficiently but probably should
%	be done before separate answer lists are 'picked' from the list of
%	all solutions.
%
%	Error messages are printed if there are local variables in answers
%	or if there are elayed calls (unfortunately, any call delayed
%	on a global var causes this message).
%
%						Lee Naish


%	nesting level of calls to setof/bagof/solutions
?- putprop($solns_setBagof, nesting, 0).

%	solutions/3 is transformed into $solutions/4 by the preprocessor
%	the first arg is the global vars
$solutions(G, X, P, Set) :-
	listOfVars(G, GV),
	( GV == [] ->
		$solbag(X, P, Bag)
	;	( GV = [V] ->
			GT = V
		;	GT =.. f.GV
		),
		$solbag(X, P, GT, Bag)
	),
	$dsort(Bag, Set).

%	get "bag" of solutions (no global vars)
%	Test for unbound local vars and delayed calls.
$solbag(X, P, Bag) :-
	once getprop($solns_setBagof, nesting, N),
	N1 is N + 1,
	putprop($solns_setBagof, nesting, N1),
	(	call(P),			% find solution
		waitedOn(P, V),
		( cons(V) ->
			format(user_error,
				"~NWarning: solutions/3 goal may have floundered.~n", [])
		),
		addprop($solns_setBagof, N1, X),
		\+ ground(X),
		format(user_error,
			"~NError: unbound local variables in solutions/3.~n", []),
				% local vars in solutions are non-logical
				% (see paper in SLP85)
		fail
	;	true
	),
	properties($solns_setBagof, N1, Bags),
	remprop($solns_setBagof, N1),
	putprop($solns_setBagof, nesting, N),
	Bag = Bags.

%	get "bag" of solutions with common global variable (Key) values
%	(not sorted etc)  Test for unbound local vars and delayed calls.
$solbag(X, P, Key, Bag) :-
	once getprop($solns_setBagof, nesting, N),
	N1 is N + 1,
	putprop($solns_setBagof, nesting, N1),
	(	call(P),			% find solution
		waitedOn(P, V),
		( cons(V) ->
			format(user_error,
				"~NWarning: solutions/3 goal may have floundered.~n", [])
		),
		addprop($solns_setBagof, N1, Key - X),
		/* BUG!
		 * This doesn't interact well with floundering calls to solutions.
		 * All it does is wake them up, and if they don't like [] as
		 * variable bindings, they fail.
		 *
		 * Need to replace it with a test that doesn't bind variables.
		 *
		 * Note that $solbag/3 doesn't have this problem.
		 */
		$make_ground(Key),
		\+ ground(X),
		format(user_error,
			"~NError: unbound local variables in solutions/3.~n", []),
				% local vars in solutions are non-logical
				% (see paper in SLP85)
		fail
	;	true
	),
	properties($solns_setBagof, N1, Bags),
	remprop($solns_setBagof, N1),
	putprop($solns_setBagof, nesting, N),
	$pick(Bags, Key, Bag).

%	get "set" of solutions with common global variable (Key) values
%	(sorted and duplicates removed)
setof(X, P, Set) :-
	bagof(X, P, Bag),
	sort(Bag, SBag),
	Set = SBag.

bagof(X, P, Bag) :-
	$globalVars(P, X, [], L),
	cons(L),
	!,
	Key =.. f.L,
	$bagof(X, P, Key, Bag).
		% get "bag" of solutions with no common global variables values
		% (not sorted etc).  Fails if bag is empty.
bagof(X, P, Bag) :-
	once getprop($solns_setBagof, nesting, N),
	N1 is N + 1,
	putprop($solns_setBagof, nesting, N1),
	(	call(P),			% find solution
		addprop($solns_setBagof, N1, X),
		fail
	;	true
	),
	properties($solns_setBagof, N1, Bags),
	remprop($solns_setBagof, N1),
	putprop($solns_setBagof, nesting, N),
	cons(Bags),
	Bag = Bags.

%	get "bag" of solutions with common global variable (Key) values
%	(not sorted etc).  Fails if bag is empty.
$bagof(X, P, Key, Bag) :-
	once getprop($solns_setBagof, nesting, N),
	N1 is N + 1,
	putprop($solns_setBagof, nesting, N1),
	(	call(P),			% find solution
		addprop($solns_setBagof, N1, Key-X),
		fail
	;	true
	),
	properties($solns_setBagof, N1, Bags),
	remprop($solns_setBagof, N1),
	putprop($solns_setBagof, nesting, N),
	$bagPick(Bags, Key, Bag1),
	Bag1 \== [],
	Bag = Bag1.

%	this version is still "logical", though slightly more
%	restricted (either all keys are ground or the global variables
%	are ground).  It is more efficient.
$pick([], _, []).
$pick(S, G, B) :-
	cons(S),
	( $keysGr(S) ->
		keySort(S, (K-T).SS),
		$kGrPick(G, K, SS, [T], B)
	;	$whenGrPick(S, G, B)
	).

%	check all keys are ground
$keysGr([]).
$keysGr((K-_).L) :-
	ground(K),
	$keysGr(L).

?- $kGrPick(_, _, L, _, _) when L.
$kGrPick(G, LG, [], B, B1) :-
	(	B1 = B,
		G = LG
	;	B1 = [],
		G ~= LG
	).
$kGrPick(G, LG, (LG-T).S, B, B1) :-
	!,
	$kGrPick(G, LG, S, T.B, B1).
$kGrPick(G, LG, (LG1-T).S, B, B1) :-
	(	B1 = B,
		G = LG
	;	$kGrPick(G, LG1, S, [T], B1),
		G ~= LG
	).

%	delay until globals are ground then pick
?- $whenGrPick(_, G, _) when ground(G).
$whenGrPick(S, G, B) :-
	$gGrPick(S, G, B).

%	pick assuming all global vars are ground
?- $gGrPick(L, _, _) when L.
$gGrPick([], _, []).
$gGrPick((G-T).S, G, T.B) :-
	!,
	$gGrPick(S, G, B).
$gGrPick(_.S, G, B) :-
	$gGrPick(S, G, B).

%	pick out list of answers with global var (Key) values
%   equal to current Key value
$bagPick(S, G, B) :-
	keySort(S, SS),
	$bagPick1(_, SS, [], B, G).

?- $bagPick1(_, L, _, _, _) when L.
$bagPick1(G, [], B, B, G) :-
	!.
$bagPick1(G, (G-T).S, B, B1, G1) :-
	!,
	$bagPick1(G, S, T.B, B1, G1).
$bagPick1(G, _, B, B, G).
$bagPick1(_, S, _, B, G1) :-
	$bagPick1(_, S, [], B, G1).

%	make a term ground (all vars are bound to [])
$make_ground(X) :-
	var(X),
	X = [].
$make_ground(X) :-
	const(X).
$make_ground(X) :-
	cons(X),
	X = A.B,
	$make_ground(A),
	$make_ground(B).
$make_ground(X) :-
	$struct(X),
	X =.. _.Args,
	$make_ground(Args).

%	This is just like built in sort except it's logical (delays etc)
% $dsort(List, Res) : sort and delete duplicates from List into Res.
% 	Calls will delay until List is insufficiently instantiated.
% $dsortMerge(List1, List2, NewList) : sort the lists, merge to give NewList.
% 	Calls will delay until lists are insufficiently instantiated.
% $dsorted(List) : List is sorted.
% 	Calls will delay until List is insufficiently instantiated.
% $dsortedMerge(List1, List2, NewList) : merge the lists to give NewList.
% 	Calls will delay until lists are insufficiently instantiated.
%

?- $dsort(A, _) when A.
$dsort(List, Res) :-
	$dsortMergeSort(List, Res, []).

?- $dsortMergeSort(A, _, _) when A.
$dsortMergeSort([], Res, Res).
$dsortMergeSort(X.List, ROut, RIn) :-
	$dsortMergeSplit(List, X, L1, L2),
	$dsortMergeSort(L1, ROut, X.RTmp),
	$dsortMergeSort(L2, RTmp, RIn).

/* Correct, but A and B has merit. */
% ?- $dsortMergeSplit(A, B, C, D) when A or C or D. 
?- $dsortMergeSplit(A, B, C, D) when A and B.
$dsortMergeSplit([], _, [], []).
$dsortMergeSplit(A.List, X, L1, L2) :-
	termCompare(R, A, X),
	$dsortMergeSplit(R, A, List, X, L1, L2).

$dsortMergeSplit((<), A, List, X, A.L1, L2) :-
	$dsortMergeSplit(List, X, L1, L2).
$dsortMergeSplit((>), A, List, X, L1, A.L2) :-
	$dsortMergeSplit(List, X, L1, L2).
$dsortMergeSplit((=), A, List, X, L1, L2) :-
	$dsortMergeSplit(List, X, L1, L2).

%	Sort both arguments, then merge. Expensive if both arguments
%	already sorted.
?- $dsortMerge(L1, L2, _) when L1 and L2.
$dsortMerge(L1, L2, NL) :-
	$dsort(L1, NL1),
	$dsort(L2, NL2),
	$dsortedMerge(NL1, NL2, NL).

%	Merge two sorted lists. Behaviour is unpredictable if not sorted.
?- $dsortedMerge(L1, L2, _) when L1 and L2.
$dsortedMerge(Head1.Tail1, Head2.Tail2, Merged) :-
	termCompare(R, Head1, Head2),
	(	R = (<),
		Merged = Head1.Merged1,
		$dsortedMerge(Tail1, Head2.Tail2, Merged1)
	;	R = (>),
		Merged = Head2.Merged1,
		$dsortedMerge(Head1.Tail1, Tail2, Merged1)
	;	R = (=),
		Merged = Head2.Merged1,
		$dsortedMerge(Tail1, Tail2, Merged1)
	).
$dsortedMerge([], List2, List2).
$dsortedMerge(Head1.Tail1, [], Head1.Tail1).

%	gets global vars for setof/bagof (maybe should change
%	conventions so they are more compatible with solutions etc)
%$globalVars(T, X, L0, L) :-
%	( var(T) ->
%		!,					% Must commit before trying L0 = L!
%		( occurs(T, X) ->
%			L = L0
%		;	$vInsert(T, L0, L)
%		)
%	; const(T) ->
%		!,					% Must commit before trying L0 = L!
%		L0 = L
%	;	fail
%	).
$globalVars(T, X, L0, L) :-
	var(T),
	!,					% Must commit before trying L0 = L!
	( occurs(T, X) ->
		L = L0
	;	$vInsert(T, L0, L)
	).
$globalVars(T, _, L0, L) :-
	const(T),
	!,					% Must commit before trying L0 = L!
	L0 = L.
$globalVars(X ^ P, Y, L0, L) :-
	!,
	$globalVars(P, (X, Y), L0, L).
$globalVars(setof(X, P, S), Y, L0, L) :-
	!,
	$globalVars((P, S), (X, Y), L0, L).
$globalVars(bagof(X, P, S), Y, L0, L) :-
	!,
	$globalVars((P, S), (X, Y), L0, L).
$globalVars(T, X, L0, L) :-
	functor(T, _, N),
	$globalVars(N, T, X, L0, L).

$globalVars(N, T, X, L0, L) :-
	( N > 0 ->
		N1 is N - 1,
		arg(N, T, T1),
		$globalVars(T1, X, L0, L1),
		$globalVars(N1, T, X, L1, L)
	;	L0 = L
	).

%	insert variable into a list if its not there already
$vInsert(X, L0, L) :-
	( occurs(X, L0) ->
		L0 = L
	;	L = X.L0
	).

_ ^ P :-
	call(P).

%	logical term comparison
termCompare(C, A, B) :-
	( if A=B then
		C = (=)
	else
		compare(C, A, B)
	).

% The element of property list '$findall' with key '$level' is a level
% number used as a key for storing results of findall/3, countall/2 and
% some of the aggregates. The level number permits nesting of these predicates.

?- putprop($findall, $level, 0).

% findall(Term, Goal, List)
% 	executes Goal, backtracking to produce all possible bindings for Term.
% These (possibly partially) instantiated Terms are returned in a List.
%
% This is an undeclarative (but fast) version of solutions/3, which should be
% used in preference. findall/3's can be nested.

findall(Term, Goal, List) :-
	once (
		getprop($findall, $level, N),
		N1 is N + 1,
		putprop($findall, $level, N1),
		(	call(Goal),
			once addprop($findall, N1, Term),
			fail
		;	true
		),
		properties($findall, N1, List1),	% List changed to List1
		remprop($findall, N1),
		putprop($findall, $level, N)
	),
	List = List1.					% Extra conjunct

% countall(Goal, Count)
% 	executes Goal, backtracking to Count all possible solutions for Goal.
%
% This is an undeclarative (but fast) version of count/3, which should be
% used in preference. countall/2's can be nested.

countall(Goal, Count) :-
	once (
		getprop($findall, $level, N),
		N1 is N + 1,
		putprop($findall, $level, N1),
		putprop($findall, N1, 0),
		(	call(Goal),
			once (
				getprop($findall, N1, Old),
				New is Old + 1,
				putprop($findall, N1, New)
			),
			fail
		;	true
		),
		getprop($findall, N1, Count),
		remprop($findall, N1),
		putprop($findall, $level, N)
	).
