/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

sortConstantWhens(Pos, Whens, Heads, Groups, LeftOverGroups) :-
	constantWhenKeys(Pos, Whens, WhenGroups),
	constantHeadKeys(Pos, Heads, HeadGroups),
	append(WhenGroups, HeadGroups, KeyGroups),
	keySort(KeyGroups, SortedGroups),
	leftOverWhens(SortedGroups, Groups, LeftOvers),
	stripKeys(LeftOvers, LeftOverGroups).

sortConstantHeads(Pos, Heads, Groups) :-
	constantHeadKeys(Pos, Heads, KeyGroups),
	keySort(KeyGroups, SortedGroups),
	groupTuples(SortedGroups, Groups).

sortStructWhens(Pos, Whens, Heads, Groups, LeftOverGroups) :-
	structWhenKeys(Pos, Whens, WhenGroups),
	structHeadKeys(Pos, Heads, HeadGroups),
	append(WhenGroups, HeadGroups, KeyGroups),
	keySort(KeyGroups, SortedGroups),
	leftOverWhens(SortedGroups, Groups, LeftOvers),
	stripKeys(LeftOvers, LeftOverGroups).

sortStructHeads(Pos, Heads, Groups) :-
	structHeadKeys(Pos, Heads, KeyGroups),
	keySort(KeyGroups, SortedGroups),
	groupTuples(SortedGroups, Groups).

%	Group when declarations and their matching heads together
%	and collect the left-over heads.
?- leftOverWhens(X, _, _) when X.
leftOverWhens([], [], []).
%leftOverWhens(Head.Groups, ValidGroups, Head.LeftOvers) :- % LEE
%	Head = _ - clause(_, _, _, _, _),
%	leftOverWhens(Groups, ValidGroups, LeftOvers).
leftOverWhens(When.Groups, group(Key, WG, HG).ValidGroups, LeftOvers) :-
%	When = Key - when(_, _),	% LEE
	When = Key - _,
	keyGroup(Key, When.Groups, WG, HG, RestOfGroups),
	( WG == [] ->				% LEE grungy warning
		putl(user_error,
			"Warning: no when declaration matches clause containing "),
		writeln(user_error, Key)
	),
	leftOverWhens(RestOfGroups, ValidGroups, LeftOvers).

?- keyGroup(_, X, _, _, _) when X.
keyGroup(_, [], [], [], []).
keyGroup(Key1, Groups, WhenGroup, HeadGroup, RestOfGroups) :-
	Groups = X.GroupsT,
	X = (Key2 - Term),
	( Key1 = Key2 ->
		keyGroup(Key1, GroupsT, WhenGroup, HeadGroup, RestOfGroups, Term, Term)
	;	RestOfGroups = Groups,
		WhenGroup = [],
		HeadGroup = []
	).

?- keyGroup(_, _, _, _, _, X, _) when X.				% Really index.
keyGroup(Key, Groups, WhenGroup, Head.HeadGroup, RestOfGroups,
		clause(_, _, _, _, _), Head) :-
	keyGroup(Key, Groups, WhenGroup, HeadGroup, RestOfGroups).
keyGroup(Key, Groups, When.WhenGroup, HeadGroup, RestOfGroups,
		when(_, _), When) :-
	keyGroup(Key, Groups, WhenGroup, HeadGroup, RestOfGroups).

%	Group tuples with the same key together.
?- groupTuples(X, Y) when X or Y.
groupTuples([], []).
groupTuples(Tuple.Tuples, group(Key, Term.Group).Groups) :-
	Tuple = (Key - Term),
	keyPrefix(Key, Tuples, Group, RestOfTuples),
	groupTuples(RestOfTuples, Groups).

?- keyPrefix(_, X, _, _) when X.
keyPrefix(_, [], [], []).
keyPrefix(Key1, Tuples, Group, Rest) :-
	Tuples = (Key2 - Term).TuplesT,
	( Key1 = Key2 ->
		Group = Term.GroupT,
		keyPrefix(Key1, TuplesT, GroupT, Rest)
	;	Rest = Tuples,
		Group = []
	).

%	Make a list of constant when declarations suitable for a (stable) keysort.
?- constantWhenKeys(_, X, _) when X.
constantWhenKeys(_, [], []).
constantWhenKeys(Pos, When.Whens, (Const - When).WhenGroups) :-
	When = when(Pattern, _),
	nth0(Pos, Pattern, Const),
	constantWhenKeys(Pos, Whens, WhenGroups).

%	Make a list of constant heads suitable for a (stable) keysort.
constantHeadKeys(Pos, Heads, HeadGroups) :-
	Pos1 is Pos + 1,
	constantHeadKeys1(Pos1, Heads, HeadGroups).

?- constantHeadKeys1(_, X, _) when X.
constantHeadKeys1(_, [], []).
constantHeadKeys1(Pos, Head.Heads, (Const - Head).HeadGroups) :-
	Head = clause(_, _, Term, _, _),
	arg(Pos, Term, Const),
	constantHeadKeys1(Pos, Heads, HeadGroups).

%	Make a list of struct when declarations suitable for a (stable) keysort.
?- structWhenKeys(_, X, _) when X.
structWhenKeys(_, [], []).
structWhenKeys(Pos, When.Whens, (Pred/Arity - When).WhenGroups) :-
	When = when(Pattern, _),
	nth0(Pos, Pattern, Arg),
	functor(Arg, Pred, Arity),
	structWhenKeys(Pos, Whens, WhenGroups).

%	Make a list of struct heads suitable for a (stable) keysort.
structHeadKeys(Pos, Heads, HeadGroups) :-
	Pos1 is Pos + 1,
	structHeadKeys1(Pos1, Heads, HeadGroups).

?- structHeadKeys1(_, X, _) when X.
structHeadKeys1(_, [], []).
structHeadKeys1(Pos, Head.Heads, (Pred/Arity - Head).HeadGroups) :-
	Head = clause(_, _, Term, _, _),
	arg(Pos, Term, Arg),
	functor(Arg, Pred, Arity),
	structHeadKeys1(Pos, Heads, HeadGroups).

%	Strip off the keys.
?- stripKeys(X, Y) when X or Y.
stripKeys([], []).
stripKeys((_ - X).XT, X.YT) :-
	stripKeys(XT, YT).
