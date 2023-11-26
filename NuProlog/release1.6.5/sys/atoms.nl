/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog predicates to do with atoms.

$propertyMember(X, $bmt(_, _, _, _, _, Y)) :-
	$propertyMember1(X, Y).

$propertyMember1(X, Y) :-
	Y = $bmt(I, _, _, _, _, Succ),		% 'Ware the interaction with retract/1.
	integer(I),
	(	X = Y
	;	$propertyMember1(X, Succ)
	).

$propField(Atom, PropField) :-
	( atom(Atom) ->
		true
	;	format(user_error,
			"~NAtom rather than ~w expected in $propField/2.~n", [Atom]),
		fail
	),
	$symbol(Atom, Def),
	arg(4, Def, PropField).

%	Raw instances of all the properties of Atom under Key.
$rawProperties(Atom, Key, Properties) :-
	( ground(Key) ->
		true
	;	format(user_error,
			"~NGround Key rather than ~w expected in $rawProperties/3.~n",
			[Key]),
		fail
	),
	$proplist(Atom, Key, Properties).

%	Instances of all the properties of Atom under Key.
properties(Atom, Key, Properties) :-
	$rawProperties(Atom, Key, RawProperties),
	$uncopyProperties(RawProperties, Properties).

%	Warning: no error checking.
?- $uncopyProperties(P, _) when P.
$uncopyProperties([], []).
$uncopyProperties($prop(_, P, I).PropList, Properties) :-
	(instance($ref(P, I), _Key, Copy) ->
		Properties = Copy.RestOfProperties
	;	Properties = RestOfProperties
	),
	$uncopyProperties(PropList, RestOfProperties).

clauses(Pred, Arity, Clauses) :-
	$clauses(Pred, Arity, Clauses).

$clauses(Pred, Arity, Clauses) :-
	properties(Pred, $clause(Arity), Clauses).

%	Memory efficient version of
%		clauses(Pred, Arity, Clauses), member(Clause, Clauses).
getclause(Pred, Arity, Clause) :-
	$getclause(Pred, Arity, Clause, _Ref).

getclause(Pred, Arity, Clause, Ref) :-
	$getclause(Pred, Arity, Clause, Ref).

$getclause(Pred, Arity, Clause) :-
	$getclause(Pred, Arity, Clause, _Ref).

$getclause(Pred, Arity, Clause, Ref) :-
	$rawProperties(Pred, $clause(Arity), RawClauses),
	Ref = $ref(P, I),
	member($prop(_, P, I), RawClauses),
	instance(Ref, _Key, Clause).

whens(Pred, Arity, Whens) :-
	properties(Pred, $when(Arity), Whens).

%	Memory efficient version of
%		whens(Pred, Arity, Whens), member(When, Whens).
getwhen(Pred, Arity, When) :-
	getwhen(Pred, Arity, When, _Ref).

getwhen(Pred, Arity, When, Ref) :-
	$rawProperties(Pred, $when(Arity), RawWhens),
	Ref = $ref(P, I),
	member($prop(_, P, I), RawWhens),
	instance(Ref, _Key, When).

getprop(Atom, Key, Prop) :-
	( atom(Atom) ->
		true
	;	format(user_error,
			"~NAtom rather than ~w expected in getprop/3.~n",
			[Atom]),
		fail
	),
	$symbol(Atom, Def),
	arg(4, Def, PropList),
	$propertyMember(BMT, PropList),
	instance(BMT, Key, Prop).

getprop(Atom, Key, Prop, Ref) :-
	Ref = $ref(P, I),
	$getprop(Atom, $prop(Key, P, I)),
	instance(Ref, Prop).

$getprop(Atom, Prop) :-
	( atom(Atom) ->
		true
	;	format(user_error,
			"~NAtom rather than ~w expected in $getprop/2.~n",
			[Atom]),
		fail
	),
	$symbol(Atom, Def),
	arg(4, Def, PropList),
	Prop = $prop(Key, P, I),
	$propertyMember(BMT, PropList),
	BMT = $bmt(I, _, _, KeySkel, _, _),
	$uncopy(KeySkel, 0, Key),
	$makeObject($block, BMT, P).

putprop(Atom, Key, Prop) :-
	putprop(Atom, Key, Prop, _Ref).

%	BUG!  Should use Ref to determine which property to change.
putprop(Atom, Key, Prop, $ref(P, I)) :-
	$propErrorCheck(Atom, Key, putprop),
%	$makeProplist(Atom, PropList),
	$propField(Atom, PropList),
	$makeBMT(Key, Prop, BMT, P),
	BMT = $bmt(I, _, _, _, _, _),
	((	$propertyMember(OldBMT, PropList),
		OldBMT = $bmt(_, _, _, Key, Pred, _)
		)
	->	$linkBMT(1, BMT, Pred),
		erase(OldBMT)
	;	$linkBMT(1, BMT, Atom)
	).

addpropa(Atom, Key, Prop) :-
	addpropa(Atom, Key, Prop, _Ref).

addpropa(Atom, Key, Prop, $ref(P, I)) :-
	$propErrorCheck(Atom, Key, addpropa),
	$makeBMT(Key, Prop, BMT, P),
	BMT = $bmt(I, _, _, _, _, _),
%	$makeProplist(Atom, PropList),
	$linkBMT(1, BMT, Atom).

addprop(Atom, Key, Prop, Ref) :-
	addpropz(Atom, Key, Prop, Ref).

addprop(Atom, Key, Prop) :-
	addpropz(Atom, Key, Prop, _Ref).

addpropz(Atom, Key, Prop) :-
	addpropz(Atom, Key, Prop, _Ref).

addpropz(Atom, Key, Prop, $ref(P, I)) :-
	$propErrorCheck(Atom, Key, addpropz),
	$makeBMT(Key, Prop, BMT, P),
	BMT = $bmt(I, _, _, _, _, _),
%	$makeProplist(Atom, PropList),
%	PropList = $bmt(_, _, _, _, Pred, _),
	$linkBMT(0, BMT, Atom).

$propErrorCheck(Atom, Key, Pred) :-
	( atom(Atom) ->
		true
	;	format(user_error,
			"~NAtom rather than ~w expected in ~a/3.~n",
			[Atom, Pred]),
		fail
	),
	( ground(Key) ->
		true
	;	format(user_error,
			"~NGround key rather than ~w expected in ~a/3.~n",
			[Atom, Pred]),
		fail
	).

%	Remove all the properties under Key attached to Atom.
remprop(Atom, Key) :-
	( ground(Key) ->
		$proplist(Atom, Key, PropList)
	;	$proplist(Atom, PropList)
	),
	Ref = $ref(P, I),
	member($prop(Key, P, I), PropList),
	erase(Ref),
	fail.
remprop(_, _).

%	Remove all the properties Prop under Key attached to Atom.
remprop(Atom, Key, Prop) :-
	( ground(Key) ->
		$proplist(Atom, Key, PropList)
	;	$proplist(Atom, PropList)
	),
	Ref = $ref(P, I),
	member($prop(Key, P, I), PropList),
	instance(Ref, Prop),
	erase(Ref),
	fail.
remprop(_, _, _).

%	The assert, retract, retractall/2, abolish and clause predicates
%	call $-prefixed versions of themselves.  This is to make the kind
%	of additions that the database systems like to make to these
%	predicates easy.

$asserta(Clause, Ref) :-
	var(Clause),
	!,
	format(user_error, "~NError in ~w.~n", [asserta(Clause)]),
	format(user_error, "~NVariable unexpected.~n", []),
	fail.
$asserta(Clause, Ref) :-
	( Clause = (Head :- _Body)
	->	(var(Head) ->
			format(user_error, "~NError in ~w.~n", [asserta(Clause)]),
			format(user_error, "~NVariable head unexpected.~n", []),
			fail
		),
		functor(Head, Atom, Arity),
		$makeDynamic(Atom, Arity),
		addpropa(Atom, $clause(Arity), Clause, Ref)
	;	functor(Clause, Atom, Arity),
		$makeDynamic(Atom, Arity),
		addpropa(Atom, $clause(Arity), (Clause :- true), Ref)
	).

asserta(X) :-
	$asserta(X, _Ref).

asserta(X, Ref) :-
	$asserta(X, Ref).

$assertz(Clause, Ref) :-
	var(Clause),
	!,
	format(user_error, "~NError in ~w.~n", [assertz(Clause)]),
	format(user_error, "~NVariable unexpected.~n", []),
	fail.
$assertz(Clause, Ref) :-
	( Clause = (Head :- _Body) ->
		( var(Head) ->
			format(user_error, "~NError in ~w.~n", [asserta(Clause)]),
			format(user_error, "~NVariable head unexpected.~n", []),
			fail
		),
		functor(Head, Atom, Arity),
		$makeDynamic(Atom, Arity),
		addpropz(Atom, $clause(Arity), Clause, Ref)
	;	functor(Clause, Atom, Arity),
		$makeDynamic(Atom, Arity),
		addpropz(Atom, $clause(Arity), (Clause :- true), Ref)
	).

assertz(X) :-
	$assertz(X, _Ref).

assertz(X, Ref) :-
	$assertz(X, Ref).

assert(X) :-
	assertz(X, _Ref).

assert(X, Ref) :-
	assertz(X, Ref).

$makeDynamic(Pred, Arity) :-
	( dynamic(Pred, Arity) ->
		true
	;	$dynamic(Pred/Arity, fail)
	).

$makeDynamicWithWhens(Pred, Arity) :-
	( dynamic(Pred, Arity), $hasWhens(Pred, Arity) ->
		true
	;	$dynamic(Pred/Arity, true)
	).

$retract(Clause) :-
	var(Clause),
	!,
	format(user_error, "~NError in ~w.~n", [retract(Clause)]),
	format(user_error, "~NVariable unexpected.~n", []),
	fail.
$retract(Clause) :-
	( Clause = (Head :- _) ->
		( var(Head) ->
			format(user_error, "~NError in ~w.~n", [retract(Clause)]),
			format(user_error, "~NVariable head unexpected.~n", []),
			fail
		),
		$iretract(Clause)
	;	$iretract((Clause :- _))
	).

retract(Clause) :-
	$retract(Clause).

$iretract(Clause) :-
	Clause = (Head :- _),
	functor(Head, Atom, Arity),
	$proplist(Atom, PropList),
	member($prop($clause(Arity), P, I), PropList),
	instance($ref(P, I), _, Clause),
	erase($ref(P, I)).

%	Retract all the interpreted clauses for Pred/Arity.
%	Much faster than (retract(_), fail).
$retractall(Pred, Arity) :-
	( \+ atom(Pred) ->
		format(user_error,
			"~NError in ~w -- atom expected.~n",
			[retractall(Pred, Arity)]),
		fail
	),
	( \+ integer(Arity) ->
		format(user_error, "~NError in ~w.~n", [retractall(Pred, Arity)]),
		format(user_error, "~NInteger expected.~n", []),
		fail
	),
	remprop(Pred, $clause(Arity)),
	remprop(Pred, $when(Arity)).

retractall(Pred, Arity) :-
	$retractall(Pred, Arity).

retractall(Term) :-
	retract(Term),
	fail.
retractall(_).

%	Completely remove a predicate.
$abolish(Pred, Arity) :-
	( \+ atom(Pred) ->
		format(user_error,
			"~NError in ~w -- atom expected.~n",
			[abolish(Pred, Arity)]),
		fail
	),
	( \+ integer(Arity) ->
		format(user_error, "~NError in ~w.~n", [abolish(Pred, Arity)]),
		format(user_error, "~NInteger expected.~n", []),
		fail
	),
	( systemPredicate(Pred, Arity) ->
		format(user_error, "~NError in ~w.~n", [abolish(Pred, Arity)]),
		format(user_error, "~NAbolishing system predicate.~n", []),
		( prologFlag(wizard, on) ->
			format(user_error, "~NBut you're a wizard so . . . .~n", [])
		;	fail
		)
	),
	remprop(Pred, $clause(Arity)),
	remprop(Pred, $when(Arity)),
	remprop(Pred, $property(Arity)),
	(	retract($spypoint(Pred, Arity)),
		fail
	;	true
	),
	$abolishCode(Pred, Arity).

abolish(Pred, Arity) :-
	$abolish(Pred, Arity).

abolish(Preds) :-
	\+ (
		member(Pred/Arity, Preds),
		abolish(Pred, Arity),
		fail
	).

%	Backtrack over the clauses matching Head.
%	Error checking (and delaying?) needed.
$clause(Head, Body, Ref) :-
	functor(Head, Pred, Arity),
	getclause(Pred, Arity, (Head :- Body), Ref).

clause(Head, Body) :-
	$clause(Head, Body, _Ref).

clause(Head, Body, Ref) :-
	$clause(Head, Body, Ref).

%	Quintus compatible ways to get at the property list.
%
%	There are times when I really think that I should have hung property
%	lists off functors.
record(Key, Term, Ref) :-
	recordz(Key, Term, Ref).

recorda(Key, Term, Ref) :-
	functor(Key, Atom, Arity),
	atom(Atom),
	!,
	addpropa(Atom, $recorded(Arity), Term, Ref).
recorda(Key, Term, Ref) :-
	integer(Key),
	!,
	addpropa($recorded, $recorded(integer(Key)), Term, Ref).
recorda(Key, Term, Ref) :-
	format(user_error, "~NError in ~w.~n", [recorda(Key, Term, Ref)]),
	format(user_error, "~NAtomic key expected rather than ~w.~n", [Key]),
	fail.

recordz(Key, Term, Ref) :-
	functor(Key, Atom, Arity),
	atom(Atom),
	!,
	addpropz(Atom, $recorded(Arity), Term, Ref).
recordz(Key, Term, Ref) :-
	integer(Key),
	!,
	addpropz($recorded, $recorded(integer(Key)), Term, Ref).
recordz(Key, Term, Ref) :-
	format(user_error, "~NError in ~w.~n", [recordz(Key, Term, Ref)]),
	format(user_error, "~NAtomic key expected rather than ~w.~n", [Key]),
	fail.

recorded(Key, Term, Ref) :-
	functor(Key, Atom, Arity),
	atom(Atom),
	!,
	getprop(Atom, $recorded(Arity), Term, Ref).
recorded(Key, Term, Ref) :-
	integer(Key),
	!,
	getprop($recorded, $recorded(integer(Key)), Term, Ref).
recorded(Key, Term, Ref) :-
	format(user_error, "~NError in ~w.~n", [recorded(Key, Term, Ref)]),
	format(user_error, "~NAtomic key expected rather than ~w.~n", [Key]),
	fail.

currentKey(Key) :-
	( nonvar(Key) ->
		( integer(Key) ->
			getprop($recorded, $recorded(integer(Key)), _)
		;	functor(Key, Atom, Arity),
			atom(Atom),
			getprop(Atom, $recorded(Arity), _)
		)
	;	getprop($recorded, $recorded(integer(Key)), _)
	;	currentAtom(Atom),
		getprop(Atom, $recorded(Arity), _),
		functor(Key, Atom, Arity)
	).
