/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog meta-predicates.

?- call(Goal) when Goal.
call(Goal) :-
	$label(Label),
	$call(Goal, Label).

?- $call(Goal, Label) when Goal.
$call((A, B), Label) :-
	!,
	$call(A, Label),
	$call(B, Label).
$call((A -> B; C), Label) :-
	!,
	(call(A) -> $call(B, Label); $call(C, Label)).
$call((A -> B), Label) :-
	!,
	(call(A) -> $call(B, Label)).
$call((A; B), Label) :-
	!,
	(	$call(A, Label)
	;	$call(B, Label)
	).
$call(!, Label) :-
	% !,						% Not necessary.  C.f. $softCut.
	$cutd(Label).
$call($softCut, Label) :-
	!,
	$softCut(Label).
$call(true, _) :-
	!.
$call(fail, _) :-
	!,
	fail.
$call(Goal, _) :-
	$flags(Flags),
	arg(8, Flags, LocalDebug),		% BUG!  Magic Number (localDebug)
	( LocalDebug == trace ->
		spy$watch(Goal)
	;	$funcall(Goal)			% BUG!  Could use $funcallBuiltin(0) directly
	).

%	The perverse argument ordering is for the efficiency of compiled code
%	-- not interpreted!
$exect(P) :- $funcall(P).
call(P, A) :- $funcall(A, P).
call(P, A, B) :- $funcall(A, B, P).
call(P, A, B, C) :- $funcall(A, B, C, P).
call(P, A, B, C, D) :- $funcall(A, B, C, D, P).
call(P, A, B, C, D, E) :- $funcall(A, B, C, D, E, P).
call(P, A, B, C, D, E, F) :- $funcall(A, B, C, D, E, F, P).

%apply(P, A) :- $apply(A, P).
%apply(P, A, B) :- $apply(A, B, P).
%apply(P, A, B, C) :- $apply(A, B, C, P).
%apply(P, A, B, C, D) :- $apply(A, B, C, D, P).
%apply(P, A, B, C, D, E) :- $apply(A, B, C, D, E, P).
%apply(P, A, B, C, D, E, F) :- $apply(A, B, C, D, E, F, P).

\+(P, A) :- \+ call(P, A).
\+(P, A, B) :- \+ call(P, A, B).
\+(P, A, B, C) :- \+ call(P, A, B, C).
\+(P, A, B, C, D) :- \+ call(P, A, B, C, D).
\+(P, A, B, C, D, E) :- \+ call(P, A, B, C, D, E).
\+(P, A, B, C, D, E, F) :- \+ call(P, A, B, C, D, E, F).

not(P, A) :- not call(P, A).
not(P, A, B) :- not call(P, A, B).
not(P, A, B, C) :- not call(P, A, B, C).
not(P, A, B, C, D) :- not call(P, A, B, C, D).
not(P, A, B, C, D, E) :- not call(P, A, B, C, D, E).
not(P, A, B, C, D, E, F) :- not call(P, A, B, C, D, E, F).

%	Freeze may be open-coded by the compiler.
?- freeze(X, _) when X.
freeze(_, Goal) :-
	call(Goal).

%	Call using stored when declarations
%	This is only applicable to dynamic predicates.
sys$wcall(Call) :-
	functor(Call, F, N),
	properties(F, $when(N), WL),
	sys$whensCall(WL, Call).

%	Call using list of when declarations.
?- sys$whensCall(WL, _) when WL.
sys$whensCall([], Call) :-
	sys$callDynamic(Call).
sys$whensCall((WH when WB).WL, Call) :-
	(if gSome WH Call = WH then		% isn't gSome/$is_eq wonderful!
		% Call = WH,					% may need if gSome/$is_eq is changed
		sys$wbcall(WB, Call)
	else
		sys$whensCall(WL, Call)
	).

%	Call a dynamic predicate.
sys$callDynamic(Head) :-
	functor(Head, Pred, Arity),
	$label(L),
	getclause(Pred, Arity, Clause),
	Clause = (Head :- Body),
	$call(Body, L).

%	Call using when body.
sys$wbcall(Ever, Call) :-
	Ever == ever,
	!,
	sys$callDynamic(Call).
sys$wbcall(WB, Call) :-
	sys$waitForWB(WB, Sat),
	sys$freezeDynamic(Sat, Call).

%	Ye Olde Freeze
?- sys$freezeDynamic(V, G) when V.
sys$freezeDynamic(_, G) :-
	sys$callDynamic(G).

%	Wait for the first argument to be ground and bind the second to 'kill'.
?- sys$freezeAndBindToKill(V, _) when V.
sys$freezeAndBindToKill(_V, Kill) :-
	Kill = kill.

%	Bind Sat to 'true' when WB is satisfied.
sys$waitForWB(WB, Sat) :-
	sys$waitForWB(WB, true, Sat, Kill),
	sys$freezeAndBindToKill(Sat, Kill).

%	Bind third arg to second arg (short circuit) when first arg
%	(the body of the when) is satisfied.
%
%	If last arg is bound, just succeed.  This prevents the goal's
%	being called twice when there is a disjunction in the when body.
%
%	Slightly tricky, since vars in when bodies don't have any functor
%	around them.
%	BUG!  What if bound to and/2?
?- sys$waitForWB(WB, _, _, Kill) when WB or Kill.
sys$waitForWB(_, _, _, Kill) :-
	nonvar(Kill),
	!.
sys$waitForWB(A and B, Sat1, Sat, Kill) :-
	!,
	sys$waitForWB(A, Sat1, Sat2, Kill),
	sys$waitForWB(B, Sat2, Sat, Kill).
sys$waitForWB(A or B, Sat1, Sat, Kill) :-
	!,
	sys$waitForWB(A, Sat1, Sat, Kill),
	sys$waitForWB(B, Sat1, Sat, Kill).
sys$waitForWB(ground(A), Sat1, Sat, Kill) :-
	!,
	sys$waitForGround(A, Sat1, Sat, Kill).
sys$waitForWB(_A, Sat, Sat, _Kill).	% plain var (bound)

%	Wait until first arg is ground or fourth argument is 'kill',
%	then short circuit.
?- sys$waitForGround(A, _, _, Kill) when ground(A) or Kill.
sys$waitForGround(_A, Sat, Sat, _Kill).

%	Determine whether a call using stored when declarations is ready to run.
%	This is only applicable to dynamic predicates.
%
%	sys$wcall should probably use this.
sys$readyToRun(Call, Run) :-
	functor(Call, F, N),
	properties(F, $when(N), WL),
	sys$readyToRunWhens(WL, Call, Run).

?- sys$readyToRunWhens(WL, _, _) when WL.
sys$readyToRunWhens([], _, true).
sys$readyToRunWhens((WH when WB).WL, C, Run) :-
	(if gSome WH C = WH then		% isn't gSome/$is_eq wonderful!
		% C = WH,					% may need if gSome/$is_eq is changed
		sys$readyToRunBody(WB, Run)
	else
		sys$readyToRunWhens(WL, C, Run)
	).

sys$readyToRunBody(Ever, Run) :-
	Ever == ever,
	!,
	Run = true.
sys$readyToRunBody(WB, Run) :-
	sys$waitForWB(WB, Run).

%	Ancestors are not recorded for compiled code.
ancestors([]).

subGoalOf(S) :-
	ancestors(A),
	member(S, A).

%	Depth is not recorded for compiled code.
depth(0).

maxDepth(D) :-
	integer(D),
	$setFlag(maxDepth, D).

%	Apply a predicate to each of a list of objects.
%	Used by various system predicates like spy/1.
$applyToEach(X, Y, Goal) :-
	ground(Y),
	( Y == [] ->
		true
	; cons(Y) ->
		\+ (member(X, Y), call(Goal), fail)
	;	X = Y,
		\+ (call(Goal), fail)
	),
	!.

%	Apply a predicate to each of a list of predicate specifications.
%	Accepts ,/2 trees.
%	Used by various system predicates like spy/1.
$applyToEachPredicate(X, Y, Goal) :-
	nonvar(Y),
	( Y == [] ->
		true
	;	(	Y = Y1.Y2
		;	Y = (Y1, Y2)
		),
		$applyToEachPredicate(X, Y1, Goal),
		$applyToEachPredicate(X, Y2, Goal)
	),
	!.
$applyToEachPredicate(X, Y, Goal) :-
	(	atom(Y)
	;	Y = Pred/Arity,
		atom(Pred),
		integer(Arity)
	),
	!,
	\+ (X = Y, call(Goal), fail).

$symbolTable(Name, S) :-
	getprop(Name, $symbolTable, S).

$symbolTableName(S, Name) :-
	arg(0, S, BN),
	$makeObject(atom, BN, Name).

$symbolTableSize(S, Size) :-
	arg(1, S, BS),
	$eRef(BS, LogSize),
	Size is 1 << LogSize.

$symbolTableEntries(S, Entries) :-
	arg(2, S, BE),
	$eRef(BE, Entries).

$symbolTableAddress(S, Address) :-
	arg(4, S, Address).

%	BUG!  Potential machine dependency.
%	Only works if the nil pointer bit pattern is 0.
$validSymbolTableEntry(X) :-
	$block(X),
	$eRef(X, N),
	N \== 0.

currentAtom(Atom) :-
	currentAtom(user, Atom).

currentAtom(Module, Atom) :-
	atom(Atom),
	!,
	currentModule(Module),
	$symbol(Atom, Symbol),
	arg(2, Symbol, Table),
	$symbolTableName(Table, Module).
currentAtom(Module, Atom) :-
	var(Atom),
	currentModule(Module),
	repeat,								% Start again if the table moves.
	$symbolTable(Module, Table),
	$symbolTableSize(Table, Size),
	$symbolTableAddress(Table, Address),
	Size1 is Size - 1,
	between(0, Size1, N),
	$symbolTableAddress(Table, Address),	% Make sure it hasn't moved.
	( N == Size1 ->
		!								% There are no words for this horror!
	),
	arg(N, Address, A),
	$validSymbolTableEntry(A),
	$makeObject(atom, A, Atom).

currentModule(Module) :-
	( var(Module) ->
		currentAtom(user, Module)
	;	atom(Module)
	),
	$symbolTable(Module, _).

currentPredicate(Pred, Arity) :-
	currentPredicate(user, Pred, Arity).

currentPredicate(Module, Pred, Arity) :-
	currentAtom(Module, Pred),
	$currentArity(Pred, Arity).

$currentArity(Pred, Arity) :-
	atom(Pred),
	$predicateArities(Pred, Arities),
	sort(Arities, SortedArities),
	gSome SortedArities member(Arity, SortedArities),
	$defined(Pred, Arity).
