/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog spy point predicates.

debug :- $setFlag(debugging, debug).
trace :- $setFlag(debugging, trace).
nodebug :- $setFlag(debugging, off).
notrace :- $setFlag(debugging, off).

leash(all) :-
	!,
	leash([call, exit, redo, fail, delay, wake]).
leash(List) :-
	ground(List),
	$list(List),
	retractall(spy$leashMode, 1),
	assert(spy$leashMode(List)),
	!.

spy$spying :- \+ $getFlag(localDebug, off).

spy$spying(Pred, Arity) :-
	\+ \+
	(	$getFlag(localDebug, trace)
	;	$getFlag(localDebug, debug),
		$spypoint(Pred, Arity)
	).

spy$leashing(Goal, Port) :-
	\+ \+
	(	(leashCondition(Goal, Port, Condition) ->
			call(Condition)
		;	spy$leashMode(Mode),
			member(Port, Mode)
		)
	).

debugging :-
	$getFlag(debugging, Flag),
	writeln(user, Flag),
	fail.
debugging :-
	spy$leashMode(List),
	format(user, "Tracing will pause at the ports ~w.~n", [List]),
	fail.
debugging :-
	putl(user, "Print warning and fail on undefined predicates\n"),
	fail.
debugging :-
	putl(user, "There are spypoints on\n"),
	$spyingOn(Pred, Arity),
	format(user, "    ~a/~d~n", [Pred, Arity]),
	fail.
debugging.

%	Record of predicates that might be spypoints.
?- initializing, $dynamic($spypoint/2).

%	Call number of call to skip to.
?- initializing, $dynamic(spy$skipping/1).

%	Current default leash mode.
?- initializing, $dynamic(spy$leashMode/1).
spy$leashMode([call, exit, redo, fail, delay, wake]).

%	Conditional skipping of spypoints.
?- initializing, $dynamic(spyCondition/3).

%	Conditional (non-)interaction at tracepoints.
?- initializing, $dynamic(leashCondition/3).

%	Predicate to pipe debugger goals to.
?- initializing, $dynamic(spyHook/2).

%	BUG!  Should change the name of this.
$spyingOn(Pred, Arity) :-
	$spypoint(Pred, Arity),
	$defined(Pred, Arity, _, CodeType),
	($codeType(spypoint, CodeType)
	->	true
	;	retract($spypoint(Pred, Arity)),
		fail
	).

spy(Y) :-
	(prologFlag(debugging, off) -> debug),
	$applyToEachPredicate(X, Y, $spy(X)).

$spy(Pred) :-
	atom(Pred),
	(if some Arity currentPredicate(Pred, Arity) then
		spy$spy(Pred, Arity),
		fail
	else
		format(user_error,
			"~NError in ~w.~n\c
			No predicates defined for name ~a.~n",
			[$spy(Pred), Pred])
	).
$spy(Pred/Arity) :-
	atom(Pred),
	integer(Arity),
	spy$spy(Pred, Arity).

spy$spy(Pred, Arity) :-
	$spyingOn(Pred, Arity),
	!,
	format(user_error,
		"~NError in ~w.~n\c
		Trying to set a spypoint on ~a/~d which is all ready one.~n",
		[spy(Pred/Arity), Pred, Arity]).
spy$spy(Pred, Arity) :-
	systemPredicate(Pred, Arity),
	format(user_error,
		"~NError in ~w.~nTrying to spy on ~a/~d which is a system predicate.~n",
		[spy(Pred/Arity), Pred, Arity]),
	(prologFlag(wizard, on)
	->	format(user_error, "... but you're a wizard, so ....~n", []),
		fail
	),
	!.
spy$spy(Pred, Arity) :-
	$defined(Pred, Arity),
	!,
	asserta($spypoint(Pred, Arity)),
	$spy(Pred, Arity).
spy$spy(Pred, Arity) :-
	format(user_error,
		"~NError in ~w.~n\c
		Trying to set a spypoint on ~a/~d which is undefined.~n",
		[spy(Pred/Arity), Pred, Arity]).

nospy(Y) :-
	$applyToEachPredicate(X, Y, $nospy(X)).

$nospy(Pred) :-
	atom(Pred),
	\+ (
		$spyingOn(Pred, Arity),
		spy$nospy(Pred, Arity),
		fail
	).
$nospy(Pred/Arity) :-
	atom(Pred),
	integer(Arity),
	once $spyingOn(Pred, Arity),
	spy$nospy(Pred, Arity).

nospyall :-
	retract($spypoint(Pred, Arity)),
	$nospy(Pred, Arity),
	fail.
nospyall.

spy$nospy(Pred, Arity) :-
	retract($spypoint(Pred, Arity)),
	!,
	$nospy(Pred, Arity).
spy$nospy(Pred, Arity) :-
	format(user_error,
		"~NError in ~w.~n\c
		Trying to remove a spypoint on ~a/~d which is not a spypoint.~n",
		[nospy(Pred/Arity), Pred, Arity]).

spy$debug(Goal, CN) :-
	spy$debug(Goal, Goal, CN, 0, call).

spy$wakeDebug(Goal, CN) :-
	spy$debug(Goal, Goal, CN, 0, wake).

spy$wakeDebug(Goal, CallGoal, CN) :-
	spy$debug(Goal, CallGoal, CN, 0, wake).

spy$debug(Goal, CallGoal, CN, CD, EntryType) :-
	$getFlag(callNumber, CurrentCN),
	( EntryType == wake ->
		% This has the same effect as not resetting the callNumber at all.
		OldCN = CurrentCN
	;	OldCN = CN
	),
	repeat,
	$label(RedoLabel),
	$setFlag(callNumber, CurrentCN),
	(	$label(FailLabel),
		spy$act(Goal, CN, CD, EntryType, RedoLabel, FailLabel)
	;	$label(FailLabel),
		$setFlag(callNumber, OldCN),
		spy$act(Goal, CN, CD, fail, RedoLabel, FailLabel),
		!,
		fail
	),
	$catch($spyCall(CallGoal, CN), $debugger(Result)),
	( var(Result) ->
		(	$getFlag(delayed, true),
			!,					% BUG!  Invalidates RedoLabel and FailLabel
			$setFlag(delayed, fail),		% Defensive (a.k.a. nervous)
			spy$act(Goal, CN, CD, delay, RedoLabel, FailLabel)
		;	spy$act(Goal, CN, CD, exit, RedoLabel, FailLabel)
		;	$setFlag(callNumber, CurrentCN),
			spy$act(Goal, CN, CD, redo, RedoLabel, FailLabel),
			fail
		)
	; Result = redo(ThrownCN) ->
		(ThrownCN >= CN ->
			$cutd(RedoLabel),
			fail
		;	!,
			throw($debugger(Result))
		)
	; Result = fail(ThrownCN) ->
		(ThrownCN >= CN ->
			$cutd(FailLabel),
			fail
		;	!,
			throw($debugger(Result))
		)
	; Result = ancestors(N) ->
		(N > 0 ->
			%prologFlag(localDebug, OldFlag, off),	% Don't trace format.
			format(user, "++ (~d) ~d : ", [CN, CD]),
			%$setFlag(localDebug, OldFlag),
			$getFlag(printDepth, PrintDepth),
			print(user, Goal, PrintDepth),
			nl(user),
			N1 is N - 1,
			N1 > 0,
			throw($debugger(ancestors(N1)))
		;	fail
		)
	).

$spyCall(Goal, CN) :-
	$execs(Goal, CN).

%	Control for a spy-point.
spy$act(Goal, CN, CD, Port, RedoLabel, FailLabel) :-
	spy$skip(CN, Skip),
	prologFlag(localDebug, Debug, off),
	(	true
	;	$setFlag(localDebug, Debug),		% All paths must reset localDebug
		fail
	),
	functor(Goal, Pred, Arity),
	( \+ $spypoint(Pred, Arity) ->
		once
		(	Debug = trace
		;	Skip = skip
		),
		( spy$leashing(Goal, Port) ->
			spy$act2(Goal, CN, CD, Port, Debug, Skip, RedoLabel, FailLabel)
		;	spy$printPort(CN, CD, Port, Skip, Goal),
			nl(user),
			$setFlag(localDebug, Debug)
		)
%	;	( Debug == debug ->	
%			$getFlag(debugging, Trace),			% Recover from leap
%			$setFlag(localDebug, Trace)
%		;	Debug = trace
%		),
	;	once
		(	Debug = debug
		;	Debug = trace
		),
		(spyCondition(Goal, Port, Condition) -> call(Condition)),
		spy$act2(Goal, CN, CD, Port, Debug, Skip, RedoLabel, FailLabel)
	),
	!.
spy$act(_, _, _, _, _, _).

spy$act2(Goal, CN, CD, Port, Local, Skip, RedoLabel, FailLabel) :-
	repeat,
	spy$printPort(CN, CD, Port, Skip, Goal),
	once (
		repeat,
		get0(user, Action),
		Action =:= 0'\n or Action =:= -1 or 32 =< Action and Action < 127
	),
	spy$spyAction(Action, Goal, CN, CD, Port, Local, RedoLabel, FailLabel),
	!.

spy$skip(CN, skip) :-
	spy$skipping(SkipCN),
	CN =< SkipCN,
	retract(spy$skipping(SkipCN)),
	!,
	$getFlag(debugging, Debug),
	$setFlag(localDebug, Debug).
spy$skip(_, no).

spy$printPort(CN, CD, Port, Skip, Goal) :-
	(functor(Goal, Pred, Arity), $spypoint(Pred, Arity) ->
		Mode = (spy)
	;	Mode = trace
	),
	spy$modeString(Mode, Skip, ModeString),
	format(user, "~s (~d) ~d ~a: ", [ModeString, CN, CD, Port]),
	$getFlag(printDepth, Depth),
	print(user, Goal, Depth),
	putl(user, "? "),
	flushOutput(user),					% BUG? in System V line-buffering
	fail.
spy$printPort(_, _, _, _, _).

spy$modeString(spy, no, "**").
spy$modeString(spy, skip, "*>").
spy$modeString(trace, skip, " >").
spy$modeString(trace, no, "  ").

spy$skipLine :-
	skip(user, 0'\n).

spy$getNumber(J) :-
	getl(user, Line),
	tokenize(Line, J, number, _),
	integer(J).

%	Note that localDebug should be off before this is called
%	to ensure that none of the various bits of code called get
%	debugged.  The various options that succeed will turn it
%	back on again if necessary.
spy$spyAction(-1, _, _, _, _, _, _, _) :-
	!,
	clearIOError(user_input),
	$close(user_input),				% Re-open user_input on terminal
	fail.
spy$spyAction(0'\n, _, _, _, _, _, _, _) :-
	!,
	$setFlag(localDebug, trace).
spy$spyAction(0'+, Goal, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	functor(Goal, Pred, Arity),
	(predicateProperty(Pred, Arity, spypoint) ->
		true
	;	spy(Pred/Arity)
	),
	fail.
spy$spyAction(0'-, Goal, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	functor(Goal, Pred, Arity),
	(predicateProperty(Pred, Arity, spypoint) ->
		nospy(Pred/Arity)
	),
	fail.
spy$spyAction(0'@, _, _, _, _, _, _, _) :-
	!,
	read(Command),
	once call(Command),
	fail.
spy$spyAction(0'=, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	debugging,			% BUG!  Refers to debugging rather than localDebug
	fail.
spy$spyAction(0'<, _, _, _, _, _, _, _) :-
	!,
	((spy$getNumber(J), J > 0) ->
		true
	;	J is maxint
	),
	$setFlag(printDepth, J),
	fail.
spy$spyAction(0'?, _, _, _, _, _, _, _) :-
	!,
	spy$spyAction(0'h, _, _, _, _, _, _, _).
spy$spyAction(0'|, Goal, CN, CD, Port, Local, _, _) :-
	!,
	spy$skipLine,
	spyHook(info(CN, CD, Port, Local), Goal),
	fail.
spy$spyAction(0'a, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	abort.
spy$spyAction(0'b, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	break,
	fail.
spy$spyAction(0'c, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	$setFlag(localDebug, trace).
spy$spyAction(0'd, X, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	display(user, X), nl(user),
	fail.
spy$spyAction(0'f, _, CN, _, Port, Local, _, FailLabel) :-
	!,
	(Port = fail ->
		spy$skipLine,
		fail
	; spy$getNumber(J), J < CN ->
		$setFlag(localDebug, Local),
		throw($debugger(fail(J)))
	;	$cutd(FailLabel),
		$setFlag(localDebug, Local),
		fail
	).
spy$spyAction(0'g, _, _, _, _, _, _, _) :-
	!,
	(spy$getNumber(J) ->
		true
	;	J is maxint					% BUG!
	),
	throw($debugger(ancestors(J))).	% Fails back here after printing ancestors.
spy$spyAction(0'h, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	putl(user,
"Prolog debugging options:
    <cr>  creep
    +/-   spy/nospy
    @     single command
    =     debugging
    < <n> set print depth
    |     spyHook(Info, Goal)
    a     abort
    b     break
    c     creep
    d     display
    f     fail\n"),
	putl(user,	% BUG!  String too long for lex on some machines -- split it.
"    f <n> fail <n>
    g     ancestors
    g <n> last <n> ancestors
    h/?   help
    l     leap
    n     nodebug
    p     print
    r     redo
    r <n> redo <n>
    s     skip
    w     write\n"
	),
	fail.
spy$spyAction(0'l, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	$setFlag(localDebug, debug).
spy$spyAction(0'n, _, _, _, _, _, _, _) :-
	!,
	spy$skipLine.
spy$spyAction(0'p, X, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	$getFlag(printDepth, Depth),
	print(user, X, Depth),
	nl(user),
	fail.
spy$spyAction(0'r, _, CN, _, _, Local, RedoLabel, _) :-
	!,
	((spy$getNumber(J), J < CN) ->
		$setFlag(localDebug, Local),
		throw($debugger(redo(J)))
	;	$cutd(RedoLabel),
		$setFlag(localDebug, Local),
		fail
	).
spy$spyAction(0's, _, CN, _, Mode, _, _, _) :-
	!,
	spy$skipLine,
	((Mode == call ; Mode == wake; Mode == redo) ->
		%$setFlag(localDebug, off),		% BUG!  Should this be skip?
		assert(spy$skipping(CN))
	;	$setFlag(localDebug, trace)
	).
spy$spyAction(0'w, X, _, _, _, _, _, _) :-
	!,
	spy$skipLine,
	write(user, X),
	nl(user),
	fail.
spy$spyAction(_, _, _, _, _, _, _, _) :-
	spy$skipLine,
	fail.

?- spy$watch(Goal) when Goal.
spy$watch(Goal) :-
%	BUG!?
	$getFlag(callNumber, CN),
	$label(Label),
	spy$watch(Goal, Label, 0, []).

?- spy$watch(Goal, _, _) when Goal.
spy$watch(Goal, Depth, Goals) :-
	$label(Label),
	spy$watch(Goal, Label, Depth, Goals).

?- spy$watch(Goal, _, _, _) when Goal.
spy$watch((A, B), Label, Depth, Goals) :-
	!,
	spy$watch(A, Label, Depth, Goals),
	spy$watch(B, Label, Depth, Goals).
spy$watch((A -> B; C), Label, Depth, Goals) :-
	!,
	(spy$watch(A, Depth, Goals) -> 
		spy$watch(B, Label, Depth, Goals)
	;	spy$watch(C, Label, Depth, Goals)).
spy$watch((A -> B), Label, Depth, Goals) :-
	!,
	(spy$watch(A, Depth, Goals) ->
		spy$watch(B, Label, Depth, Goals)).
spy$watch((A; B), Label, Depth, Goals) :-
	!,
	(	spy$watch(A, Label, Depth, Goals)
	;	spy$watch(B, Label, Depth, Goals)
	).
spy$watch(!, Label, _, _) :-
	% !,						% Not necessary.  C.f. $softCut.
	$cutd(Label).
spy$watch($softCut, Label, _, _) :-
	!,
	$softCut(Label).
spy$watch(true, _, _, _) :-
	!.
spy$watch(fail, _, _, _) :-
	!,
	fail.
spy$watch(ancestors(Ancestors0), _, _, Ancestors) :-
	!,
	Ancestors0 = Ancestors.
spy$watch(subGoalOf(S), _, _, Goals) :-
	!,
	member(S, Goals).
spy$watch(depth(Depth0), _, Depth, _) :-
	!,
	Depth0 = Depth.
spy$watch(Goal, _, Depth, Goals) :-
	$getFlag(callNumber, CN),
	CN1 is CN + 1,
	$setFlag(callNumber, CN1),
	spy$debug(Goal, spy$watchCall(Goal, CN, Depth, Goals), CN, Depth, call).

spy$watchCall(Goal, CN, Depth, Goals) :-
	((functor(Goal, Pred, Arity), predicateProperty(Pred, Arity, dynamic)) ->
		$getFlag(maxDepth, MaxDepth),
		Depth < MaxDepth,					% BUG?  Report failure?
		sys$readyToRun(Goal, Ready),
		(var(Ready) ->
			spy$watchCallWhenReady(Ready, Goal, CN, Depth, Goals),
			$setFlag(delayed, true)
		;	$label(Label),
			getclause(Pred, Arity, Clause),
			Clause = (Goal :- Body),
			Depth1 is Depth + 1,
			spy$watch(Body, Label, Depth1, Goal.Goals)
		)
	;	$funcall(Goal)
	).

?- spy$watchCallWhenReady(Ready, _, _, _, _) when Ready.
spy$watchCallWhenReady(_Ready, Goal, CN, Depth, Goals) :-
	spy$debug(Goal, spy$callDynamic(Goal, CN, Depth, Goals), CN, Depth, wake).

%	BUG!?  What do I do with CN?
spy$callDynamic(Goal, CN, Depth, Goals) :-
	functor(Goal, Pred, Arity),
	$label(Label),
	getclause(Pred, Arity, Clause),
	Clause = (Goal :- Body),
	Depth1 is Depth + 1,
	spy$watch(Body, Label, Depth1, Goal.Goals).
