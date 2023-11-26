/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart
 */

% Nepolog mainloop.

main :-
	$mainloop.

main(Args) :-
	$mainloop(Args).

$mainloop :-
	$mainloop([]).

$mainloop(Args) :-
	$setMainloopSignals,
	(\+ getprop($mainloop, history$cnum, _)	% Initialize if mainloop inactive.
	->	putprop($mainloop, history$cnum, 1),
		%	The following is a unix dependent method for deciding
		%	which editor to invoke to edit history list entries.
		(	getenv(Name, Value1),
			( Name = 'VISUAL' ; Name = 'EDITOR' ),
			cons(Value1)
		->	( Value1 == "vi" ->
				Value = "ex +1o "
			; append(Value2, "/vi", Value1) ->
				append(Value2, "/ex +1o ", Value)
			;	append(Value1, " ", Value)
			)
		;	Value = "/bin/ed "
		),
		%	Value is now a string representing editor name + a space
		putprop($mainloop, $editor, Value)

	),
	$mainloop(Args, 0).

$setMainloopSignals :-
	signal(sigint, $break).

%	BUG!  The control involved in printing the correct entry message is
%	rapidly becoming quite stupid!
$mainloop(_Args, Break) :-
/* feof NYI
	(feof(user_input) -> clearIOError(user_input), $close(user_input)),
*/
	( getprop($mainloop, $restart, G) ->
		once remprop($mainloop, $restart, G),
		$setFlag(localDebug, off),
		\+ (once call(G), fail),			% Yeuch!
		putl(user, "Re-entering prolog top-level.\n")
	;	$writeEntryMessage(Break)
	),
	$setFlag(localDebug, off),
	putprop($mainloop, $breaklevel, Break),
	repeat,
	once prompt,
	getTokenList(user, TL),
	$runGoal(TL),
	!,
	$getFlag(debugging, Debugging),
	$setFlag(localDebug, Debugging).	% In case this is a break

$runGoal(TL) :-
	ot$var(TL),
	!,
	format(user_error, "~NVariable goal to top-level.~n", []),
	fail.
$runGoal(TL) :-
	ot$eof(TL),
	!,
	breakLevel(Break),
	$writeExitMessage(Break),
	NewBreak is Break - 1,
	putprop($mainloop, $breaklevel, NewBreak),
	clearIOError(user_input).		% in case this is a break level
$runGoal((h.atom).[]) :-
	!,
	$history.
$runGoal(TL) :-
	(TL = [(e.atom), (ECNum.number)] ; TL = (e.atom).[], ECNum = -1),
	!,
	( historyGoal(ECNum, TL1) ->
		(	getpid(Pid),
			intToString(Pid, LPid),
			append("/tmp/np", LPid, EFile),
			open(EFile, write, Stream),
			putTokenList(Stream, TL1),
			close(Stream),
			getprop($mainloop, $editor, Editor),
			append(Editor, EFile, ECommand),
			( system(ECommand, 0) ->
				open(EFile, read, Stream1),
				getTokenList(Stream1, TL2),
				close(Stream1),
				unlink(EFile),
				\+ ot$var(TL2),
				\+ ot$eof(TL2),
				$addHistory(TL2),
				!,
				$rGoal(TL2)
			;	putl(user, "Error: Unable to invoke editor.\n"),
				unlink(EFile)
			)
		;	putl(user, "Error: Temp file for history list update.\n")
		)
	;	putl(user, "Usage: e [N]. where N is a history list number.\n")
	),
	!,
	fail.
$runGoal([(s.atom), (Filename.T)]) :-
	!,
	( (T == atom ; T == quoted) ->
		(	open(Filename, write, Stream),
			(	getprop($mainloop, history$goal, _ - TL),
				putTokenList(Stream, TL),
				fail
			;	true
			),
			close(Stream),
			putl(user, "History list saved.\n")
		;	putl(user, "Error: Save file for history list.\n")
		)
	;	putl(user, "Usage: s Filename. where Filename is an atom.\n")
	),
	!,
	fail.
$runGoal([(r.atom), (Filename.T)]) :-
	!,
	( (T == atom ; T == quoted) ->
		(	open(Filename, read, Stream),
			repeat,
			getTokenList(Stream, TL),
			\+ ot$var(TL),
			(	ot$eof(TL)
			;	once $addHistory(TL),
				fail
			),
			putl(user, "History list restored.\n"),
			close(Stream)
		;	putl(user, "Error: Restore file for history list.\n")
		)
	;	putl(user, "Usage: r Filename. where Filename is an atom.\n")
	),
	!,
	fail.
$runGoal(TL) :-
	( TL = (Num.number).[] ->
		(	historyGoal(Num, TL1),
			putTokenList(user, TL1),
			$addHistory(TL1),
			!,
			$rGoal(TL1)
		;	putl(user, "Goal not in history list.\n"),
			!,
			fail
		)
	;	$addHistory(TL),
		!,
		$rGoal(TL)
	).

historyLength(20).		% User can change this.

$addHistory(TL) :-
	getprop($mainloop, history$cnum, CNum),
	addpropz($mainloop, history$goal, CNum - TL),
	CNum1 is CNum + 1,
	putprop($mainloop, history$cnum, CNum1),
	historyLength(HL),
	( CNum > HL ->
		OldCNum is CNum - HL,
		remprop($mainloop, history$goal, OldCNum - _)
	).
	
historyGoal(I, TokenGoal) :-
	integer(I),
	( I >= 0 ->
		GNum = I
	;	commandNumber(CNum),
		GNum is CNum + I
	),
	once getprop($mainloop, history$goal, GNum - TokenGoal).

$history :-
	getprop($mainloop, history$goal, CNum - TL),
	once (
		format(user, "~d\t", [CNum]),
		putTokenList(user, TL)
	),
	fail.

$rGoal(TL) :-
	once treadTerm(TL, Goal, VarNames, Vars),
	$label(GoalLabel),
	catch($rGoal(Goal, VarNames, Vars), Result),
	( var(Result) ->
		$cutd(GoalLabel)
	; Result = $debugger(ancestors(_)) ->
		fail
	;	$cutd(GoalLabel),
		duplicate(Goal+Vars, Goal2+Vars2),
		$matchNameList(VarNames, Vars2),
		format(user_error,
			"~NError in running ~w.~nUncaught thrown message ~w.~n",
			[Goal2, Result])
	),
	fail.

$rGoal((T : B), _VarNames, _Vars) :-
	!,
	'$:'(T, B).
$rGoal(:(B), _VarNames, _Vars) :-
	!,
	'$:'(B).
$rGoal((insert B), _VarNames, _Vars) :-
	!,
	$insert(B).
$rGoal((delete B), _VarNames, _Vars) :-
	!,
	$delete(B).
$rGoal((update B), _VarNames, _Vars) :-
	!,
	$update(B).
$rGoal(Goal, VarNames, Vars) :-
	(
		once
		(	$transformOcall(Goal, VarNames, Vars, VarNames1, Vars1),
			(VarNames1 = [] ->
				putl(user, "true.\n")
			;	duplicate(Vars1, Vars2),
				$matchNameList(VarNames1, Vars2),
				$writeGoalVars(VarNames1, Vars2),
				flushOutput(user),			% BUG? in System V line-buffering
				$moreSolutions
			)
		)
	;	putl(user, "fail.\n")
	).

$transformOcall(Goal, VarNames, Vars, VarNames1, Vars1) :-
	$extra_vars(Goal, [], EV),
	$rem_non_ev(EV, VarNames, Vars, VarNames1, Vars1),
	$unique_vars(Goal, U),		% U is implicitly
	$call_trf(Goal, U, Goal1),	% quantified vars in Goal
	$ocall(Goal1).

% Extract non-explicitly quantified vars from var name list and var list.
$rem_non_ev(_, [], [], [], []).
$rem_non_ev(EV, VN.VNL, V.VL, VN.VNL1, V.VL1) :-
	occurs(V, EV),
	!,
	$rem_non_ev(EV, VNL, VL, VNL1, VL1).
$rem_non_ev(EV, _.VNL, _.VL, VNL1, VL1) :-
	$rem_non_ev(EV, VNL, VL, VNL1, VL1).

$writeEntryMessage(0) :-
	!,
	( getprop($mainloop, $breaklevel, _) ->
		putl(user, "Prolog abort.\n")
	;	$version(V0, V1, V2),
		format(user, "NU-Prolog ~d.~d.~d\n", [V0, V1, V2])
	).
$writeEntryMessage(Break) :-
%	Break > 0,
	format(user, "~N[ Break (level ~d) ]~n", [Break]).

$writeExitMessage(0) :-
	!,
	putl(user, "End of Session\n").
$writeExitMessage(_) :-
%	Break > 0,
	nl(user).

$version(V0, V1, V2) :-
	prologFlag(version, V),
	V0 is V // 10000,
	V1 is (V // 100) mod 100,
	V2 is V mod 100.

breakLevel(Break) :-
	once getprop($mainloop, $breaklevel, Break).

commandNumber(CNum) :-
	once getprop($mainloop, history$cnum, CNum).

%	This allows the user to define their own prompts.
prompt :-
	breakLevel(Break),
	commandNumber(CNum),
	( Break == 0 ->
		format(user, "~d?- ", [CNum])
	;	format(user, "[~d] ~d?- ", [Break, CNum])
	),
	flushOutput(user).				% BUG? in System V line-buffering

$matchNameList([], []) :-
	!.
$matchNameList(Name.VarNames, Val.Values) :-
	( var(Val) ->
		Val = write$var(Name)
	),
	$matchNameList(VarNames, Values).

$writeGoalVars(Name.VarNames, Val.Values) :-
	write(user, write$var(Name)),
	putl(user, " = "),
	once (
		currentOp(Prec, F, '='),
		name(F, [_, 0'f, _])
	),
	Depth is maxint,
	Prec1 is Prec - 1,
	print(user, Val, Depth, Prec1),
	(VarNames == [] ->
		put(user, 0'\s)
	;	putl(user, ",\n"),
		$writeGoalVars(VarNames, Values)
	).

%	Ask if more solutions wanted.
$moreSolutions :-
	once
	(	repeat,
		get0(user, Action),
		( Action == -1 ->
			clearIOError(user_input),
			$close(user_input)					% Reopen it on terminal
		;	member(Action, ";\n")
		)
	),
	( Action == 0'; ->
		skip(user, 0'\n),
		fail
	).

'$:'(O, T) :-
	nl,
%	listOfVars(O, V),		BUG!
	$unique_vars((O : T), U),
%	append(V, U, U1),		BUG!
%	$call_trf(T, U1, T1),
	$call_trf(T, U, T1),
	( var(O) ->
		(	$ocall(T1), print(user, O), nl(user), fail
		;	true
		)
	; (O = (Term sorted) ->
		findall(Term, $ocall(T1), L1),
		sort(L1, L),
		(	member(E, L), print(user, E), nl(user), fail
		;	true
		)
	; (O = (Term sorted Sort) ->
		$toList(Sort, S),
		findall(Term, $ocall(T1), L1),
		sort(L1, L2),			% sort used to remove duplicates
		multiKeySort(S, Term, L2, L),
		(	member(E, L), print(user, E), nl(user), fail
		;	true
		)
	;	(	$ocall(T1), print(user, O), nl(user), fail
		;	true
		)
	))),
	nl.

'$:'(T) :-
	$unique_vars(T, U),
	$call_trf(T, U, T1),
	( $ocall(T1) ->
		putl(user, "\nYes.\n\n")
	;	putl(user, "\nNo.\n\n")
	).

% insert PLIST where G.
% delete PLIST where G.
% update ULIST in PLIST where G.
% PLIST --> p1(...), ..., pN(...)
% ULIST --> V1 to T1, ..., VM to TM

% update N to N1 in emp(E, Name, N) where E > 5, N1 is N + 1.
% emp(E, Name, N), E > 5, 

$insert(I) :-
	( I = (P where G) ->
		true
	;	(I where true) = (P where G)
	),
	listOfVars(P, PL),
	T =.. $term.PL,
	$toList(P, P1),
	$unique_vars((P where G), U),
	append(PL, U, U1),
	$call_trf(G, U1, G1),
	$insert([], T, P1, G1),
	putl(user, "Insertion completed.\n").

$delete(D) :-
	( D = (P where G) ->
		true
	;	(D where true) = (P where G)
	),
	listOfVars(P, PL),
	T =.. $term.PL,
	$toList(P, P1),
	$unique_vars((G, P), U),
	append(PL, U, U1),
	$call_trf((G, P), U1, G1),
	$delete([], T, P1, G1),
	putl(user, "Deletion completed.\n").

$update(U) :-
	( U = (S in P where G) ->
		true
	;	( (U where true) = (S in P where G) ->
			true
		;	putl(user, "Update must include 'in' clause.\n"),
			!,
			fail
		)
	),
	$toList(P, P1),
	$toList(S, S1),
	numberVars($term(S1, P1, G), 1, _),
	$lsubst(S1, P1, P2),
	varNumbers($term(P2, P1, (G, P)), $term(VP2, VP1, VG)),
	listOfVars(VP1-VP2, L),
	T =.. $term.L,
	$unique_vars(VG, U1),
	append(L, U1, U2),
	$call_trf(VG, U2, VG1),
	$update([], T, VP2, VP1, VG1),
	putl(user, "Update completed.\n").

$subst(_, Exp, Exp) :-
	const(Exp),
	!.
$subst(S, $VAR(N), T) :-
	atomic(N),
	member(to($VAR(N), T), S),
	!.
$subst(_, $VAR(N), $VAR(N)) :-
	atomic(N),
	!.
$subst(S, ExpIn, ExpOut) :-
	ExpIn =.. F.ArgIn,
	$lsubst(S, ArgIn, ArgOut),
	ExpOut =.. F.ArgOut.

?- $lsubst(_, X, Y) when X or Y.
$lsubst(_, [], []).
$lsubst(S, H.T, H1.T1) :- $subst(S, H, H1), $lsubst(S, T, T1).

$toList(X, [X]) :-
	var(X),
	!.
$toList(X, X) :-
	cons(X),
	!,
	( $list(X) ->
		true
	;	format(user_error,
			"~NInappropriate key term ~w to top-level goal.~n",
			[X]),
		fail
	).
$toList((X, Y), X.Y1) :-
	!,
	$toList(Y, Y1).
$toList(X, [X]).

$ocall(G) :-
	( prologFlag(optimizeQuery, on) ->
		$optimize(G, G1)
	;	G1 = G
	),
	$setFlag(callNumber, 0),
	$getFlag(debugging, Debugging),
	(	$setFlag(localDebug, Debugging)
	;	$setFlag(localDebug, off),
		fail
	),
	call(G1),
	(	$setFlag(localDebug, off)
	;	$setFlag(localDebug, Debugging),
		fail
	),
	waitedOn(G1, DelayedVars),
	(cons(DelayedVars) ->
		putl(user_output, "Warning: Goal floundered.\n")
	;	true
	).

restart(Goal) :-
	( getprop($mainloop, history$cnum, _)	% Test if mainloop active.
	->	putprop($mainloop, $restart, Goal),
		abort
	;	putl(user_error, "restart/1 called from outside $mainloop.\n")
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  putTokenList/1 and putTokenList/2 - (plus local predicates)

putTokenList(L) :-
	currentOutput(S),
	ot$putTokenList(S, junk.rubbish, L).		% No space after rubbish

putTokenList(S, L) :-
	prologFlag(vars, VarsFlag, on),
	(	ot$putTokenList(S, junk.rubbish, L),	% No space after rubbish
		fail
	;	$setFlag(vars, VarsFlag)
	).

?- ot$putTokenList(_, _, L) when L.	% For clause indexing
ot$putTokenList(S, P.T, []) :-
	( T = atom, ot$syAtom(P) ->
		putl(S, " .\n")
	;	putl(S, ".\n")
	).
ot$putTokenList(S, P, (C.T).List) :-
	( ot$space(P, (C.T)) ->
		put(S, 0'\s)
	),
	once ot$write(S, T, C),
	ot$putTokenList(S, (C.T), List).

ot$write(S, var, C) :-
	write(S, $VAR(C)).
ot$write(S, quoted, C) :-
	writev(S, [quoteall], C).
ot$write(S, _, C) :-
	write(S, C).

% This is the guts of it!

ot$space((,).atom, C) :-		% Only after a comma if in 'group'
	ot$group(C),			% or open
	!.
ot$space((,).atom, C.atom) :-
	ot$open(C),
	!.
ot$space((,).atom, _) :-
	!,
	fail.
ot$space(_, (,).atom) :-		% Never before a comma
	!,
	fail.
ot$space(_.number, (.).atom) :-		% If a (.) follows a number or an
	!.				% atom it could combine with.
ot$space(A.atom, (.).atom) :-
	!,
	ot$syAtom(A).
ot$space(_, (.).atom) :-
	!,
	fail.
ot$space((.).atom, _) :-		% Never after a (.)
	!,
	fail.
ot$space(P.atom, C) :-			% Separate close and 'group's
	ot$close(P),
	!,
	ot$group(C).
ot$space(P, C) :-			% Always separate 'group's
	ot$group(P),
	ot$group(C).

ot$group(_.var).
ot$group(_.string).
ot$group(_.quoted).
ot$group(_.number).
ot$group(A.atom) :-
	ot$a$Atom(A).
ot$group(A.atom) :-
	ot$syAtom(A).

ot$a$Atom(A) :-
	atomToString(A, H._),
	(0'a =< H and H =< 0'z) or (H =:= 0'$).
	
ot$syAtom(A) :-
	atomToString(A, H._),
	ot$symbol(H).
	
ot$symbol(0'+).
ot$symbol(0'-).
ot$symbol(0'*).
ot$symbol(0'/).
ot$symbol(0'~).
ot$symbol(0'<).
ot$symbol(0'=).
ot$symbol(0'>).
ot$symbol(0'`).
ot$symbol(0':).
ot$symbol(0'.).
ot$symbol(0'?).
ot$symbol(0'@).
ot$symbol(0'#).
ot$symbol(0'&).
ot$symbol(0'\\).

% ot$open('(').
ot$open('[').
ot$open('{').

ot$close(')').
ot$close(']').
ot$close('}').

% ot$special('!').
% ot$special(';').
% ot$special('|').
% ot$special('(').
% ot$special(')').
% ot$special('[').
% ot$special(']').
% ot$special('{').
% ot$special('}').	% plus (,)

ot$eof(TL) :-
	$last(TL, X),
	(	X = [].end_of_file
	;	X = end_of_file.atom
	),
	!.

ot$var([(_.var)]).
