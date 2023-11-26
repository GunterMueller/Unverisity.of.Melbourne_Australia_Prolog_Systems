/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- source canonicalizer.

%	Note that variables are numbered from 1 rather than 0 and that
%	thus there are actually NVars - 1 of them.
%
%	The third argument has type
%		t -->	conj(list(u)); disj(_, _, list(t)).
%		u -->
%				head(atom/arity)
%			;	conj(list(u))
%			;	disj(_, _, list(t))
%			;	conj(list(u)) -> conj(list(u))
%			;	init(list(vr(_, _, _, _))
%			;	builtin(atom, integer, list(_))
%			;	fence
%			;	goal(atom, integer, _, list(_), (par; seq))
%			;	_ = _.

canonicalize(Head, Body, conj(CC), NVars, EnvNeeded) :-
	listOfVars(Head+Body, Vars),
	nameVars(1, NC, Vars),
	canonHead(Head, CC, CCE, NC, NH),
	canonBody(Body, conj(CCE), NH, NVars, EnvNeeded).

%	Instantiate variables in original clause.
:- nameVars(A, B, C) when C or A and B.
nameVars(N, N, []).
nameVars(N, NMax, vr(N, _, _).Vars) :-
	N1 is N + 1,
	nameVars(N1, NMax, Vars).

%	Number variables introduced during dismemberment
:- co$numberVars(A, B, C) when C or A and B.
co$numberVars(N, N, []).
co$numberVars(N, NMax, N.Vars) :-
	N1 is N + 1,
	co$numberVars(N1, NMax, Vars).

canonHead(Head, head(Pred/Arity).Gets, GetsEnd, NIn, NOut) :-
	Head =.. (Pred.Args),
	length(Args, Arity),
	getParameters(Args, 0, Gets, GetsEnd, Vars, []),
	co$numberVars(NIn, NOut, Vars).

canonBody(Body, CanonBody, NIn, NOut, EnvNeeded) :-
	flatten(Body, FlatBody),
	dismemberBody(FlatBody, CanonBody, Vars, [], EnvNeeded),
	(var(EnvNeeded) -> EnvNeeded = false),
	co$numberVars(NIn, NOut, Vars).

%	Expand various builtins into Prolog code and determine if they
%	use cut or have side-effects.
%	Probably should be replaced with a *general* macro facility.
preFlatten(Var, call(Var), _) :-
	var(Var),
	!.
preFlatten((B1, B2), (FB1, FB2), Cut) :-
	!,
	preFlatten(B1, FB1, Cut),
	preFlatten(B2, FB2, Cut).
preFlatten((C -> B1; B2), Expansion, Cut) :-
	co$simpleCondition(C),
	!,
	Expansion = (FC -> FB1; FB2),
%	Expansion = ($condition, FC, $arithCut, FB1; FB2),
	preFlatten(C, FC, Cut),
	preFlatten(B1, FB1, Cut),
	preFlatten(B2, FB2, Cut).
preFlatten((C -> B1; B2), Expansion, Cut) :-
	(	B2 = (_ -> _; _)
	;	B2 = (_ -> _)
	),
	!,
	Expansion = ($label(L), (FC, $cutd(L), FB1; D)),
	preFlatten(C, FC, Cut),
	preFlatten(B1, FB1, Cut),
	collapseCond(L, B2, D, Cut).
preFlatten((C -> B1; B2), ($label(L), (FC, $cutd(L), FB1; FB2)), Cut) :-
	!,
	preFlatten(C, FC, Cut),
	preFlatten(B1, FB1, Cut),
	preFlatten(B2, FB2, Cut).
preFlatten((B1; B2), (FB1; FB2), Cut) :-
	!,
	preFlatten(B1, FB1, Cut),
	preFlatten(B2, FB2, Cut).
preFlatten((C -> B), Expansion, Cut) :-
	co$simpleCondition(C),
	!,
	Expansion = (FC -> FB; true),
%	Expansion = ($condition, FC, $arithCut, FB; true),
	preFlatten(C, FC, Cut),
	preFlatten(B, FB, Cut).
preFlatten((C -> B), ($label(L), (FC, $cutd(L), FB; true)), Cut) :-
	!,
	preFlatten(C, FC, Cut),
	preFlatten(B, FB, Cut).
preFlatten(!, $cutd(L), cut(L)) :-
	!.
preFlatten($softCut, $softCut(L), cut(L)) :-
	!.
preFlatten(B, FB, Cut) :-
	macro(B, MB),
	!,
	preFlatten(MB, FB, Cut).
preFlatten(Goal, Goal, _).

:- collapseCond(_, X, _, _) when X.							% Really index.
collapseCond(L, (C -> B1; B2), (FC -> D1; D2), Cut) :-
%collapseCond(L, (C -> B1; B2), ($condition, FC, $arithCut, D1; D2), Cut) :-
	co$simpleCondition(C),
	!,
	preFlatten(C, FC, Cut),
	( B2 = (_ ; _) ->
		%	Continue using the current label (and thus choice-point).
		D1 = ($cutd(L), FB1),
		preFlatten(B1, FB1, Cut),
		collapseCond(L, B2, D2, Cut)
	;	preFlatten(B1, D1, Cut),
		preFlatten(B2, D2, Cut)
	).
collapseCond(_, (C -> B), (FC -> FB; true), Cut) :-
%collapseCond(_, (C -> B), ($condition, FC, $arithCut, FB; true), Cut) :-
	co$simpleCondition(C),
	!,
	preFlatten(C, FC, Cut),
	preFlatten(B, FB, Cut).
collapseCond(L, (C -> B1; B2), (FC, $cutd(L), FB1; D), Cut) :-
	!,
	preFlatten(C, FC, Cut),
	preFlatten(B1, FB1, Cut),
	collapseCond(L, B2, D, Cut).
collapseCond(L, (C -> B), (FC, $cutd(L), FB; true), Cut) :-
	!,
	preFlatten(C, FC, Cut),
	preFlatten(B, FB, Cut).
collapseCond(_, B, FB, Cut) :-
	preFlatten(B, FB, Cut).

macro(\+ B, (B -> fail ; true)).	% So that \+ atom(X) is done efficiently
%	The argument permutation is for efficiency of common case.
macro(call(P, A), $funcall(A, P)).
macro(call(P, A, B), $funcall(A, B, P)).
macro(call(P, A, B, C), $funcall(A, B, C, P)).
macro(call(P, A, B, C, D), $funcall(A, B, C, D, P)).
macro(call(P, A, B, C, D, E), $funcall(A, B, C, D, E, P)).
macro(call(P, A, B, C, D, E, F), $funcall(A, B, C, D, E, F, P)).
macro(apply(P, A), $apply(A, P)).
macro(apply(P, A, B), $apply(A, B, P)).
macro(apply(P, A, B, C), $apply(A, B, C, P)).
macro(apply(P, A, B, C, D), $apply(A, B, C, D, P)).
macro(apply(P, A, B, C, D, E), $apply(A, B, C, D, E, P)).
macro(apply(P, A, B, C, D, E, F), $apply(A, B, C, D, E, F, P)).
macro(B1 \= B2, \+ B1 = B2).
macro(format(F, A), (currentOutput(S), Expansion)) :-
	macroFormat(S, F, A, Expansion).
macro(format(S, F, A), Expansion) :-
	macroFormat(S, F, A, Expansion).
macro(X is Y, $is(Y, X)).		% Order reversed for temporary allocation.
macro(X =:= Y, $arithPred(X =:= Y)).
macro(X =\= Y, $arithPred(X =\= Y)).
macro(X < Y, $arithPred(X < Y)).
macro(X =< Y, $arithPred(X =< Y)).
macro(X > Y, $arithPred(X > Y)).
macro(X >= Y, $arithPred(X >= Y)).
macro(X and Y, $arithPred(X and Y)).
macro(X or Y, $arithPred(X or Y)).
macro($eRef(X, Y), ($eRefBuiltin(X, Z), Z = Y)).
macro($mkObj(W, X, Y, Z), ($mkObjBuiltin(W, X, Y, A), A = Z)).
macro(X @< Y, compare(<, X, Y)).
macro(@=<(X, Y), (compare(Op, X, Y), $compareLE(Op))).
macro(@>(X, Y), @<(Y, X)).
macro(@>=(X, Y), @=<(Y, X)).
macro(compare(Op, X, Y), ($compareBuiltin(A, X, Y), A = Op)).
macro($sort(K, X, Y), ($sortBuiltin(K, X, A), A = Y)).
macro(once(B), ($label(L), B, $cutd(L))).
%macro(ground(X), ($fvar(X, Y), nonvar(Y))).
macro($defined(P, A), $defined(P, A, _, _)).
macro($defined(P, A, X, Y), ($definedBuiltin(P, A, U, V), U = X, V = Y)).
macro($predicateArities(P, A), ($predicateAritiesBuiltin(P, U), U = A)).
macro($flags(X), ($flagsBuiltin(Y), Y = X)).
macro(arg(A, B, X), ($argBuiltin(A, B, Y), Y = X)).
macro($aref(A, B, X, N), ($arefBuiltin(A, B, Y, N), Y = X)).
macro($aset(A, B, C, X, N), ($asetBuiltin(A, B, C, Y, N), Y = X)).
macro($fvar(A, X), ($fvarBuiltin(A, Y), Y = X)).
macro($symbol(A, X), ($symbolBuiltin(A, Y), Y = X)).
macro($copy(A, B, M, X), ($copyBuiltin(A, B, N, Y), Y = X, N = M)).
macro($uncopy(A, N, X), ($uncopyBuiltin(A, N, Y), Y = X)).
macro($makeBMT(A, B, P, I), ($makeBMTBuiltin(A, B, N, Y), Y = I, N = P)).
macro(instance(BMT, K, X), ($instanceBuiltin(BMT, N, Y), Y = X, N = K)).
macro($proplist(Atom, Key, X), ($proplistBuiltin(Atom, Key, Y), Y = X)).
macro($proplist(Atom, X), ($proplistBuiltin(Atom, _, Y), Y = X)).
macro($is_eq(A, B, R), ($is_eqBuiltin(A, B, S), S = R)).
macro($syscall(C, U, V, W, Z), ($syscallBuiltin(C, U, V, W, A), A = Z)).
macro(fork(W, X, Y), ($forkBuiltin(A, B, C), A = W, B = X, C = Y)).
macro(fork(W, X, Y, Z), ($forkBuiltin(A, B, C, D), A = W, B = X, C = Y, D = Z)).
macro($fload(F, L, Y, Z), ($floadBuiltin(F, L, Y, A), A = Z)).
macro(exit, exit(0)).
macro(get0(X), ($get0Builtin(Y), Y = X)).
macro(get(X), ($getBuiltin(Y), Y = X)).
macro(get0(S, X), ($get0Builtin(S, Y), Y = X)).
macro(get(S, X), ($getBuiltin(S, Y), Y = X)).
macro($sprt(F, X, S), ($sprtBuiltin(F, X, T), T = S)).
macro($printNumber(F, P, X, S), ($printNumberBuiltin(F, P, X, T), T = S)).
macro(getToken(S, X, Y), ($getTokenBuiltin(S, A, B), A = X, B = Y)).
macro($tokenize(S0, X, Y, S1),
		($tokenizeBuiltin(S0, A, B, C), A = X, B = Y, C = S1)).
macro(putl(S), (currentOutput(O), putl(O, S))).
macro(nl, put(10)).
macro(nl(S), put(S, 10)).
macro($open(X, Y, Z), ($openBuiltin(X, Y, A), A = Z)).
macro($close(X), $close(X, _)).
macro($close(X, Y), ($closeBuiltin(X, A), A = Y)).
macro(
	$currentStream(W, X, Y, Z),
	($getStream(W, $stream(X, Y, Z, _, _, _, _)), Z \== [])).
macro($getStream(W, X), ($getStreamBuiltin(W, A), A = X)).
macro(currentInput(X), ($currentInputBuiltin(Y), Y = X)).
macro(currentOutput(X), ($currentOutputBuiltin(Y), Y = X)).
macro(ttyget0(X), get0(user, X)).
macro(ttyget(X), get(user, X)).
macro(ttyskip(X), skip(user, X)).
macro(ttyput(X), put(user, X)).
macro(ttynl, nl(user)).
macro(ttytab(N), tab(user, N)).
macro(ttyflush, flushOutput(user)).
macro($pstot(X, Y), ($pstotBuiltin(X, A), A = Y)).
macro($simc_hash(W, X, Y, Z), ($simc_hashBuiltin(W, X, Y, A), A = Z)).
macro($simc_query(W, X, Y, Z), ($simc_queryBuiltin(W, X, Y, A), A = Z)).
macro($simc_next(X, Y), ($simc_nextBuiltin(X, A), A = Y)).
macro($dsimc_open(W, X, Y, Z), ($dsimc_openBuiltin(W, X, Y, A), A = Z)).
macro($dsimc_cv(X, Y, Z), ($dsimc_cvBuiltin(X, Y, A), A = Z)).
macro($dsimc_sfbquery(V, W, X, Y, Z),
		($dsimc_sfbqueryBuiltin(V, W, X, Y, A), A = Z)).
macro($dsimc_query(W, X, Y, Z), ($dsimc_queryBuiltin(W, X, Y, A), A = Z)).
macro($sql_query(X, Y, Z), ($sql_queryBuiltin(X, Y, A), A = Z)).
macro($sql_next(X, Y), ($sql_nextBuiltin(X, A), A = Y)).

co$simpleCondition(X) :-
	var(X),
	!,
	fail.
co$simpleCondition(X) :-
	functor(X, Type, 1),
	(	$type(Type, _)
	;	$ctype(Type, _)
	),
	!.
co$simpleCondition(ground(_)).
co$simpleCondition(_ == _).
co$simpleCondition(_ \== _).
co$simpleCondition(_ < _).
co$simpleCondition(_ =< _).
co$simpleCondition(_ > _).
co$simpleCondition(_ >= _).
co$simpleCondition(_ =:= _).
co$simpleCondition(_ =\= _).
co$simpleCondition(_ and _).
co$simpleCondition(_ or _).
co$simpleCondition(\+(X)) :-
	co$simpleCondition(X).
co$simpleCondition((X, Y)) :-
	co$simpleCondition(X),
	co$simpleCondition(Y).
co$simpleCondition((X; Y)) :-
	co$simpleCondition(X),
	co$simpleCondition(Y).

%	Pre-process format/2 strings.
macroFormat(Stream, Format, Args, Expansion) :-
	( atom(Format) ->
		name(Format, FormatString),
		macroFormat1(Stream, FormatString, Args, Expansion)
	; $string(Format) ->
		macroFormat1(Stream, Format, Args, Expansion)
	;	Expansion = $format(Stream, Format, Args)
	).

macroFormat1(Stream, Format, Args, Expansion) :-
	( member(0'~, Format, _.R0.R), macroFormat2(R0, R, C), member(C, "N|+t") ->
		Expansion = $format(Stream, Format, Args)
	; Args == [], macroFormatToString(Format, MFormat) ->
		Expansion = putl(Stream, MFormat)
	;	Expansion = $quickFormat(Stream, Format, Args)
%	; member(0'~, Format, R0.R), \+ member(R, "n~") ->
%		Expansion = $quickFormat(Stream, Format, Args)
%	;	Expansion = $format(Stream, Format, Args)
	).

macroFormat2(0'', _.R._, R).
macroFormat2(0'*, R._, R).
macroFormat2(D, R, C) :-
	macroFormat3(D, R, C).

macroFormat3(D, R, C) :-
	( 0'0 =< D and D =< 0'9 ->
		R = R0.R1,
		macroFormat3(R0, R1, C)
	;	D = C
	).

macroFormatToString([], []).
macroFormatToString(X1.XT0, Y) :-
	( X1 == 0'~ ->
		XT0 = X2.XT,
		( X2 == 0'~ ->
			Y = 0'~.YT
		; X2 == 0'n ->
			Y = 0'\n.YT
		;	fail
		)
	;	Y = X1.YT,
		XT0 = XT
	),
	macroFormatToString(XT, YT).

%	Flatten a clause body into an and-or tree.
flatten((B1; B2), conj([disj(_, _, FB)])) :-
	!,
	flattenDisj(B1, FB, FB1),
	flattenDisj(B2, FB1, []).
flatten(B, conj(FB)) :-
	flattenConj(B, FB, []).

flattenConj((B1, B2), F0, F) :-
	!,
	flattenConj(B1, F0, F1),
	flattenConj(B2, F1, F).
flattenConj((B1; B2), F0, F) :-
	!,
	F0 = disj(_, _, FB).F,
	flattenDisj(B1, FB, FB1),
	flattenDisj(B2, FB1, []).
flattenConj(Goal, thing(Goal).F, F).

flattenDisj((B1; B2), F0, F) :-
	!,
	flattenDisj(B1, F0, F1),
	flattenDisj(B2, F1, F).
flattenDisj((B1 -> B2), F0, F) :-
	!,
	F0 = (conj(FB1) -> conj(FB2)).F,
	flattenConj(B1, FB1, []),
	flattenConj(B2, FB2, []).
flattenDisj(X, conj(FX).F, F) :-
	flattenConj(X, FX, []).

%	Break the body of a clause up into single operations.
%	Leaves lots of variables to be filled in by later passes.
%
%	Second argument has type
%		t --> conj(list(u)); disj(_, _, list(u)).
%		u -->
%				conj(list(u))
%			;	disj(_, _, list(u))
%			;	init(list(vr(_, _, _, _))
%			;	builtin(atom, integer, list(_))
%			;	fence
%			;	goal(atom, integer, _, list(_), (par; seq))
%			;	_ = _.

dismemberBody(conj(B), conj(DB), V0, V, EnvNeeded) :-
	dismemberL(B, DB, [], V0, V, EnvNeeded).

:- dismemberL(X, _, _, _, _, _) when X.
dismemberL([], L, L, V, V, _).
dismemberL(T.L, D, DE, V, VE, EnvNeeded) :-
	dismemberTerm(T, D, DE1, V, VE1, EnvNeeded),
	dismemberL(L, DE1, DE, VE1, VE, EnvNeeded).

dismemberTerm(thing(T1 = T2), D0, D, V0, V, _) :-
	!,
	dismemberEquality(T1, T2, D0, D, V0, V).
dismemberTerm(thing(Goal), D0, D, V0, V, _) :-
	builtin(Goal),
	!,
	D0 = builtin(Pred, Arity, EArgs).D1,
	V0 = V,
	Goal =.. (Pred.Args),
	length(Args, Arity),
	co$expandTermL(Args, EArgs),
	( fence(Goal) ->
		D1 = fence.D
	;	D1 = D
	).
dismemberTerm(thing(Goal), D0, D, V0, V, true) :-
	Goal =.. Pred.Args,
	(\+ atom(Pred) ->
		throwCompilerError(	
			format(user_error, "~NError: ~w not a valid goal.~n", [Goal]))
	),
	length(Args, Arity),
	putParameters(Args, P, D0, goal(Pred, Arity, _, P, seq).fence.D, V0, V).
dismemberTerm(conj(X), conj(DX).D, D, V0, V, EnvNeeded) :-
	dismemberL(X, DX, [], V0, V, EnvNeeded).
dismemberTerm(disj(NSaved, NTemps, X),
		init(_).disj(NSaved, NTemps, DX).D, D,
		V0, V, EnvNeeded) :-
	dismemberL(X, DX, [], V0, V, EnvNeeded).
dismemberTerm((conj(B1) -> conj(B2)), (conj(DB1) -> conj(DB2)).D, D,
		V0, V, EnvNeeded) :-
	dismemberL(B1, DB1, [], V0, V1, EnvNeeded),
	dismemberL(B2, DB2, [], V1, V, EnvNeeded).

%	Dismember an equality, leaving terms of the form
%	Var = list/struct tree.
%	Deeply nested terms are broken up in prefix order.
%	Because of this, X = f(g(h)) will sometimes be coded as
%	PS/UVAR/GS/UC rather than PS/UC/PS/UVAR.  This should be rare
%	enough not to matter.
dismemberEquality(vr(VN1, VR1, VT1), vr(VN2, VR2, VT2), D0, D, V0, V) :-
	!,
	V0 = V,
	( VN1 == VN2 ->
		D0 = builtin(caramel, 1, [vr(VN1, VR1, VT1, _)]).D
	;	D0 = (vr(VN1, VR1, VT1, _) = vr(VN2, VR2, VT2, _)).D
	).
dismemberEquality(T1, vr(VN, VR, VT),
		(vr(VN, VR, VT, _) = ET1).D0, D, V0, V) :-
	!,
	expandEquality(T1, ET1, D0, D, V0, V).
dismemberEquality(vr(VN, VR, VT), T2,
		(vr(VN, VR, VT, _) = ET2).D0, D, V0, V) :-
	!,
	expandEquality(T2, ET2, D0, D, V0, V).
dismemberEquality(T, T, builtin(true, 0, []).D, D, V, V) :-
	!.
dismemberEquality(T1, T2, D0, D, V0, V) :-
	atomic(T1),
	atomic(T2),
	!,
	D0 = builtin(fail, 0, []).D,
	V0 = V,
	format(user_error, "~NWarning: ~w not unifiable~n", [T1 = T2]).
dismemberEquality(T1, T2, D0, D, V0, V) :-
	( functor(T1, Head, N), functor(T2, Head, N) ->
		T1 =.. (Head.Args1), T2 =.. (Head.Args2),
		dismemberEqualityL(Args1, Args2, D0, D, V0, V)
	;	format(user_error, "~NWarning: ~w not unifiable~n", [T1 = T2]),
		D0 = builtin(fail, 0, []).D,
		V0 = V
	).

:- dismemberEqualityL(X, Y, _, _, _, _) when X or Y.
dismemberEqualityL([], [], DE, DE, VE, VE).
dismemberEqualityL(Term1.Terms1, Term2.Terms2, D, DE, V, VE) :-
	dismemberEquality(Term1, Term2, D, DE1, V, VE1),
	dismemberEqualityL(Terms1, Terms2, DE1, DE, VE1, VE).

%	Put parameters for a call and collect all the new variables.
%	Put the structured arguments first to maximize the number
%	of temporary variables.
%
%	This is optimal if there are no temporaries defined before the goal,
%	and is usually optimal at other times.
putParameters(Args, P, D0, D, V0, V) :-
	putParameters(Args, 0, P, SimpleP, StructP),
	putParameters1(StructP, D0, D1, V0, V1),
	putParameters1(SimpleP, D1, D, V1, V).

:- putParameters(X, _, _, _, _) when X.
putParameters([], _, [], [], []).
putParameters(Arg.Args, N, P0, SimpleP0, StructP0) :-
	P0 = vr(t(Instance), N, temp, val(safe)).P,
	Pair = pair(Arg, vr(t(Instance), N, temp, var)),
	nextInstance(Instance),
	( simpleTerm(Arg) ->
		SimpleP0 = Pair.SimpleP,
		StructP0 = StructP
	;	StructP0 = Pair.StructP,
		SimpleP0 = SimpleP
	),
	N1 is N + 1,
	putParameters(Args, N1, P, SimpleP, StructP).

:- putParameters1(X, _, _, _, _) when X.
putParameters1([], D, D, V, V).
putParameters1(pair(Arg, Param).P, D0, D, V0, V) :-
	putTerm(Arg, Param, D0, D1, V0, V1),
	putParameters1(P, D1, D, V1, V).

:- putTerm(X, _, _, _, _, _) when X.
putTerm(vr(VN, VR, VT), Var, D0, D, V0, V) :-
	!,
	D0 = (vr(VN, VR, VT, _) = Var).D,
	V0 = V.
putTerm(AtomicTerm, Var, (const(AtomicTerm) = Var).DE, DE, VE, VE) :-
	atomic(AtomicTerm),
	!.
putTerm(GroundTerm, Var, D0, D, V0, V) :-
	ground(GroundTerm),
	!,
	D0 = (ground(Tag, Label, GroundTerm) = Var).D,
	V0 = V,
	co$expandTerm(GroundTerm, Term),
	tagOfTerm(Term, Tag),
	makeLabel(Label),
	asserta(groundTerm(Tag, Label, Term)).
putTerm(Arg.Args, Var, D, DE, V, VE) :-
	!,
	putTermArgs(
		[Arg, Args], ImmediateArgs,
		D, (list(ImmediateArgs) = Var).DE,
		V, VE).
putTerm(Term, Var, D, DE, V, VE) :-
	term(Term),
	Term =.. (Functor.Args),
	putTermArgs(
		Args, ImmediateArgs,
		D, (struct(Functor.ImmediateArgs) = Var).DE,
		V, VE).

:- putTermArgs(X, _, _, _, _, _) when X.
putTermArgs([], [], D, D, V, V).
putTermArgs(vr(VN, VR, VT).Args, vr(VN, VR, VT, _).IArgs, D0, D, V0, V) :-
	!,
	putTermArgs(Args, IArgs, D0, D, V0, V).
putTermArgs(Arg.Args, const(Arg).IArgs, D0, D, V0, V) :-
	atomic(Arg),
	!,
	putTermArgs(Args, IArgs, D0, D, V0, V).
putTermArgs([Arg], [list(IArgs)], D0, D, V0, V) :-
	Arg = Arg1.Arg2,
	\+ $string(Arg),
	!,
	putTermArgs([Arg1, Arg2], IArgs, D0, D, V0, V).
putTermArgs([Arg], [struct(Pred.IArgs)], D0, D, V0, V) :-
	$struct(Arg),
	!,
	Arg =.. Pred.Args,
	putTermArgs(Args, IArgs, D0, D, V0, V).
putTermArgs(Arg.Args, vr(VN, VR, VT, _).IArgs, D0, D, VN.V0, V) :-
	putTerm(Arg, vr(VN, VR, VT, _), D0, D1, V0, V1),
	putTermArgs(Args, IArgs, D1, D, V1, V).

:- getParameters(X, _, _, _, _, _) when X.
getParameters([], _, D, D, V, V).
getParameters(Arg.Args, N, D, DE, V, VE) :-
	nextInstance(Instance),
	getTerm(Arg, vr(t(Instance), N, temp, val(safe)), D, DE1, V, VE1),
	N1 is N + 1,
	getParameters(Args, N1, DE1, DE, VE1, VE).

:- getTerm(X, _, _, _, _, _) when X.
getTerm(vr(VN, VR, VT), Var, D0, D, V0, V) :-
	!,
	D0 = (Var = vr(VN, VR, VT, _)).D,
	V0 = V.
getTerm(AtomicTerm, Var, D0, D, V0, V) :-
	atomic(AtomicTerm),
	!,
	D0 = (Var = const(AtomicTerm)).D,
	V0 = V.
getTerm(GroundTerm, Var, D0, D, V0, V) :-
	$string(GroundTerm),
	GroundTerm = _._._,
	!,
	D0 = (Var = ground(Tag, Label, Term)).D,
	V0 = V,
	co$expandTerm(GroundTerm, Term),
	tagOfTerm(Term, Tag),
	makeLabel(Label),
	asserta(groundTerm(Tag, Label, Term)).
getTerm(Arg.Args, Var, D0, D, V0, V) :-
	!,
	D0 = (Var = list(ImmediateArgs)).D1,
	getTermArgs([Arg, Args], ImmediateArgs, D1, D, V0, V).
getTerm(Term, Var, (Var = struct(Functor.ImmediateArgs)).D0, D, V0, V) :-
	compound(Term),
	Term =.. (Functor.Args),
	getTermArgs(Args, ImmediateArgs, D0, D, V0, V).

:- getTermArgs(X, _, _, _, _, _) when X.
getTermArgs([], [], D, D, V, V).
getTermArgs(vr(VN, VR, VT).Args, vr(VN, VR, VT, _).IArgs, D0, D, V0, V) :-
	!,
	getTermArgs(Args, IArgs, D0, D, V0, V).
getTermArgs(Arg.Args, const(Arg).IArgs, D0, D, V0, V) :-
	atomic(Arg),
	!,
	getTermArgs(Args, IArgs, D0, D, V0, V).
% here's where to fiddle tail-recursive gets
getTermArgs([Arg], [list(IArgs)], D0, D, V0, V) :-
	Arg = Arg1.Arg2,
	\+ $string(Arg),
	!,
	getTermArgs([Arg1, Arg2], IArgs, D0, D, V0, V).
getTermArgs([Arg], [struct(Pred.IArgs)], D0, D, V0, V) :-
	$struct(Arg),
	!,
	Arg =.. Pred.Args,
	getTermArgs(Args, IArgs, D0, D, V0, V).
getTermArgs(Arg.Args, vr(VN, VR, VT, var).IArgs, D0, D, VN.V0, V) :-
	getTerm(Arg, vr(VN, VR, VT, val(safe)), D0, D1, V0, V1),
	getTermArgs(Args, IArgs, D1, D, V1, V).

%	Express a term as a struct/list tree, converting vr/3 terms
%	to vr/4.
:- co$expandTerm(X, Y) when X or Y.
co$expandTerm(Atomic, const(Atomic)) :-
	atomic(Atomic),
	!.
co$expandTerm(vr(VN, VR, VT), ET) :-
	!,
	ET = vr(VN, VR, VT, _).
co$expandTerm(String, string(String)) :-
	cons(String),
	ground(String),
	isPrintL(String),
	!.
co$expandTerm(Arg.Args, list([EArg, EArgs])) :-
	!,
	co$expandTerm(Arg, EArg),
	co$expandTerm(Args, EArgs).
co$expandTerm(Term, struct(Functor.EArgs)) :-
	term(Term),
	Term =.. (Functor.Args),
	co$expandTermL(Args, EArgs).

:- co$expandTermL(X, Y) when X or Y.
co$expandTermL([], []).
co$expandTermL(T.TT, ET.ETT) :-
	co$expandTerm(T, ET),
	co$expandTermL(TT, ETT).

%	Expand the argument of an (assignment) equality goal into
%	sub-goals and collect the intermediate variables.
:- expandEquality(X, Y, _, _, _, _) when X or Y.
expandEquality(Atomic, const(Atomic), D0, D, V0, V) :-
	atomic(Atomic),
	!,
	D0 = D,
	V0 = V.
expandEquality(vr(VN, VR, VT), EE, D0, D, V0, V) :-
	!,
	EE = vr(VN, VR, VT, _),
	D0 = D,
	V0 = V.
expandEquality(Arg.Args, list(EArgs), D0, D, V0, V) :-
	!,
	expandEqualityL([Arg, Args], EArgs, D0, D, V0, V).
expandEquality(Term, struct(Functor.EArgs), D0, D, V0, V) :-
	term(Term),
	Term =.. (Functor.Args),
	expandEqualityL(Args, EArgs, D0, D, V0, V).

:- expandEqualityL(X, Y, _, _, _, _) when X or Y.
expandEqualityL([], [], DE, DE, VE, VE).
expandEqualityL(Atomic.TT, const(Atomic).ETT, D0, D, V0, V) :-
	atomic(Atomic),
	!,
	expandEqualityL(TT, ETT, D0, D, V0, V).
expandEqualityL(vr(VN, VR, VT).TT, vr(VN, VR, VT, _).ETT, D0, D, V0, V) :-
	!,
	expandEqualityL(TT, ETT, D0, D, V0, V).
expandEqualityL([Arg], [list(IArgs)], D0, D, V0, V) :-
	Arg = Arg1.Arg2,
	\+ $string(Arg),
	!,
	expandEqualityL([Arg1, Arg2], IArgs, D0, D, V0, V).
expandEqualityL([Arg], [struct(Pred.IArgs)], D0, D, V0, V) :-
	$struct(Arg),
	!,
	Arg =.. Pred.Args,
	expandEqualityL(Args, IArgs, D0, D, V0, V).
expandEqualityL(T.TT, vr(VN, VR, VT, _).ETT,
		(vr(VN, VR, VT, _) = ET).D, DE, VN.V, VE) :-
	expandEquality(T, ET, D, DE1, V, VE1),
	expandEqualityL(TT, ETT, DE1, DE, VE1, VE).

simpleTerm(X) :-
	( atomic(X) ->
		true
	;	X = vr(_, _, _)
	).

nextInstance(L) :-
	makeLabel(L).
