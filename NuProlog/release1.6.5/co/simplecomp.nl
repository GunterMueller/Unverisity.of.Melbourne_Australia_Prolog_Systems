/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- simple clause compiler.

compile1Head(Head, MaxArg, Code) :-
	functor(Head, _, Arity),
	numberTopLevelVariables(Arity, Head),
	numberRestOfVariables(Arity, Head, MaxArg, NVars0),
	NVars0 < 255,
	compile1Head(1, Arity, Head, NVars0, Code, [pro]),
	true.

numberTopLevelVariables(N, Head) :-
	( N > 0 ->
		arg(N, Head, Arg),
		N1 is N - 1,
		( var(Arg) ->
			Arg = vr(N1, local)
		),
		numberTopLevelVariables(N1, Head)
	).

numberRestOfVariables(T, NV0, NV) :-
	( var(T) ->
		T = vr(_, _),
		NV = NV0
	; const(T) ->
		NV = NV0
	; T = vr(V, _) ->
		( var(V) ->
			V = NV0,
			NV is NV0 + 1
		;	NV = NV0
		)
	;	functor(T, _, Arity),
		numberRestOfVariables(Arity, T, NV0, NV)
	).

numberRestOfVariables(N, T, NV0, NV) :-
	( N > 0 ->
		arg(N, T, Arg),
		numberRestOfVariables(Arg, NV0, NV1),
		N1 is N - 1,
		numberRestOfVariables(N1, T, NV1, NV)
	;	NV = NV0
	).

compile1Head(N, Arity, Head, NVars) -->
	( { N =< Arity } ->
		{ arg(N, Head, Arg) },
		{ HV is N - 1 },
		compileHeadArg(HV, Arg, Insts),
		compileDeferredHeadArgs(Insts, NVars),
		{ N1 is N + 1 },
		compile1Head(N1, Arity, Head, NVars)
	).

compileHeadArg(HV, Arg, Insts) -->
	( { const(Arg) } ->
		[gc(temp(HV), const(Arg))],
		{ Insts = [] }
	; { Arg = vr(V, _) } ->
		( { integer(V), V =\= HV } ->
			[gval(temp(HV), temp(V))]
		),
		{ Insts = [] }
	; { $string(Arg), Arg = _._._ } ->
		compileHeadArgString(HV, Arg, Insts)
	; { cons(Arg) } ->
		[gl(temp(HV))],
		compileHeadArgs(1, 2, Arg, Insts, [])
	;	{ functor(Arg, F, Arity) },
		[gs(temp(HV), F/Arity)],
		compileHeadArgs(1, Arity, Arg, Insts, [])
	).

compileHeadArgString(HV, Arg, Insts) -->
	{ Term = string(Arg) },
	[pc(#(Tag, Label, Term), temp(255))],
	[gval(temp(HV), temp(255))],
	{ tagOfTerm(Term, Tag) } ,
	{ makeLabel(Label) } ,
	{ asserta(groundTerm(Tag, Label, Term)) } .

compileHeadArgs(N, Arity, T, Insts0, Insts) -->
	( { N =< Arity } ->
		{ arg(N, T, Arg) },
		{ Last is (N =:= Arity) },
		compileNestedHeadArg(Arg, Last, Insts0, Insts1),
		{ N1 is N + 1 },
		compileHeadArgs(N1, Arity, T, Insts1, Insts)
	;	{ Insts = Insts0 }
	).

compileNestedHeadArg(Arg, Last, Insts0, Insts) -->
	( { const(Arg) } ->
		[uc(const(Arg))],
		{ Insts = Insts0 }
	; { Arg = vr(V, I) } ->
		( { var(V) } ->
			[uvoid(1)]
		; { var(I) } ->
			{ I = V },
			[uvar(temp(V))]
		; { integer(I) } ->
			[uval(temp(V), global)]
		;	[uval(temp(V), local)]
		),
		{ Insts = Insts0 }
	; { cons(Arg), \+ $string(Arg), Last == 1 } ->
		[ul],
		compileHeadArgs(1, 2, Arg, Insts0, Insts)
	; { $struct(Arg), Last == 1 } ->
		{ functor(Arg, Pred, Arity) },
		[us(Pred/Arity)],
		compileHeadArgs(1, Arity, Arg, Insts0, Insts)
	;	[uvar(temp(HV))],
		{ Insts0 = (HV = Arg).Insts }
	).

compileDeferredHeadArgs(Insts, NVars0) -->
	{ allocateDestructuringTemps(Insts, NVars0, NVars) },
	compileDeferredHeadArgs1(Insts, NVars).

allocateDestructuringTemps([], NV, NV) :-
	NV < 255.
allocateDestructuringTemps((NV0 = _).Insts, NV0, NV) :-
	NV1 is NV0 + 1,
	allocateDestructuringTemps(Insts, NV1, NV).

compileDeferredHeadArgs1([], _) -->
	[].
compileDeferredHeadArgs1((HV = Arg).Insts, NVars) -->
	compileHeadArg(HV, Arg, NewInsts),
	compileDeferredHeadArgs(NewInsts, NVars),
	compileDeferredHeadArgs1(Insts, NVars).
