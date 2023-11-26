/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- variable classifier.

%	Make a conj/disj/-> tree of all vr/4, fence/0 and init/1 occurrences
%	in a clause.
%	Very non-logical.
varsOfClause(conj(C), conj(Vars)) :-
	varsOfTermL(C, Vars, []).
varsOfClause(disj(NSaved, NTemps, C), disj(NSaved, NTemps, Vars)) :-
	varsOfTermL(C, Vars, []).

:- varsOfTerm(X, _, _) when X.
varsOfTerm(Var, V0, V) :-
	Var = vr(VN, _, _, _),
	( integer(VN) ->
		V0 = Var.V
	;	V0 = V
	).
varsOfTerm(head(_), V, V).
varsOfTerm(fence, fence.V, V).
varsOfTerm(init(Vars), init(Vars).V, V).
varsOfTerm(T1 = T2, V0, V) :-
	varsOfTerm(T1, V0, V1),
	varsOfTerm(T2, V1, V).
varsOfTerm(builtin(_, _, SubTerms), V0, V) :-
	varsOfTermL(SubTerms, V0, V).
varsOfTerm(struct(_.SubTerms), V0, V) :-
	varsOfTermL(SubTerms, V0, V).
varsOfTerm(list(SubTerms), V0, V) :-
	varsOfTermL(SubTerms, V0, V).
varsOfTerm(ground(_, _, _), V, V).
varsOfTerm(string(_), V, V).
varsOfTerm(const(_), V, V).
varsOfTerm(goal(_, _, _, _, _), V, V).		% All the vars are t(_).
varsOfTerm(conj(C), conj(Vars).V, V) :-
	varsOfTermL(C, Vars, []).
varsOfTerm(disj(NSaved, NTemps, C), disj(NSaved, NTemps, Vars).V, V) :-
	varsOfTermL(C, Vars, []).
varsOfTerm((conj(C1) -> conj(C2)), (conj(Vars1) -> conj(Vars2)).V, V) :-
	varsOfTermL(C1, Vars1, []),
	varsOfTermL(C2, Vars2, []).

:- varsOfTermL(X, _, _) when X.
varsOfTermL([], VE, VE).
varsOfTermL(C.CT, V, VE) :-
	varsOfTerm(C, V, VE1),
	varsOfTermL(CT, VE1, VE).

varListInsert(X, Vars0, Vars) :-
	X = vr(VN, _, _, _),
	( occurs(VN, Vars0) ->
		Vars = Vars0
	;	Vars = X.Vars0
	).

:- varListAppend(X, _, _) when X.
varListAppend([], Y, Y).
varListAppend(V.X, Y, Z) :-
	varListAppend(X, Y, Z1),
	varListInsert(V, Z1, Z).

numberListInsert(N, Vars0, Vars) :-
	( occurs(N, Vars0) ->
		Vars = Vars0
	;	Vars = N.Vars0
	).

:- numberListAppend(X, _, _) when X.
numberListAppend([], Y, Y).
numberListAppend(V.X, Y, Z) :-
	numberListAppend(X, Y, Z1),
	numberListInsert(V, Z1, Z).

:- varListToNumberList(X, Y) when X or Y.
varListToNumberList([], []).
varListToNumberList(var(N, _, _, _).XT, N.YT) :-
	varListToNumberList(XT, YT).

%	Instantiate the init(_) terms.
%
%	Most of the task of determining initializers, permanent variables,
%	and variable instance annotation could be done in the one pass.
%
%	Note that
%		(p, (q(T); true); true), q(T)
%	will produce one init/1 term for each disjunction.  The second
%	one is thrown away when code is emited for it.
addInitializers(conj(Clause)) :-
	addInitializersL(Clause, [], _, []).

:- addInitializersL([], _, _, _) when ever.
:- addInitializersL(X._, _, _, _) when X.
addInitializersL([], Vars, Vars, _).
addInitializersL(Var.Clauses, Vars0, Vars, RestOfClause) :-
	Var = vr(_, _, _, _),
	varListInsert(Var, Vars0, Vars1),
	addInitializersL(Clauses, Vars1, Vars, RestOfClause).
addInitializersL(fence.Clauses, Vars0, Vars, RestOfClause) :-
	addInitializersL(Clauses, Vars0, Vars, RestOfClause).
addInitializersL(
		init(Inits).disj(_, _, Disjuncts).Clauses,
		Vars0, Vars, RestOfClause) :-
	addInitializersD(Disjuncts, Inits, Vars0, Vars1, Clauses+RestOfClause),
	addInitializersL(Clauses, Vars1, Vars, RestOfClause).
addInitializersL(init(Inits).Clauses, Vars0, Vars, RestOfClause) :-
	Clauses ~= disj(_, _, _)._,
	addInitializersL(Clauses, Vars0, Vars, RestOfClause).

addInitializersD(Disjuncts, Inits, Vars0, Vars, RestOfClause) :-
	addInitializersDL(Disjuncts, Vars0, VarLists, RestOfClause),
	determineInitializers(VarLists, Vars0, Vars, RestOfClause, Inits).

:- addInitializersDL([], _, _, _) when ever.
:- addInitializersDL(X._, _, _, _) when X.
addInitializersDL([], _, [], _).
addInitializersDL(
		conj(Clause).Disjuncts,
		Vars0,
		Vars.VarLists,
		RestOfClause) :-
	addInitializersL(Clause, Vars0, Vars, RestOfClause),
	addInitializersDL(Disjuncts, Vars0, VarLists, RestOfClause).
addInitializersDL(
		(conj(C1) -> conj(C2)).Disjuncts,
		Vars0,
		Vars.VarLists,
		RestOfClause) :-
	append(C1, C2, C),
	addInitializersL(C, Vars0, Vars, RestOfClause),
	addInitializersDL(Disjuncts, Vars0, VarLists, RestOfClause).

determineInitializers(VarLists, VarsIn, VarsOut, RestOfClause, Inits) :-
	newVarLists(VarsIn, VarLists, NewVarLists),
	newVarsOut(NewVarLists, NewVarsOut),
	append(NewVarsOut, VarsIn, VarsOut),
	determineInitializers2(NewVarLists, VarsIn, VarsOut, RestOfClause, Inits).

:- determineInitializers2(_, _, X, _, _) when X.
determineInitializers2(_, _, [], _, []).
determineInitializers2(
		VarLists, VarsIn,
		vr(VN, VR, VT, _).VarsOut, RestOfClause,
		vr(VN, VR, VT, _/*var*/).Inits) :-
	occurs(VN, RestOfClause),
	\+ occurs(VN, VarsIn),
	member(VarList, VarLists),
	\+ occurs(VN, VarList),
	!,
	determineInitializers2(VarLists, VarsIn, VarsOut, RestOfClause, Inits).
determineInitializers2(VarLists, VarsIn, _.VarsOut, RestOfClause, Inits) :-
	determineInitializers2(VarLists, VarsIn, VarsOut, RestOfClause, Inits).

%	Prune already known vars from each element of a list of varLists.
:- newVarLists(_, X, Y) when X or Y.
newVarLists(_, [], []).
newVarLists(VarsIn, VarsOut.VarLists, NewVarsOut.NewVarLists) :-
	append(NewVarsOut, VarsIn, VarsOut),
	newVarLists(VarsIn, VarLists, NewVarLists).

:- newVarsOut(X, _) when X.
newVarsOut([], []).
newVarsOut(Vars.VarLists, VarsOut) :-
	newVarsOut(VarLists, VarsOut1),
	varListAppend(Vars, VarsOut1, VarsOut).

%	Determine variable types.
detVars(Clause) :-
	detPermVars(Clause, [], [], Vars1, Vars2),
	detTempVarsL(Vars1),
	detTempVarsL(Vars2).

detPermVars(
		conj(Clauses),
		Vars0, PermVars0,
		Vars, PermVars) :-
	detPermVarsL(Clauses, Vars0, PermVars0, Vars, PermVars).

:- detPermVarsL([], _, _, _, _) when ever.
:- detPermVarsL(X._, _, _, _, _) when X.
detPermVarsL([], Vars, PermVars, Vars, PermVars).
detPermVarsL(Var.Clauses, Vars0, PermVars0, Vars, PermVars) :-
	Var = vr(VN, _, VT, _),
	(occurs(VN, PermVars0)
	->	VT = perm,
		detPermVarsL(Clauses, Vars0, PermVars0, Vars, PermVars)
	;	varListInsert(Var, Vars0, Vars1),
		detPermVarsL(Clauses, Vars1, PermVars0, Vars, PermVars)
	).
detPermVarsL(fence.Clauses, Vars0, PermVars0, Vars, PermVars) :-
	varListAppend(Vars0, PermVars0, PermVars1),
	detPermVarsL(Clauses, [], PermVars1, Vars, PermVars).
detPermVarsL(	
		init(Inits).disj(_, _, Disjuncts).Clauses,
		Vars0, PermVars0,
		Vars, PermVars) :-
	varListAppend(Inits, Vars0, Vars0D),
	detPermVarsD(Disjuncts, Vars0D, PermVars0, VarsD, PermVarsD),
	detPermVarsL(Clauses, VarsD, PermVarsD, Vars, PermVars).

:- detPermVarsD([], _, _, _, _) when ever.
:- detPermVarsD(X._, _, _, _, _) when X.
detPermVarsD([], Vars, PermVars, Vars, PermVars).
detPermVarsD(
		conj(Clauses).Disjuncts,
		Vars0, PermVars0, Vars, PermVars) :-
	detPermVarsL(Clauses, Vars0, PermVars0, VarsOut1, PermVarsOut1),
	detPermVarsD(Disjuncts, Vars0, PermVars0, VarsOut2, PermVarsOut2),
	varListAppend(VarsOut1, VarsOut2, Vars),
	varListAppend(PermVarsOut1, PermVarsOut2, PermVars).
detPermVarsD(
		(conj(C1) -> conj(C2)).Disjuncts,
		Vars0, PermVars0, Vars, PermVars) :-
	append(C1, C2, C),
	detPermVarsL(C, Vars0, PermVars0, VarsOut1, PermVarsOut1),
	detPermVarsD(Disjuncts, Vars0, PermVars0, VarsOut2, PermVarsOut2),
	varListAppend(VarsOut1, VarsOut2, Vars),
	varListAppend(PermVarsOut1, PermVarsOut2, PermVars).

:- detTempVarsL(X) when X.
detTempVarsL([]).
detTempVarsL(vr(_, _, VT, _).Vars) :-
	( var(VT) ->
		VT = temp
	),
	detTempVarsL(Vars).

varInstances(conj(Clause)) :-
	varInstances(Clause, [], _, []).

:- varInstances([], _, _, _) when ever.
:- varInstances(X._, _, _, _) when X.
varInstances([], Vars, Vars, _).
varInstances(Var.Clauses, VarsIn, VarsOut, RestOfClause) :-
	Var = vr(VN, _, _, VI),
	( occurs(VN, VarsIn) ->
		VI = val(_),
		varInstances(Clauses, VarsIn, VarsOut, RestOfClause)
	; occurs(VN, Clauses+RestOfClause) ->
		VI = var,
		varInstances(Clauses, VN.VarsIn, VarsOut, RestOfClause)
	;	VI = uniq,
		varInstances(Clauses, VarsIn, VarsOut, RestOfClause)
	).
varInstances(fence.Clauses, VarsIn, VarsOut, RestOfClause) :-
	varInstances(Clauses, VarsIn, VarsOut, RestOfClause).
varInstances(init(Inits).Clauses, VarsIn, VarsOut, RestOfClause) :-
	append(Inits, Clauses, Clauses1),
	varInstances(Clauses1, VarsIn, VarsOut, RestOfClause).
varInstances(disj(_, _, Disjuncts).Clauses, VarsIn, VarsOut, RestOfClause) :-
	varInstancesD(Disjuncts, VarsIn, VarsOut1, Clauses+RestOfClause),
	varInstances(Clauses, VarsOut1, VarsOut, RestOfClause).

:- varInstancesD([], _, _, _) when ever.
:- varInstancesD(X._, _, _, _) when X.
varInstancesD([], Vars, Vars, _).
varInstancesD(conj(Clauses).Disjuncts, Vars0, Vars, RestOfClause) :-
	varInstances(Clauses, Vars0, VarsOut1, RestOfClause),
	varInstancesD(Disjuncts, Vars0, VarsOut2, RestOfClause),
	append(NewVars, Vars0, VarsOut1),
	numberListAppend(NewVars, VarsOut2, Vars).
varInstancesD((conj(C1) -> conj(C2)).Disjuncts, Vars0, Vars, RestOfClause) :-
	append(C1, C2, C),
	varInstances(C, Vars0, VarsOut1, RestOfClause),
	varInstancesD(Disjuncts, Vars0, VarsOut2, RestOfClause),
	append(NewVars, Vars0, VarsOut1),
	numberListAppend(NewVars, VarsOut2, Vars).

%	Mark local variables.
%
%	The algorithm is conservative about the variables that are
%	initialized in disjunctions.  If any initializer is local then
%	all instances of the variable are treated as local.
%
%	It's questionable whether this is worth the effort in a bytecode
%	interpreter.
local(Code, NVars) :-
	functor(Locals, local, NVars),
	local2(Code, Locals),
	markGlobals(NVars, Locals).

markGlobals(I, Locals) :-
	( I > 0 ->
		arg(I, Locals, X),
		( var(X) ->
			X = global
		),
		I1 is I - 1,
		markGlobals(I1, Locals)
	).

local2(conj(C), Locals) :-
	localL(C, Locals).
local2(disj(_, _, C), Locals) :-
	localL(C, Locals).
local2((conj(C1) -> conj(C2)), Locals) :-
	localL(C1, Locals),
	localL(C2, Locals).
local2(init(Vars), Locals) :-
	localInits(Vars, Locals).
local2(T1 = T2, Locals) :-
	localTerm(T1, T2, Locals),
	localTerm(T2, T1, Locals).
local2(head(_), _).
local2(fence, _).
local2(builtin(F, A, Args), Locals) :-
	( complexBuiltin(F, A, Modes) ->
		localBuiltins(Modes, Args, Locals)
	).
local2(goal(_, _, _, _, _), _).

:- localL(X, _) when X.
localL([], _).
localL(C.CT, Locals) :-
	local2(C, Locals),
	localL(CT, Locals).

:- localInits(X, _) when X.
localInits([], _).
localInits(vr(VN, _, VT, _).Vars, Locals) :-
	( VT == perm ->
		arg(VN, Locals, local)
	),
	localInits(Vars, Locals).

%	Local variables in a term at top level.
localTerm(const(T1), _, _) :-
	const(T1).
localTerm(vr(VN1, _, VT1, VI1), T2, Locals) :-
	( integer(VN1) ->
		( VI1 = var, T2 = vr(VN2, _, VT2, VI2) ->
			( integer(VN2) ->
				( VI2 = val(L2) ->
					arg(VN1, Locals, Local),
					arg(VN2, Locals, Local),
					( VT2 == temp ->
						L2 = Local
					)
				; VT1 == temp, VT2 == temp ->
					true
				;	arg(VN1, Locals, local)
				)
			; VI2 = val(_) ->			% Input arguments are possible locals.
				arg(VN1, Locals, local)
			; VT1 == perm, VI2 == var ->	% So are some outputs.
				arg(VN1, Locals, local)
			)
		)
	).
localTerm(struct(_.Args), _, Locals) :-
	localArgs(Args, Locals).
localTerm(list(Args), _, Locals) :-
	localArgs(Args, Locals).
localTerm(ground(_, _, _), _, _).
localTerm(string(_), _, _).

%	Local variables in a term not at top level.
localTerm2(struct(S), Locals):-
	!,
	S = _.Args,
	localArgs(Args, Locals).
localTerm2(list(Args), Locals) :-
	!,
	localArgs(Args, Locals).
localTerm2(vr(VN, _, _, val(Local)), Locals) :-
	!,
	arg(VN, Locals, Local).
localTerm2(_, _).

:- localArgs(X, _) when X.
localArgs([], _).
localArgs(Arg.Args, Locals) :-
	localTerm2(Arg, Locals),
	localArgs(Args, Locals).

:- localBuiltins(X, Y, _) when X or Y.
localBuiltins([], [], _).
localBuiltins([- | Modes], vr(VN, _, temp, var).Args, Locals) :-
	!,
	% We can only get here if the builtin generates this arg and it's a
	% temporary generated by macro/2, so there is no risk of global/local
	% clashes with other initializing instances.
	arg(VN, Locals, global),
	localBuiltins(Modes, Args, Locals).
localBuiltins(_.Modes, _.Args, Locals) :-
	localBuiltins(Modes, Args, Locals).
