/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- variable allocator.

%	Build a desire list for a given clause.
%	Misses a few cases at the moment.
desireList(Clause, Desires) :-
	desireList(Clause, Opts0, []),
	keySort(Opts0, Opts1),
	groupDesires(Opts1, Desires).

groupDesires([], []).
groupDesires((VN - Opt).Opts, Desires) :-
	Opt = _ - VR,
	groupDesires(VN, VR, [Opt], Opts, Desires).

groupDesires(VN, VR, Group, Opts, Desires) :-
	( Opts = (VN - Opt).OptsT ->
		groupDesires(VN, VR, Opt.Group, OptsT, Desires)
	;	keySort(Group, Sort),
		Desires = desire(VR, Desire).DesiresT,
		gatherDesire(Sort, Desire),
		groupDesires(Opts, DesiresT)
	).

gatherDesire([], []).
gatherDesire((VR - _).Opts, Desire) :-
	gatherDesire(VR, 1, Opts, Desire).

gatherDesire(VR, N, Opts, Desire) :-
	( Opts = (V - Opt).OptsT, V == VR ->
		N1 is N + 1,
		gatherDesire(VR, N1, OptsT, Desire)
	;	Desire = handful(N, VR).DesireT,
		gatherDesire(Opts, DesireT)
	).

desireList(conj(Clauses), D0, D) :-
	!,
	desireListL(Clauses, D0, D).
desireList(disj(_, _, Clauses), D0, D) :-
	!,
	desireListL(Clauses, D0, D).
desireList((C1 -> C2), D0, D) :-
	!,
	desireList(C1, D0, D1),
	desireList(C2, D1, D).
desireList(vr(VN1, VR1, temp, VI1) = vr(VN2, VR2, temp, VI2), D0, D) :-
	(	VI1 = var, VI2 = val(_)
	;	VI2 = var, VI1 = val(_)
	),
	!,
	( nonvar(VR1) ->
		D0 = (VN2 - (VR1 - VR2)).D
	; %nonvar(VR2) ->
		D0 = (VN1 - (VR2 - VR1)).D
	).
desireList(_, D, D).

?- desireListL(X, _, _) when X.
desireListL([], D, D).
desireListL(C.Clauses, D0, D) :-
	desireList(C, D0, D1),
	desireListL(Clauses, D1, D).

?- desireCount(X, _) when X.
desireCount([], 1).
desireCount(desire(_, L).DT, C) :-
	length(L, NL),
	desireCount(DT, CT),
	C is (NL + 1) * CT.

%	Make a conj/disj/-> tree of all vr(_, _, temp, _) occurrences
%	in a clause.  The order in which variables are enumerated
%	matters.
%
%	The second argument is
%		t --> conj(list(u)); disj(_, _, list(u)).
%		u -->	conj(list(u))
%			;	disj(_, _, list(u))
%			;	conj(list(u)) -> conj(list(u))
%			;	vr(_, _, _, _)
%			;	fence.
%
%	Very non-logical.
tempsOfClause(conj(C), conj(Vars)) :-
	tempsOfTermL(C, Vars, []).
tempsOfClause(disj(NSaved, NTemps, C), disj(NSaved, NTemps, Vars)) :-
	tempsOfTermL(C, Vars, []).

?- tempsOfTerm(X, _, _) when X.
tempsOfTerm(Var, V0, V) :-
	Var = vr(_, _, VI, _),
	( VI == temp ->
		V0 = Var.V
	;	V0 = V
	).
tempsOfTerm(head(_), V, V).
tempsOfTerm(fence, fence.V, V).
tempsOfTerm(init(Vars), V0, V) :-
	tempsOfTermL(Vars, V0, V).
tempsOfTerm(T1 = T2, V0, V) :-
	( T1 = vr(_, _, _, _) ->
		tempsOfTerm(T1, V0, V1),
		tempsOfTerm(T2, V1, V)
	;	tempsOfTerm(T2, V0, V1),
		tempsOfTerm(T1, V1, V)
	).
tempsOfTerm(builtin(_, _, SubTerms), V0, V) :-
	tempsOfTermL(SubTerms, V0, V).
tempsOfTerm(struct(_.SubTerms), V0, V) :-
	tempsOfTermL(SubTerms, V0, V).
tempsOfTerm(list(SubTerms), V0, V) :-
	tempsOfTermL(SubTerms, V0, V).
tempsOfTerm(ground(_, _, _), V, V).
tempsOfTerm(string(_), V, V).
tempsOfTerm(const(_), V, V).
tempsOfTerm(goal(_, _, _, SubTerms, _), V0, V) :-
	tempsOfTermL(SubTerms, V0, V).
tempsOfTerm(conj(C), conj(Vars).V, V) :-
	tempsOfTermL(C, Vars, []).
tempsOfTerm((conj(C1) -> conj(C2)), (conj(Vars1) -> conj(Vars2)).V, V) :-
	tempsOfTermL(C1, Vars1, []),
	tempsOfTermL(C2, Vars2, []).
tempsOfTerm(disj(NSaved, NTemps, C), disj(NSaved, NTemps, Vars).V0, V) :-
	V0 = V,
	tempsOfTermL(C, Vars, []).

?- tempsOfTermL(X, _, _) when X.
tempsOfTermL([], V, V).
tempsOfTermL(C.CT, V, VE) :-
	tempsOfTerm(C, V, VE1),
	tempsOfTermL(CT, VE1, VE).

%	Constrain VR field of temporary variables to prevent register
%	allocation clashes.  Also constrain the NTemps field of
%	disjunctions to be large enough to save all active registers.
%
%	Note that the list of potentially active variables is a list
%	of vr/2 and disj/2 records.
%
%	The handling of parameter registers is needed to prevent the
%	allocation of temporary structure building registers to already
%	allocated parameter registers.  Very strange (and currently
%	impossible, thankfully) optimizations involving interaction
%	between disjunctions and parameter stuffing would break this.
constrainRegs(C, Constraints) :-
	constrainRegs(C, [], [], [], first, Constraints, []).

?- constrainRegs(conj(_), _, _, _, _, _, _) when ever.
?- constrainRegs(disj(_, _, _), _, _, _, _, _, _) when ever.
?- constrainRegs((_ -> _), _, _, _, _, _, _) when ever.
?- constrainRegs(fence, _, _, _, X, _, _) when X.
?- constrainRegs(vr(_, _, _, X), _, _, _, _, _, _) when X.
?- constrainRegs(rest, _, _, _, _, _, _) when ever.
constrainRegs(conj(C), Vars, Cont, Params, Level) -->
	{ append(C, Cont, NewCont) },
	constrainCont(Vars, NewCont, Params, Level).
constrainRegs(disj(NSaved, NTemps, C.CT), Vars, Cont, _Params, Level) -->
	[NSaved >= NTemps],
	constrainRegsD(C, CT, disj(NSaved).Vars, disj(NTemps).Vars, Cont, Level).
constrainRegs(vr(VN, VR, _, var), Vars, Cont, Params, Level) -->
	( { integer(VN) } ->
		[use(VR)],
		constrainCont(vr(VN, VR).Vars, Cont, Params, Level)
	;	{ VN = t(_) },
		constrainCont(vr(VN, VR).Vars, Cont, vr(VN, VR).Params, Level)
	).
constrainRegs(vr(VN, VR, _, val(_)), Vars, Cont, Params, Level) -->
	applyConstraints(VN, VR, Vars),
	constrainCont(Vars, Cont, Params, Level).
constrainRegs(vr(_, _, _, uniq), Vars, Cont, Params, Level) -->
	constrainCont(Vars, Cont, Params, Level).
constrainRegs(fence, Vars, Cont, Params, first) -->
	applyParamConstraints(Vars, Params),
	constrainCont([], Cont, [], first).
constrainRegs(fence, Vars, _Cont, Params, rest) -->
	applyParamConstraints(Vars, Params).
constrainRegs(rest, Vars, Cont, Params, _Level) -->
	constrainCont(Vars, Cont, Params, rest).

?- constrainCont(_, X, _, _, _, _) when X.
constrainCont(_, [], _, _) -->
	[].
constrainCont(Vars, C.Cont, Params, Level) -->
	constrainRegs(C, Vars, Cont, Params, Level).

constrainRegsD(conj(C), CT, NSavedVars, NTempsVars, Cont, Level) -->
	constrainRegs(conj(C), NSavedVars, Cont, [], Level),
	constrainRegsDL(CT, NTempsVars, [], rest.Cont).
constrainRegsD((C1 -> C2), CT, NSavedVars, NTempsVars, Cont, Level) -->
	constrainRegs(C1, NSavedVars, C2.Cont, [], Level),
	constrainRegsDL(CT, NTempsVars, [C1], rest.Cont).

constrainRegsDR(conj(C), CT, Vars, Prefix0, Cont0) -->
	{ append(Prefix0, conj(C).Cont0, Cont) },
	constrainCont(Vars, Cont, [], first),
	constrainRegsDL(CT, Vars, [], Cont0).
constrainRegsDR((C1 -> C2), CT, Vars, Prefix0, Cont0) -->
	{ append(Prefix0, C1.C2.Cont0, Cont) },
	constrainCont(Vars, Cont, [], first),
	{ append(Prefix0, [C1], Prefix) },
	constrainRegsDL(CT, Vars, Prefix, Cont0).

?- constrainRegsDL(X, _, _, _, _, _) when X.
constrainRegsDL([], _, _, _) -->
	[].
constrainRegsDL(C.CT, Vars, Prefix, Cont) -->
	constrainRegsDR(C, CT, Vars, Prefix, Cont).
%	constrainRegsDL(CT, Vars, Cont).

?- applyParamConstraints(_, X, _, _) when X.
applyParamConstraints(_, []) -->
	[].
applyParamConstraints(Vars, vr(VN, VR).Params) -->
	applyConstraints(VN, VR, Vars),
	applyParamConstraints(Vars, Params).

?- applyConstraints(_, _, [], _, _) when ever.
?- applyConstraints(_, _, X._, _, _) when X.
applyConstraints(_, _, []) -->
	[].
applyConstraints(VN, VR, vr(VNX, VRX).Vars) -->
	( { VN \== VNX } ->
		( { var(VR) ; var(VRX) } ->
%			[VR ~= VRX],
			{ VR ~= VRX }
		),
		applyConstraints(VN, VR, Vars)
	).
applyConstraints(VN, VR, disj(NTemps).Vars) -->
	[VR < NTemps],
	applyConstraints(VN, VR, Vars).

%	Attempt to allocate desirable registers to temporary variables.
%	Doesn't try very hard!
fulfillDesires(Desires) :-
	fulfillDesiresL(Desires),
	!.

?- fulfillDesiresL(X) when X.
fulfillDesiresL([]).
fulfillDesiresL(desire(VR, Bag).Desires) :-
	(	member(handful(_, VR), Bag)			% Pick a desirable register
	;	true								% Or leave allocation till later
	),
	fulfillDesiresL(Desires).

%	Make a list of temporary variables as vr(2) terms
%	from a conj/disj/-> tree of vr(4) occurances.
%
%	Non-logical.
%	Relies on the VN fields' containing the only integers in the tree.
tempList(Clause, Temps) :-
	tempList(Clause, [], Temps).

?- tempList(X, _, _) when X.
tempList(conj(Clauses), Temps, NewTemps) :-
	tempListL(Clauses, Temps, NewTemps).
tempList(disj(_, _, Clauses), Temps, NewTemps) :-
	tempListL(Clauses, Temps, NewTemps).
tempList((C1 -> C2), Temps0, Temps) :-
	tempList(C1, Temps0, Temps1),
	tempList(C2, Temps1, Temps).
tempList(vr(VN, VR, _, VI), Temps0, Temps) :-
	( integer(VN), VI == var, \+ occurs(VN, Temps0) ->
		Temps = vr(VN, VR).Temps0
	;	Temps = Temps0
	).
tempList(fence, Temps, Temps).

?- tempListL(X, _, _) when X.
tempListL([], Temps, Temps).
tempListL(C.Clauses, Temps, NewTemps) :-
	tempList(C, Temps, Temps1),
	tempListL(Clauses, Temps1, NewTemps).

%	Allocate registers to temporary variables.
allocateTemps(Constraints, Arity, MaxTemp) :-
	allocateTemps1(Constraints, 0, MaxTemp0),
	( Arity > MaxTemp0 ->
		MaxTemp = Arity
	;	MaxTemp = MaxTemp0
	),
	allocateTemps2(Constraints, MaxTemp),
	!.
allocateTemps(_, _, _) :-
	throwCompilerError(
		format(user_error, "~NError -- unable to allocate temporaries.~n\c
							Consult your guru.~n", [])).

?- allocateTemps1([], _, _) when ever.
?- allocateTemps1(X._, _, _) when X.
allocateTemps1([], Max, Max).
allocateTemps1(use(_).TempList, Max0, Max) :-
	allocateTemps1(TempList, Max0, Max).
%allocateTemps1((_ ~= _).TempList, Max0, Max) :-
%	allocateTemps1(TempList, Max0, Max).
allocateTemps1((_ >= _).TempList, Max0, Max) :-
	allocateTemps1(TempList, Max0, Max).
allocateTemps1((VR < NT).TempList, Max0, Max) :-
	once between(0, 253, VR),
	VR < NT,					% More efficient to constrina it here.
	( Max0 > VR ->
		Max1 = Max0
	;	Max1 = VR
	),
	allocateTemps1(TempList, Max1, Max).

?- allocateTemps2([], _) when ever.
?- allocateTemps2(X._, _) when X.
allocateTemps2([], _).
allocateTemps2(use(VR).TempList, Base) :-
	( var(VR) ->
		once between(Base, 253, VR)
	),
	allocateTemps2(TempList, Base).
%allocateTemps2((_ ~= _).TempList, Base) :-
%	allocateTemps2(TempList, Base).
allocateTemps2((NSave >= NTemp).TempList, Base) :-
	once between(0, 253, NTemp),						% Not Base!
	once between(NTemp, 253, NSave),
	allocateTemps2(TempList, Base).
allocateTemps2((VR < _).TempList, Base) :-
	allocateTemps2(TempList, Base).

%	Allocate places in the environment to permanent variables
%	and fill in the active vars field of call instructions.
%
%	A quick fudge.
allocatePerms(Clause, MaxPerm) :-
	allocatePerms(Clause, 0, _, 0, MaxPerm).

?- allocatePerms(X, _, _, _, _) when X.
allocatePerms(conj(C), Used, NewUsed, Free, NewFree) :-
	allocatePermsL(C, Used, NewUsed, Free, NewFree).
allocatePerms(disj(_, _, C), Used, NewUsed, Free, NewFree) :-
	allocatePermsD(C, Used, NewUsed, Free, NewFree).
allocatePerms((C1 -> C2), Used, NewUsed, Free, NewFree) :-
	allocatePermsL([C1, C2], Used, NewUsed, Free, NewFree).
allocatePerms(goal(_, _, Used, _, _), Used, Used, Free, Free).
allocatePerms(init(Vars), Used, NewUsed, Free, NewFree) :-
	allocatePermsTL(Vars, Used, NewUsed, Free, NewFree).
allocatePerms(T1 = T2, Used0, Used, Free0, Free) :-
	allocatePermsT(T2, Used0, Used1, Free0, Free1),
	allocatePermsT(T1, Used1, Used, Free1, Free).
allocatePerms(builtin(_, _, Args), Used, NewUsed, Free, NewFree) :-
	allocatePermsTL(Args, Used, NewUsed, Free, NewFree).
allocatePerms(fence, Used, Used, Free, Free).
allocatePerms(head(_), Used, Used, Free, Free).

?- allocatePermsL(X, _, _, _, _) when X.
allocatePermsL([], Used, Used, Free, Free).
allocatePermsL(C.CT, Used0, Used, Free0, Free) :-
	allocatePermsL(CT, Used0, Used1, Free0, Free1),
	allocatePerms(C, Used1, Used, Free1, Free).

%	Not same as allocatePermsL/4.
?- allocatePermsD(X, _, _, _, _) when X.
allocatePermsD([], Used, Used, Free, Free).
allocatePermsD(C.CT, Used, NewUsed, Free0, Free) :-
	allocatePermsD(CT, Used, Used1, Free0, Free1),
	allocatePerms(C, Used, Used2, Free1, Free),
	( Used1 > Used2 ->
		NewUsed = Used1
	;	NewUsed = Used2
	).

allocatePermsT(const(Const), Used, Used, Free, Free) :-
	const(Const).
allocatePermsT(vr(_, VR, VT, _), Used, NewUsed, Free, NewFree) :-
	( VT == temp ->
		NewUsed = Used,
		NewFree = Free
	;	% VT = perm here
	  VR = Free ->						% Yeuch!
		NewFree is Free + 1,
		NewUsed = NewFree
	;	NewFree = Free,
		( VR >= Used ->
			NewUsed is VR + 1
		;	NewUsed = Used
		)
	).
allocatePermsT(struct(_.Args), Used, NewUsed, Free, NewFree) :-
	allocatePermsTL(Args, Used, NewUsed, Free, NewFree).
allocatePermsT(list(Args), Used, NewUsed, Free, NewFree) :-
	allocatePermsTL(Args, Used, NewUsed, Free, NewFree).
allocatePermsT(string(_), Used, Used, Free, Free).
allocatePermsT(ground(_, _, _), Used, Used, Free, Free).

?- allocatePermsTL(X, _, _, _, _) when X.
allocatePermsTL([], Used, Used, Free, Free).
allocatePermsTL(C.CT, Used0, Used, Free0, Free) :-
	allocatePermsTL(CT, Used0, Used1, Free0, Free1),
	allocatePermsT(C, Used1, Used, Free1, Free).
