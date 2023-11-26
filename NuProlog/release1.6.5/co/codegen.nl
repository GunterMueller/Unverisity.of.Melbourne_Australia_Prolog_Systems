/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- code generator.

codegen(Clause, NArgs, EnvNeeded0, NPerms, Code) :-
	codegen2(Clause, CT1),
	codegenDallExec(EnvNeeded0, CT1, CT2, EnvNeeded1),
	unsafe(CT2),
	( EnvNeeded1 == true ->
		allocate(CT2, NPerms, CT3)
	;	CT2 = CT3
	),
	codegenDisj(CT3, NArgs, Code0),
	codegenCleanUp(Code0, Code).

codegen2(conj(C), conj(Code)) :-
	codegen2L(C, Code, []).

codegen2(disj(NSaved, NTemps, C), Code0, Code) :-
	!,
	Code0 = disj(NSaved, NTemps, D).Code,
	codegen2D(C, D).
codegen2(Stmt, Code0, Code) :-
	codegenStmt(Stmt, Code0, Code).

:- codegen2L(X, _, _) when X.
codegen2L([], Code, Code).
codegen2L(C.CT, Code0, Code) :-
	codegen2(C, Code0, Code1),
	codegen2L(CT, Code1, Code).

:- codegen2D([], _) when ever.
:- codegen2D(X._, _) when X.
codegen2D([], []).
codegen2D(conj(C).CT, conj(D).DT) :-
	codegen2L(C, D, []),
	codegen2D(CT, DT).
codegen2D((conj(C1) -> conj(C2)).CT, (conj(D1) -> conj(D2)).DT) :-
	codegen2L(C1, D1, []),
	codegen2L(C2, D2, []),
	codegen2D(CT, DT).

%	Convert one "statement" into machine code.
codegenStmt(X1 = X2, C0, C) :-
	codegenEquals(X1, X2, C0, C).
codegenStmt(goal(Pred, Arity, NVars, _Params, Par),
		call(Pred, Arity, NVars, Par).C, C).
codegenStmt(builtin(Functor, Arity, Args), C0, C) :-
	codegenBuiltin(Functor, Arity, Args, C0, C).
codegenStmt(fence, C, C).
codegenStmt(head(_), C, C).
codegenStmt(neck(NVars), neck(NVars).C, C).
codegenStmt(init(Inits), C0, C) :-
	codegenInits(Inits, C0, C).

codegenEquals(Var1, Var2, C0, C) :-
	Var1 = vr(_, _, _, _),
	Var2 = vr(_, _, _, _),
	(	codegenDataM(Var1, Var2, C0, C)
	;	codegenDataM(Var2, Var1, C0, C)
	),
	!.
codegenEquals(vr(_, VR, VT, VI), Term, C0, C) :-
	codegenStructM(VR, VT, VI, Term, C0, C),
	!.
codegenEquals(Term, vr(_, VR, VT, VI), C0, C) :-
	codegenStructM(VR, VT, VI, Term, C0, C).

%	Arrange of the movement of data between variables.
%	Needs to be called with the variables reversed
%	if it fails the first time.
%
%	Note that pval/3 and pvar/2 must always have a temporary as their
%	second register and the pval/3 always has a perm as its first.
%
%	The exact details of some of the later cases are shrouded in mystery
%	and potential bugs.  The major source of confusion is unsafety and the
%	question of when it matters.  Most moves amongst permanent variables
%	are effectively safe despite what unsafe/1 thinks, but fixing this
%	is not worth the effort.
%
%	A related problem is keeping pointers between permanent variables
%	going the right way.  The code Y1 = Y2 should have the same effect as
%	eq(Y1, Y2) in all cases.  Glvar is used to achieve this, but it could
%	just as well be done by calling unify() explicitly.
%:- codegenDataM(vr(_, _, VT1, VI1), vr(_, _, VT2, VI2), _, _)
%	when VI1 and VI2 and VT1 and VT2.						% Really index
:- codegenDataM(vr(_, _, _, VI1), _, _, _) when VI1. 		% Really index
codegenDataM(vr(_, _, _, uniq), vr(_, _, _, uniq), CE, CE).
codegenDataM(vr(_, _, _, uniq), vr(_, VR2, VT2, var), init(Var2).CE, CE) :-
	codegenVariable(VT2, VR2, Var2).
codegenDataM(vr(_, _, _, uniq), vr(_, _, _, val(_)), CE, CE).
codegenDataM(vr(_, VR1, temp, var), vr(_, VR2, VT2, var),
		pvar(Var2, temp(VR1)).CE, CE) :-
	codegenVariable(VT2, VR2, Var2).
codegenDataM(vr(_, VR1, temp, var), vr(_, VR2, temp, val(_)),
		gvar(temp(VR1), temp(VR2)).CE, CE).
codegenDataM(vr(_, VR1, temp, var), vr(_, VR2, perm, val(Safe)),
		pval(perm(VR2), Safe, temp(VR1)).CE, CE).
codegenDataM(vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_)),
		gval(temp(VR2), temp(VR1)).CE, CE).
codegenDataM(vr(VN1, VR1, temp, val(local)), vr(_, VR2, perm, var),
%		glvar(perm(VR2), temp(VR1)).CE, CE) :-
		[	pvar(perm(VR2), temp(255)),
			gval(temp(VR1), temp(255))
		|	CE],
		CE) :-
	integer(VN1).
codegenDataM(vr(VN1, VR1, temp, val(global)), vr(_, VR2, perm, var),
		gvar(perm(VR2), temp(VR1)).CE, CE) :-
	integer(VN1).
codegenDataM(vr(t(_), VR1, temp, val(_)), vr(_, VR2, perm, var),
		gvar(perm(VR2), temp(VR1)).CE, CE).
%	Leave the decision about using PUVAL or GVALY until safety is decided.
codegenDataM(vr(_, VR1, temp, val(_)), vr(_, VR2, perm, val(Safe)),
		[	pval(perm(VR2), Safe, temp(255)),
			gval(temp(VR1), temp(255))
		|	CE],
		CE).
codegenDataM(vr(_, VR, perm, var), vr(_, VR, perm, var),
		pvar(perm(VR), temp(255)).CE, CE).
codegenDataM(vr(_, VR1, perm, var), vr(_, VR2, perm, var),
		[	pvar(perm(VRA), temp(255)),
			gvar(perm(VRB), temp(255))
		|	CE],
		CE) :-
	( VR1 < VR2 ->
		VRA = VR1,
		VRB = VR2
	;	VRA = VR2,
		VRB = VR1
	).
/*
codegenDataM(vr(_, VR1, perm, var), vr(_, VR2, perm, val(Safe)),
		[	pval(perm(VR2), Safe, temp(255)),% leave safety to unsafe/1
			gvar(perm(VR1), temp(255))
		|	CE],
		CE) :-
	VR2 < VR1.
*/
codegenDataM(vr(_, VR1, perm, var), vr(_, VR2, perm, val(Safe)),
		[	pval(perm(VR2), Safe, temp(255)),% leave safety to unsafe/1
			glvar(perm(VR1), temp(255))
		|	CE],
		CE) :-
true.
/*
	VR2 > VR1.
*/
codegenDataM(vr(_, VR1, perm, val(_)), vr(_, VR2, perm, val(Safe)),
		[	pval(perm(VR2), Safe, temp(255)),% leave safety to unsafe/1
			gval(perm(VR1), temp(255))
		|	CE],
		CE).

%	Associate a variable with a non-variable.
%	Remember that even _ = f(X) may require some code.
%	codegenStructM(VR, VT, VI, Term, C, CE).
%:- codegenStructM(_, _, VI, Term, _, _) when VI and Term.	% Really index
:- codegenStructM(_, _, VI, _, _, _) when VI.				% Really index
codegenStructM(_, _, uniq, const(Atomic),
		{'Unification with unique variable'}.CE, CE) :-
	atomic(Atomic).
codegenStructM(_, _, uniq, struct(_.Args),
		{'Unification with unique variable'}.C, CE) :-
	codegenInits(Args, C, CE).
codegenStructM(_, _, uniq, list(Args),
		{'Unification with unique variable'}.C, CE) :-
	codegenInits(Args, C, CE).
codegenStructM(_, _, uniq, string(_),
		{'Unification with unique variable'}.CE, CE).
codegenStructM(_, _, uniq, ground(_, _, _),
		{'Unification with unique variable'}.CE, CE).
codegenStructM(VR, VT, var, const(Atomic), pc(const(Atomic), Var).CE, CE) :-
	atomic(Atomic),
	codegenVariable(VT, VR, Var).
codegenStructM(VR, VT, var, struct(Pred.Args),
		ps(Pred/Arity, Var).C, CE) :-
	codegenVariable(VT, VR, Var),
	length(Args, Arity),
	codegenArgs(Args, C, CE).
codegenStructM(VR, VT, var, list(Args), pl(Var).C, CE) :-
	codegenVariable(VT, VR, Var),
	codegenArgs(Args, C, CE).
codegenStructM(VR, VT, var, ground(Tag, Label, Term),
		pc(#(Tag, Label, Term), Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenStructM(VR, VT, val(_), const(Atomic),
		gc(Var, const(Atomic)).CE, CE) :-
	const(Atomic),
	codegenVariable(VT, VR, Var).
%codegenStructM(VR, VT, val(_), const(UCN),
%		[	pc(UCN, temp(255)),
%			gval(Var, temp(255))
%		|	CE
%		], CE) :-
%	$ucn(UCN),
%	codegenVariable(VT, VR, Var).
codegenStructM(VR, VT, val(_), ground(Tag, Label, Term),
		[	pc(#(Tag, Label, Term), temp(255)),
			gval(Var, temp(255))
		|	CE
		], CE) :-
	codegenVariable(VT, VR, Var).
codegenStructM(VR, VT, val(_), struct(Pred.Args),
		gs(Var, Pred/Arity).C, CE) :-
	codegenVariable(VT, VR, Var),
	length(Args, Arity),
	codegenArgs(Args, C, CE).
codegenStructM(VR, VT, val(_), list(Args), gl(Var).C, CE) :-
	codegenVariable(VT, VR, Var),
	codegenArgs(Args, C, CE).

tagOfTerm(string(_), 'CHR').
tagOfTerm(list(_), 'LST').
tagOfTerm(struct(_), 'STR').

:- codegenArgs(X, _, _) when X.
codegenArgs([], CE, CE).
codegenArgs(Arg.Args, C, CE) :-
	codegenArg(Arg, C, C1),
	codegenArgs(Args, C1, CE).

:- codegenArg(const(_), _, _) when ever.
:- codegenArg(ground(_, _, _), _, _) when ever.
:- codegenArg(list(_), _, _) when ever.
:- codegenArg(struct(_), _, _) when ever.
:- codegenArg(vr(_, _, _, VI), _, _) when VI.				% Really index.
codegenArg(const(Atomic), uc(const(Atomic)).C, C) :-
	atomic(Atomic).
codegenArg(ground(Tag, Label, Term), uc(#(Tag, Label, Term)).C, C).
codegenArg(list(Args), ul.C0, C) :-
	codegenArgs(Args, C0, C).
codegenArg(struct(Pred.Args), us(Pred/Arity).C0, C) :-
	length(Args, Arity),
	codegenArgs(Args, C0, C).
codegenArg(vr(_, _, _, uniq), uvoid(1).C, C).
codegenArg(vr(_, VR, VT, var), uvar(Var).C, C) :-
	codegenVariable(VT, VR, Var).
codegenArg(vr(_, VR, VT, val(Safe)), uval(Var, Safe).C, C) :-
	codegenVariable(VT, VR, Var).

%	Add an initializer for each vr(_, _, _, var) in a list.
%	Ignore anything else present, particularly vr(_, _, _, val(_))
%	terms produced by our lazy handling of nested disjunctions.
:- codegenInits(X, _, _) when X.
codegenInits([]) -->
	[].
codegenInits(vr(_, VR, VT, var).Inits) -->
	!,
	[init(Var)],
	{ codegenVariable(VT, VR, Var) },
	codegenInits(Inits).
codegenInits(_.Inits) -->
	codegenInits(Inits).

%	Clean up the raw code generated by codegen.
%	Removes no-ops, unreachable code, etc.
:- codegenCleanUp([], _) when ever.
:- codegenCleanUp(X._, _) when X.						% Really index.
codegenCleanUp([], []).
codegenCleanUp(gvar(Var, Var).Code0, Code) :-
	!,
	codegenCleanUp(Code0, Code).
codegenCleanUp(gvara(Var, Var).Code0, Code) :-
	!,
	codegenCleanUp(Code0, Code).
codegenCleanUp(gs(Var, Pred/2).U2.U3.Code0, Code) :-
	codegenCleanUpUVAR(U2, VR2),
	codegenCleanUpUVAR2(U3, VR3, Code0, Code1),
	!,
	codegenCleanUp(gs2(Var, temp(VR2), temp(VR3), Pred/2).Code1, Code).
codegenCleanUp(gs(Var, Pred/1).U2.Code0, Code) :-
	codegenCleanUpUVAR(U2, VR2),
	!,
	codegenCleanUp(gs1(Var, temp(VR2), Pred/1).Code0, Code).
codegenCleanUp(gl(Var).U2.U3.Code0, Code) :-
	codegenCleanUpUVAR(U2, VR2),
	codegenCleanUpUVAR2(U3, VR3, Code0, Code1),
	!,
	codegenCleanUp(glv2(Var, temp(VR2), temp(VR3)).Code1, Code).
codegenCleanUp(
		[	pval(perm(VR2), safe, temp(255)),
			gval(temp(VR1), temp(255))
		|	Code0],
		Code) :-
	!,
	codegenCleanUp(gval(perm(VR2), temp(VR1)).Code0, Code).
codegenCleanUp(
		[	init(temp(VR3)),
			setarg(temp(VR1), temp(VR2), temp(VR3))
		|	Code0],
		Code) :-
	!,
	codegenCleanUp(setarg_var(temp(VR1), temp(VR2), temp(VR3)).Code0, Code).
codegenCleanUp(
		[	init(temp(VR3)),
			replacn(temp(VR1), temp(VR2), temp(VR3))
		|	Code0],
		Code) :-
	!,
	codegenCleanUp(replacn_var(temp(VR1), temp(VR2), temp(VR3)).Code0, Code).
codegenCleanUp(arith.push(P).Code0, Code) :-
	!,
	codegenCleanUp(apush(P).Code0, Code).
codegenCleanUp(uvoid(N1).uvoid(N2).Code0, Code) :-
	N is N1 + N2,
	N < 256,
	!,
	codegenCleanUp(uvoid(N).Code0, Code).
codegenCleanUp(dall.pro.Code, NewCode0) :-
	!,
	NewCode0 = dallPro.NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(pro.Code, NewCode0) :-
	!,
	NewCode0 = pro.NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(fail.Code, NewCode0) :-
	!,
	NewCode0 = fail.NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(j(L).Code, NewCode0) :-
	!,
	NewCode0 = j(L).NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(dall.exec(A, B).Code, NewCode0) :-
	!,
	NewCode0 = dallExec(A, B).NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(exec(A, B).Code, NewCode0) :-
	!,
	NewCode0 = exec(A, B).NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(funcall(N).Code, NewCode0) :-
	!,
	NewCode0 = funcall(N).NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(apply(N).Code, NewCode0) :-
	!,
	NewCode0 = apply(N).NewCode,
	codegenUnreachable(Code, NewCode).
codegenCleanUp(Inst.Code, Inst.NewCode) :-
	codegenCleanUp(Code, NewCode).

codegenCleanUpUVAR(uvar(temp(VR)), VR).
codegenCleanUpUVAR(uvara(temp(VR)), VR).
codegenCleanUpUVAR(uvoid(1), 255).

codegenCleanUpUVAR2(uvar(temp(VR)), VR, Code, Code).
codegenCleanUpUVAR2(uvara(temp(VR)), VR, Code, Code).
codegenCleanUpUVAR2(uvoid(1), 255, Code, Code).
codegenCleanUpUVAR2(ul, 254, gl(254).Code, Code).
codegenCleanUpUVAR2(us(P), 254, gs(254, P).Code, Code).

%	Skip instructions until a label is found.
:- codegenUnreachable(X, _) when X.
codegenUnreachable([], []).
codegenUnreachable(Label.Code, NewCode0) :-
	Label = $(_),
	!,
	NewCode0 = Label.NewCode,
	codegenCleanUp(Code, NewCode).
codegenUnreachable(_.Code, NewCode) :-
	codegenUnreachable(Code, NewCode).

%	Add deallocate and proceed instructions,
%	and convert calls to execs where possible.
codegenDallExec(EnvNeeded0, conj(Code), conj(NewCode), EnvNeeded) :-
	reverse(Code, RevCode),
	co$exec(RevCode, ExecCode, Dall),
	(EnvNeeded0 = true, Dall = dall, instructionOf(Inst, ExecCode), usesY(Inst)
	->	EnvNeeded = true
	;	Dall = nop,
		EnvNeeded = false
	),
	reverse(ExecCode, NewCode).

:- co$exec([], _, _) when ever.
:- co$exec(X._, _, _) when X.
co$exec({Comment}.Code, NewCode0, Dall) :-
	!,
	NewCode0 = {Comment}.NewCode,
	co$exec(Code, NewCode, Dall).
co$exec(call(Pred, Arity, N, seq).Code, NewCode, Dall) :-
	!,
	NewCode = exec(Pred, Arity).Dall.Code.
co$exec(disj(NSaved, NTemps, D).Code, NewCode, Dall) :-
	!,
	NewCode = disj(NSaved, NTemps, NewD).Code,
	co$execD(D, NewD, Dall).
co$exec(Code, pro.Dall.Code, Dall).

:- co$execD([], _, _) when ever.
:- co$execD(X._, _, _) when X.
co$execD([], [], _).
co$execD(conj(C).CT, conj(D).DT, Dall) :-
	reverse(C, RC),
	co$exec(RC, EC, Dall),
	reverse(EC, D),
	co$execD(CT, DT, Dall).
co$execD((conj(C1) -> conj(C2)).CT, (conj(C1) -> conj(D2)).DT, Dall) :-
	%	Note that the deallocate will always be in the body of the cond.
	reverse(C2, RC2),
	co$exec(RC2, EC2, Dall),
	reverse(EC2, D2),
	co$execD(CT, DT, Dall).

:- instructionOf(_, Y) when Y.
instructionOf(X, Y.Z) :-
	!,
	(	instructionOf(X, Y)
	;	instructionOf(X, Z)
	).
instructionOf(X, conj(D)) :-
	!,
	instructionOf(X, D).
instructionOf(X, (Y -> Z)) :-
	!,
	(	instructionOf(X, Y)
	;	instructionOf(X, Z)
	).
instructionOf(X, disj(_, _, D)) :-
	!,
	instructionOf(X, D).
instructionOf(X, X).

unsafe(conj(Code)) :-
	unsafeVars(Code, Unsafes),
	reverse(Code, RevCode),
	unsafe(RevCode, Unsafes).

unsafe(Code, Unsafes) :-
	unsafe(Code, Unsafes, 0, _).

:- unsafe([], _, _, _) when ever.
:- unsafe(X._, _, _, _) when X.								% Really index.
unsafe([], _, PVars, PVars).
unsafe(call(_, _, PVars, _).Code, Unsafes, _, LPVars) :-
	!,
	unsafe(Code, Unsafes, PVars, LPVars).
unsafe(pval(perm(VR1), unsafe, _).Code, Unsafes, PVars, LPVars) :-
	VR1 >= PVars,
	member(VR1, Unsafes),
	!,
	unsafe(Code, Unsafes, PVars, LPVars).
unsafe(pval(_, safe, _).Code, Unsafes, PVars, LPVars) :-
	!,
	unsafe(Code, Unsafes, PVars, LPVars).
unsafe(disj(_, _, D).Code, Unsafes, PVars, LPVars) :-
	!,
	unsafeD(D, Unsafes, PVars, LPVarsD),
	unsafe(Code, Unsafes, LPVarsD, LPVars).
unsafe(_.Code, Unsafes, PVars, LPVars) :-
	unsafe(Code, Unsafes, PVars, LPVars).

:- unsafeD([], _, _, _) when ever.
:- unsafeD(X._, _, _, _) when X.
unsafeD([], _, _, 99999).					% i.e. maxint
unsafeD(conj(C).CT, Unsafes, PVars, LPVars) :-
	unsafeD2(C, Unsafes, PVars, LPVars, CT).
unsafeD((conj(C1) -> conj(C2)).CT, Unsafes, PVars, LPVars) :-
	append(C1, C2, C),
	unsafeD2(C, Unsafes, PVars, LPVars, CT).

unsafeD2(C, Unsafes, PVars, LPVars, CT) :-
	reverse(C, RC),
	unsafe(RC, Unsafes, PVars, LPVars1),
	unsafeD(CT, Unsafes, PVars, LPVars2),
	(LPVars1 < LPVars2 ->
		LPVars = LPVars1
	;	LPVars = LPVars2
	).

unsafeVars(C, Unsafes) :-
	unsafeVars(C, Unsafes, []).

:- unsafeVars([], _, _) when ever.
:- unsafeVars(X._, _, _) when X.							% Really index.
unsafeVars([], Vars, Vars).
unsafeVars(pvar(perm(VR), _).Code, VR.Vars, VarsEnd) :-
	!,
	unsafeVars(Code, Vars, VarsEnd).
unsafeVars(glvar(perm(VR), _).Code, VR.Vars, VarsEnd) :-
	!,
	unsafeVars(Code, Vars, VarsEnd).
unsafeVars(init(perm(VR)).Code, VR.Vars, VarsEnd) :-
	!,
	unsafeVars(Code, Vars, VarsEnd).
unsafeVars(disj(_, _, D).Code, Vars, VarsEnd) :-
	!,
	unsafeVarsD(D, Vars, VE1),
	unsafeVars(Code, VE1, VarsEnd).
unsafeVars(_.Code, Unsafes, PVars) :-
	unsafeVars(Code, Unsafes, PVars).

:- unsafeVarsD([], _, _) when ever.
:- unsafeVarsD(X._, _, _) when X.
unsafeVarsD([], Vars, Vars).
unsafeVarsD(conj(C).CT, Vars0, Vars) :-
	unsafeVars(C, Vars0, Vars1),
	unsafeVarsD(CT, Vars1, Vars).
unsafeVarsD((conj(C1) -> conj(C2)).CT, Vars0, Vars) :-
	unsafeVars(C1, Vars0, Vars1),
	unsafeVars(C2, Vars1, Vars2),
	unsafeVarsD(CT, Vars2, Vars).

%	Install the allocate instruction.
allocate(conj(Code0), NPerms, conj(Code)) :-
	allocate2(Code0, NPerms, Code).

:- allocate2(X, _, _) when X.
allocate2([], _, []).
allocate2(X.Code, NPerms, all(NPerms).X.Code) :-
	instructionOf(Inst, X),
	usesY(Inst),
	!.
allocate2(Inst.Code0, NPerms, Inst.Code) :-
	allocate2(Code0, NPerms, Code).

codegenDisj(conj(Code0), NArgs, Code) :-
	codegenDisj(Code0, NArgs, Code, []).

:- codegenDisj([], _, _, _) when ever.
:- codegenDisj(X._, _, _, _) when X.						% Really index.
codegenDisj([], _, C, C).
codegenDisj(disj(NSaved, NTemps, D).Code, NArgs0, C0, C) :-	
	!,
	codegenDisjD(D, NSaved, NTemps, NArgs0, NArgs, C0, C1),
	codegenDisj(Code, NArgs, C1, C).
codegenDisj(gvar(V1, V2).Code, NArgs, gvara(V1, V2).C0, C) :-	
	V1 = temp(VR1),
	VR1 < NArgs,
	!,
	codegenDisj(Code, NArgs, C0, C).
codegenDisj(uvar(V1).Code, NArgs, uvara(V1).C0, C) :-	
	V1 = temp(VR1),
	VR1 < NArgs,
	!,
	codegenDisj(Code, NArgs, C0, C).
codegenDisj(Inst.Code, NArgs, Inst.C0, C) :-	
	codegenDisj(Code, NArgs, C0, C).

codegenDisjD(D, NSaved, NTemps, NArgs0, NArgs, C0, C) :-
	codegenDisjD1(D, 0, LE, nop, NTemps, NArgs0, C0, $(LE).C, Table),
	codegenDisjTable(Table, NSaved, NTemps, NArgs0, NArgs).

codegenDisjTable([], _, _, NArgs, NArgs).
codegenDisjTable(choice(te(NSaved, NTemps, L), _).Table,
		NSaved, NTemps, _, NTemps) :-
	Table = choice(_, L)._,
	codegenDisjTable(Table, NTemps).

:- codegenDisjTable(_.X, _) when X.
codegenDisjTable([choice(tre(NTemps), _)], NTemps).
codegenDisjTable(choice(re(NTemps, L), _).Table, NTemps) :-
	Table = choice(_, L)._,
	codegenDisjTable(Table, NTemps).

%	Note that only TE needs both NSaved and NTemps.
:- codegenDisjD1([], _, _, _, _, _, _, _, _) when ever.
:- codegenDisjD1(X.Y, _, _, _, _, _, _, _, _) when X and Y.
codegenDisjD1([], _, _, _, _, _, C, C, []).
codegenDisjD1([conj(C)], N, _, EP, _NTemps, NArgs, Code0, Code, Table) :-
	codegenDisjCP(N, Code0, EP.Code1, Table, []),
	codegenDisj(C, NArgs, Code1, Code).
codegenDisjD1((conj(Cond) -> conj(Body)).C2.CT, N, LE, EP, NTemps, NArgs,
		Code0, Code, Table) :-
	codegenDisjCP(N, Code0, EP.Code1, Table, Table1),
	codegenSimpleCondL(Cond, L, Code1, Code2),
	( N > 0 ->
		codegenDisj(Body, NTemps, Code3, j(LE).Code4),
		codegenDisjD1(C2.CT, -1, LE, $(L), NTemps, NTemps, Code4, Code, Table1),
		%	We re-use the choice point if the else part needs one, so
		%	we must clean it up in the then part.
		( cons(Table1) ->
			Code2 = tre(0).Code3
		;	Code2 = Code3
		)
	;	codegenDisj(Body, NArgs, Code2, j(LE).$(L).Code4),
		codegenDisjD1(C2.CT, N, LE, nop, NTemps, NArgs, Code4, Code, Table1)
	).
codegenDisjD1(conj(C1).C2.CT, N, LE, EP, NTemps, _NArgs, Code0, Code, Table) :-
	N1 is N + 1,
	codegenDisjCP(N1, Code0, EP.Code1, Table, Table1),
	codegenDisj(C1, NTemps, Code1, j(LE).Code2),
	codegenDisjD1(C2.CT, 1, LE, nop, NTemps, NTemps, Code2, Code, Table1).

codegenDisjCP(N, Code0, Code, Table0, Table) :-
	( N > 0 ->
		Code0 = $(L).CP.Code,
		Table0 = choice(CP, L).Table
	;	Code0 = Code,
		Table0 = Table
	).

codegenSimpleCond(id(T1, T2), LF) -->
	!,
	[identTest(T1, T2), jFail(LF)].
codegenSimpleCond(notid(T1, T2), LF) -->
	!,
	[identTest(T1, T2), jTrue(LF)].
codegenSimpleCond(type(V, TypeCode), LF) -->
	!,
	[jtype(V, TypeCode, LF)].
codegenSimpleCond(ctype(V, TypeCode), LF) -->
	!,
	[jctype(V, TypeCode, LF)].
codegenSimpleCond(arithP(Op), LF) -->
	!,
	[jArithP(Op, LF)].
codegenSimpleCond(fail, LF) -->
	!,
	[j(LF)].
codegenSimpleCond(disj(_, _, Disj), LF, D, DE) :-
	!,
	codegenSimpleCondD(Disj, LS, LF, D, $(LS).DE).
codegenSimpleCond(Inst, _LF) -->
	[Inst].

:- codegenSimpleCondL(X, _, _, _) when X.
codegenSimpleCondL([], _, D, D).
codegenSimpleCondL(C.CT, LF, D0, D) :-
	codegenSimpleCond(C, LF, D0, D1),
	codegenSimpleCondL(CT, LF, D1, D).

:- codegenSimpleCondD([], _, _, _, _) when ever.
:- codegenSimpleCondD(X._, _, _, _, _) when X.
codegenSimpleCondD([], _LS, _LF, DE, DE).
codegenSimpleCondD([conj(C)], _LS, LF, D, DE) :-
	codegenSimpleCondL(C, LF, D, DE).
codegenSimpleCondD(C1.C2.CT, LS, LF, D, DE) :-
	codegenSimpleCond1(C1, LN, LF, D, j(LS).$(LN).DE1),
	codegenSimpleCondD(C2.CT, LS, LF, DE1, DE).

codegenSimpleCond1((conj(C1) -> conj(C2)), LA, LF, D0, D) :-
	codegenSimpleCondL(C1, LA, D0, D1),
	codegenSimpleCondL(C2, LF, D1, D).
codegenSimpleCond1(conj(C), LA, _, D0, D) :-
	codegenSimpleCondL(C, LA, D0, D).

%	Enumerate on backtracking all the variables in an instruction.
usesVar(gvar(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(gvara(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(glvar(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(gval(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(gc(V1, _), V1).
usesVar(gs(V1, _), V1).
usesVar(gs1(V1, V2, _), V) :- (V = V1 ; V = V2).
usesVar(gs2(V1, V2, V3, _), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(gl(V1), V1).
usesVar(glv2(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(init(V1), V1).
usesVar(use(V1), V1).
usesVar(pvar(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(pval(V1, _, V2), V) :- (V = V1 ; V = V2).
usesVar(pc(_, V1), V1).
usesVar(ps(_, V1), V1).
usesVar(pl(V1), V1).
usesVar(uvar(V1), V1).
usesVar(uvara(V1), V1).
usesVar(uval(V1, _), V1).
usesVar(push(V1), V1) :- isDeterminedVar(V1).
usesVar(apush(V1), V1) :- isDeterminedVar(V1).
usesVar(pushv(V1), V1) :- isDeterminedVar(V1).
usesVar(pop(V1), V1).
usesVar(popv(V1), V1) :- isDeterminedVar(V1).
usesVar(name(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(ltos(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(occurs(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(univ(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(arg(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(plus(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(functor(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(jv(V1), V1).
usesVar(jnv(V1), V1).
usesVar(jc(V1, _, _), V1).
usesVar(js(V1, _, _), V1).
usesVar(sot(V1, _, _, _, _), V1).
usesVar(execsot(_, V1, _, _, _, _), V1).
usesVar(sos(V1, _, _), V1).
usesVar(sose(V1, _, _), V1).
usesVar(soc(V1, _, _), V1).
usesVar(soce(V1, _, _), V1).
usesVar(label(V1), V1).
usesVar(type(V1, _), V1).
usesVar(ctype(V1, _), V1).
usesVar(jtype(V1, _, _), V1).
usesVar(jctype(V1, _, _), V1).
usesVar(display(V1), V1).
usesVar(id(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(notid(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(identTest(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(eRef(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(mkObj(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(compare(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(sort(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(get0(V1), V1).
usesVar(get(V1), V1).
usesVar(put(V1), V1).
usesVar(get0(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(get(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(sprt(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(getToken(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(tokenize(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(put(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(putl(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(printf(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(printNumber(V1, V2, V3, V4), V) :-
	(V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(open(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(close(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(getStream(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(flushOutput(V1), V1).
usesVar(clearIOError(V1), V1).
usesVar(setInput(V1), V1).
usesVar(setOutput(V1), V1).
usesVar(currentInput(V1), V1).
usesVar(currentOutput(V1), V1).
%usesVar(exect(V1), V1).
usesVar(execs(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(throw(V1), V1).
usesVar(symbol(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(copy(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(uncopy(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(erase(V1), V1).
usesVar(abolishCode(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(makeBMT(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(linkBMT(_, V1, V2), V) :- (V = V1 ; V = V2).
usesVar(instance(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(proplist(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(replacn(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(replacn_var(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(setarg(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(setarg_var(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(aref(V1, V2, V3, _), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(aset(V1, V2, V3, V4, _), V) :- (V = V1 ; V = V2 ; V = V3; V = V4).
usesVar(oncut(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(iseq(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(copyVariablesToTopOfHeap(V1), V1).
usesVar(cutd(V1), V1).
usesVar(softCut(V1), V1).
usesVar(flags(V1), V1).
usesVar(defined(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(fvar(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(mark(V1), V1).
usesVar(fork(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(fork(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(load(V1), V1).
usesVar(iload(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(fload(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3; V = V4).
usesVar(spy(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(nospy(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(syscall(_, V2, V3, V4, V5), V) :- (V = V2 ; V = V3 ; V = V4 ; V = V5).
usesVar(exit(V1), V1).
usesVar(pstot(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(simc_hash(V1, V2, V3, V4), V) :- (V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(simc_query(V1, V2, V3, V4), V) :-
	(V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(simc_next(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(simc_end(V1), V1).
usesVar(simc_assert(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(simc_delete(V1), V1).
usesVar(simc_abort(V1), V1).
usesVar(dsimc_open(V1, V2, V3, V4), V) :-
	(V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(dsimc_free(V1), V1).
usesVar(dsimc_cv(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(dsimc_sfbquery(V1, V2, V3, V4, V5), V) :-
	(V = V1 ; V = V2 ; V = V3 ; V = V4 ; V = V5).
usesVar(dsimc_query(V1, V2, V3, V4), V) :-
	(V = V1 ; V = V2 ; V = V3 ; V = V4).
usesVar(sql_query(V1, V2, V3), V) :- (V = V1 ; V = V2 ; V = V3).
usesVar(sql_next(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(sql_end(V1), V1).
usesVar(sql_modify(V1, V2), V) :- (V = V1 ; V = V2).
usesVar(sql_abort(V1), V1).
usesVar(temp(VR), temp(VR)).
usesVar(perm(VR), perm(VR)).
usesVar(builtin(_, _, Args), Var) :-
	usesVarL(Args, Var).
usesVar(list(Args), Var) :-
	usesVarL(Args, Var).
usesVar(struct(_.Args), Var) :-
	usesVarL(Args, Var).

:- usesVarL(X, _) when X.
usesVarL(Arg._, Var) :-
	usesVar(Arg, Var).
usesVarL(_.Args, Var) :-
	usesVarL(Args, Var).

%	Code uses permanent variables.
usesY(Code) :-
	usesVar(Code, perm(_)),
	!.
usesY(call(_, _, _, _)).

isDeterminedVar(temp(_)).
isDeterminedVar(perm(_)).

%	Prelude instructions that are definite branches, and are small.
isSmallBranchInstruction(j(_)).
isSmallBranchInstruction(sot(_, _, _, _, _)).
%isSmallBranchInstruction(sos(_, _, _)).
%isSmallBranchInstruction(sose(_, _, _)).
%isSmallBranchInstruction(soc(_, _, _)).
%isSmallBranchInstruction(soce(_, _, _)).

polishCode(X, Y) :-
	deleteNops(X, X1),
	assignLabels(X1, Y).

polishCode(X, Pred, Arity, Branch, Y) :-
	polishCode(X, X1),
	(	(	Branch = sot(Var, LV, LC, LL, LS),
			append(Front, [Exec], X1)
		) ->
		( Exec = exec(Pred, Arity) ->
			append(Front, [execsot(Arity, Var, LV, LC, LL, LS)], Y)
		; Exec = dallExec(Pred, Arity) ->
			append(Front, [dall, execsot(Arity, Var, LV, LC, LL, LS)], Y)
		;	Y = X1
		)
	;	Y = X1
	).

:- deleteNops(X, _) when X.
deleteNops([], []).
deleteNops(nop.XT, Y) :-
	!,
	deleteNops(XT, Y).
deleteNops(X.XT, X.YT) :-
	deleteNops(XT, YT).

:- assignLabels(X, _) when X.
assignLabels([], []).
assignLabels($(L).$(L).XT, Y) :-
	!,
	assignLabels($(L).XT, Y).
assignLabels($(L).j(L).XT, Y) :-
	!,
	assignLabels(j(L).XT, Y).
assignLabels(X.XT, X.YT) :-
	( X = $(Label), var(Label) ->
		makeLabel(Label)
	),
	assignLabels(XT, YT).
