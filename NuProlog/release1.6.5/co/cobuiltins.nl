/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- code generator for builtins.

codegenBuiltin(Pred, Arity, Args, Code, CodeEnd) :-
	codegenBuiltin2(Pred, Arity, Args, Code, CodeEnd),
	!.
codegenBuiltin(Functor, _, Args, CE, CE) :-
	Builtin =.. (Functor.Args),
	throwCompilerError(
		format(user_error,
			"~NError -- Uncodeable builtin: (~w).~n", [Builtin])).

codegenBuiltin2(var, 1, [Arg], C, CE) :- codegenType(Arg, var, C, CE).
codegenBuiltin2($ref, 1, [Arg], C, CE) :- codegenType(Arg, $ref, C, CE).
codegenBuiltin2($del, 1, [Arg], C, CE) :- codegenType(Arg, $del, C, CE).
codegenBuiltin2(nonvar, 1, [Arg], C, CE) :- codegenType(Arg, nonvar, C, CE).
codegenBuiltin2($icn, 1, [Arg], C, CE) :- codegenType(Arg, $icn, C, CE).
codegenBuiltin2($ucn, 1, [Arg], C, CE) :- codegenType(Arg, $ucn, C, CE).
codegenBuiltin2(compound, 1, [Arg], C, CE) :- codegenType(Arg, compound, C, CE).
codegenBuiltin2(const, 1, [Arg], C, CE) :- codegenType(Arg, const, C, CE).
codegenBuiltin2(cons, 1, [Arg], C, CE) :- codegenType(Arg, cons, C, CE).
codegenBuiltin2($string, 1, [Arg], C, CE) :- codegenType(Arg, $string, C, CE).
codegenBuiltin2(term, 1, [Arg], C, CE) :- codegenType(Arg, term, C, CE).
codegenBuiltin2($struct, 1, [Arg], C, CE) :- codegenType(Arg, $struct, C, CE).
codegenBuiltin2($bmv, 1, [Arg], C, CE) :- codegenType(Arg, $bmv, C, CE).
codegenBuiltin2(integer, 1, [Arg], C, CE) :- codegenCType(Arg, integer, C, CE).
codegenBuiltin2(atom, 1, [Arg], C, CE) :- codegenCType(Arg, atom, C, CE).
codegenBuiltin2(atomic, 1, [Arg], C, CE) :- codegenCType(Arg, atomic, C, CE).
codegenBuiltin2($block, 1, [Arg], C, CE) :- codegenCType(Arg, $block, C, CE).
codegenBuiltin2($i32, 1, [Arg], C, CE) :- codegenCType(Arg, $i32, C, CE).
codegenBuiltin2(float, 1, [Arg], C, CE) :- codegenCType(Arg, float, C, CE).
codegenBuiltin2(number, 1, [Arg], C, CE) :- codegenCType(Arg, number, C, CE).
codegenBuiltin2(fail, 0, [], fail.CE, CE).
codegenBuiltin2(true, 0, [], CE, CE).
codegenBuiltin2(!, 0, [], ! .CE, CE).
codegenBuiltin2(caramel, 1, [vr(_, VR, VT, var)], init(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenBuiltin2(caramel, 1, [vr(_, VR, perm, _)], use(perm(VR)).CE, CE).
codegenBuiltin2(caramel, 1, [_], CE, CE).
codegenBuiltin2($label, 1, [vr(_, VR, VT, var)], label(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenBuiltin2($cutd, 1, [vr(_, VR, VT, val(_))], cutd(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenBuiltin2($softCut, 1, [vr(_, VR, VT, val(_))], softCut(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
%%	ArithCut and condition are just markers.  Code is never generated for them.
%codegenBuiltin2($arithCut, 0, [], arithCut.CE, CE).
%codegenBuiltin2($condition, 0, [], condition.CE, CE).
codegenBuiltin2($flagsBuiltin, 1, [vr(_, VR, VT, var)], flags(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenBuiltin2($definedBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, _),
			vr(_, VR4, temp, _)],
		defined(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($predicateAritiesBuiltin, 2,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, var)],
		predicateArities(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(ground, 1, [vr(_, VR1, temp, val(_))],
		[	fvar(temp(VR1), temp(255)),
			type(temp(255), TypeCode)
		|	CE], CE) :-
	$type(nonvar, TypeCode).
codegenBuiltin2($fvarBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		fvar(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($error, 2, [const(Number), const(Action)],
		error(Number, Action).CE, CE) :-
	integer(Number), integer(Action),
	0 =< Number and Number < 256,
	0 =< Action and Action < 256.
codegenBuiltin2($display, 1, [vr(_, VR, VT, _)], display(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenBuiltin2((==), 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		id(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2((\==), 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		notid(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($eRefBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		eRef(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($mkObjBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		mkObj(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($compareBuiltin, 3,
		[	vr(_, VR1, temp, var),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_))],
		compare(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($sortBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		sort(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($is, 2, [Arg2, Arg1], C, CE) :-
%	Order reversed for proper temporary allocation.
	codegenIs(Arg1, Arg2, C, CE).
%codegenBuiltin2($arithP, 3, [Op, Arg1, Arg2], arith.C, CE) :-
%	codegenArithPred(Op, Arg1, Arg2, C, CE).
codegenBuiltin2($arithPred, 1, [struct([Op, Arg1, Arg2])], arith.C, CE) :-
	codegenArithPred(Op, Arg1, Arg2, C, CE).
codegenBuiltin2(occurs, 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		occurs(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(name, 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		name(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($listToString, 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		ltos(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2((=..), 2,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _)],
		univ(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(functor, 3,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _), vr(_, VR3, temp, _)],
		functor(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2(plus, 3,
		[vr(_, VR1, temp, _), vr(_, VR2, temp, _), vr(_, VR3, temp, _)],
		plus(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($argBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		arg(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($arefBuiltin, 4,
		[vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)), vr(_, VR3, temp, var), const(N)],
		aref(temp(VR1), temp(VR2), temp(VR3), N).CE, CE) :-
	integer(N), 0 =< N and N =< 1.
codegenBuiltin2($asetBuiltin, 5,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)), vr(_, VR4, temp, var), const(N)],
		aset(temp(VR1), temp(VR2), temp(VR3), temp(VR4), N).CE, CE) :-
	integer(N), 0 =< N and N =< 1.
codegenBuiltin2($get0Builtin, 1,
		[vr(_, VR1, temp, var)],
		get0(temp(VR1)).CE, CE).
codegenBuiltin2($get0Builtin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		get0(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($getBuiltin, 1,
		[vr(_, VR1, temp, var)],
		get(temp(VR1)).CE, CE).
codegenBuiltin2($getBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		get(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($sprtBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		sprt(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($getTokenBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, var),
			vr(_, VR3, temp, var)],
		getToken(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($tokenizeBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, var),
			vr(_, VR3, temp, var),
			vr(_, VR4, temp, var)],
		tokenize(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2(put, 1, [vr(_, VR1, temp, val(_))], put(temp(VR1)).CE, CE).
codegenBuiltin2(put, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		put(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(putl, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		putl(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($printf, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_))],
		printf(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($printNumberBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		printNumber(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($openBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		open(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($closeBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		close(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($getStreamBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		getStream(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(flushOutput, 1,
		[vr(_, VR1, temp, val(_))],
		flushOutput(temp(VR1)).CE, CE).
codegenBuiltin2(clearIOError, 1,
		[vr(_, VR1, temp, val(_))],
		clearIOError(temp(VR1)).CE, CE).
codegenBuiltin2(setInput, 1,
		[vr(_, VR1, temp, val(_))],
		setInput(temp(VR1)).CE, CE).
codegenBuiltin2(setOutput, 1,
		[vr(_, VR1, temp, val(_))],
		setOutput(temp(VR1)).CE, CE).
codegenBuiltin2($currentInputBuiltin, 1,
		[vr(_, VR1, temp, var)],
		currentInput(temp(VR1)).CE, CE).
codegenBuiltin2($currentOutputBuiltin, 1,
		[vr(_, VR1, temp, var)],
		currentOutput(temp(VR1)).CE, CE).
codegenBuiltin2($funcallBuiltin, 1, [const(N)], funcall(N).CE, CE).
codegenBuiltin2($applyBuiltin, 1, [const(N)], apply(N).CE, CE).
%codegenBuiltin2($exect, 1,
%		[vr(_, VR1, temp, val(_))],
%		exect(temp(VR1)).CE, CE).
codegenBuiltin2($execs, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		execs(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2(throw, 1,
		[vr(_, VR1, temp, val(_))],
		throw(temp(VR1)).CE, CE).
codegenBuiltin2($symbolBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		symbol(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($copyBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var),
			vr(_, VR4, temp, var)],
		copy(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($uncopyBuiltin, 3,
		[vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)), vr(_, VR3, temp, var)],
		uncopy(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2(erase, 1, [vr(_, VR1, temp, val(_))],
		erase(temp(VR1)).CE, CE).
codegenBuiltin2($abolishCode, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		abolishCode(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($makeBMTBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var),
			vr(_, VR4, temp, var)],
		makeBMT(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($linkBMT, 3,
		[	const(N),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_))],
		linkBMT(N, temp(VR2), temp(VR3)).CE, CE) :-
	integer(N), 0 =< N and N =< 1.
codegenBuiltin2($instanceBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, var),
			vr(_, VR3, temp, var)],
		instance(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($proplistBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		proplist(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($replacn, 3,
		[vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)), vr(_, VR3, temp, val(_))],
		replacn(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2(setarg, 3,
		[vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)), vr(_, VR3, temp, val(_))],
		setarg(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($oncut, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		oncut(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($is_eqBuiltin, 3,
		[vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)), vr(_, VR3, temp, var)],
		iseq(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2('$copyVariablesToTopOfHeap:-)', 1,
		[vr(_, VR1, temp, val(_))],
		copyVariablesToTopOfHeap(temp(VR1)).CE, CE).
codegenBuiltin2($forkBuiltin, 3,
		[vr(_, VR1, temp, var), vr(_, VR2, temp, var), vr(_, VR3, temp, var)],
		fork(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($forkBuiltin, 4,
		[	vr(_, VR1, temp, var),
			vr(_, VR2, temp, var),
			vr(_, VR3, temp, var),
			vr(_, VR4, temp, var)],
		fork(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($load, 1, [vr(_, VR1, temp, val(_))],
		load(temp(VR1)).CE, CE).
codegenBuiltin2($iload, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_))],
		iload(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($floadBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		fload(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($spy, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		spy(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($nospy, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		nospy(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($syscallBuiltin, 5,
		[	const(SC),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, val(_)),
			vr(_, VR5, temp, var)],
		syscall(SCN, temp(VR2), temp(VR3), temp(VR4), temp(VR5)).CE, CE) :-
	$syscallNumber(SC, SCN).
codegenBuiltin2(abort, 0, [], abort.CE, CE).
codegenBuiltin2(exit, 1, [vr(_, VR1, temp, val(_))],
		exit(temp(VR1)).CE, CE).
codegenBuiltin2($pstotBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		pstot(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($simc_hashBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		simc_hash(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($simc_queryBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		simc_query(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($simc_nextBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		simc_next(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($simc_end, 1, [vr(_, VR1, temp, val(_))],
		simc_end(temp(VR1)).CE, CE).
codegenBuiltin2($simc_assert, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		simc_assert(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($simc_delete, 1, [vr(_, VR1, temp, val(_))],
		simc_delete(temp(VR1)).CE, CE).
codegenBuiltin2($simc_abort, 1, [vr(_, VR1, temp, val(_))],
		simc_abort(temp(VR1)).CE, CE).
codegenBuiltin2($dsimc_openBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		dsimc_open(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($dsimc_free, 1, [vr(_, VR1, temp, val(_))],
		dsimc_free(temp(VR1)).CE, CE).
codegenBuiltin2($dsimc_cvBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		dsimc_cv(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($dsimc_sfbqueryBuiltin, 5,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, val(_)),
			vr(_, VR5, temp, var)],
		dsimc_sfbquery(
			temp(VR1), temp(VR2), temp(VR3),
			temp(VR4), temp(VR5)).CE, CE).
codegenBuiltin2($dsimc_queryBuiltin, 4,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, val(_)),
			vr(_, VR4, temp, var)],
		dsimc_query(temp(VR1), temp(VR2), temp(VR3), temp(VR4)).CE, CE).
codegenBuiltin2($sql_queryBuiltin, 3,
		[	vr(_, VR1, temp, val(_)),
			vr(_, VR2, temp, val(_)),
			vr(_, VR3, temp, var)],
		sql_query(temp(VR1), temp(VR2), temp(VR3)).CE, CE).
codegenBuiltin2($sql_nextBuiltin, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, var)],
		sql_next(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($sql_end, 1, [vr(_, VR1, temp, val(_))],
		sql_end(temp(VR1)).CE, CE).
codegenBuiltin2($sql_modify, 2,
		[vr(_, VR1, temp, val(_)), vr(_, VR2, temp, val(_))],
		sql_modify(temp(VR1), temp(VR2)).CE, CE).
codegenBuiltin2($sql_abort, 1, [vr(_, VR1, temp, val(_))],
		sql_abort(temp(VR1)).CE, CE).

codegenVariable(temp, VR, temp(VR)).
codegenVariable(perm, VR, perm(VR)).

%	Occasionally, people (and more frequently program transformation
%	systems) write code like
%		p :- var(X), ....
%	or even
%		p :- var(_), ....
codegenType(Arg, Type, C, CE) :-
	( Arg = vr(_, VR, VT, VI) ->
		$type(Type, TypeCode),
		codegenVariable(VT, VR, Var),
		codegenType2(VI, TypeCode, Var, C, CE)
	; call(Type, Arg) ->
		C = {"Non-logical data-type test applied to non-variable"}.CE
	;	C = {"Non-logical data-type test applied to non-variable"}.fail.CE
	).

codegenType2(uniq, TypeCode, _,
		{"Non-logical data-type test applied to unique variable"}.C, CE) :-
	$type($ref, VarCode),
	( VarCode /\ TypeCode =:= 0 ->
		C = fail.CE
	;	C = CE
	).
codegenType2(var, TypeCode, V,
		{"Non-logical data-type test applied to first instance of variable"}.C,
		CE) :-
	$type($ref, VarCode),
	( VarCode /\ TypeCode =:= 0 ->
		C = fail.CE
	;	C = init(V).CE
	).
codegenType2(val(_), TypeCode, V, type(V, TypeCode).CE, CE).

codegenCType(Arg, Type, C, CE) :-
	( Arg = vr(_, VR, VT, VI) ->
		$ctype(Type, TypeCode),
		codegenVariable(VT, VR, Var),
		codegenCType2(VI, TypeCode, Var, C, CE)
	; call(Type, Arg) ->
		C = {"Data-type test applied to non-variable"}.CE
	;	C = {"Data-type test applied to non-variable"}.fail.CE
	).

codegenCType2(uniq, TypeCode, _,
		[{"Non-logical data-type test applied to unique variable"},
			fail | CE], CE).
codegenCType2(var, TypeCode, _,
		[{"Non-logical data-type test applied to first instance of variable"},
			fail | CE], CE).
codegenCType2(val(_), TypeCode, V, ctype(V, TypeCode).CE, CE).

codegenIs(Arg1, Arg2, arith.C, CE) :-
	codegenIs2(Arg2, C, CE2),
	codegenIs1Value(Arg1, CE2, CE),
	!.
codegenIs(Arg1, Arg2, CE, CE) :-
	throwCompilerError(format(user_error,
		"~NError -- Unable to compile (~w).~n", [Arg1 is Arg2])).

?- codegenIs1Value(const(_), _, _) when ever.
?- codegenIs1Value(vr(_, _, _, VI), _, _) when VI.			% Really index.
codegenIs1Value(const(Number), popv(Number).CE, CE) :-
	number(Number).
codegenIs1Value(vr(_, VR, VT, var), pop(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenIs1Value(vr(_, VR, VT, val(_)), popv(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenIs1Value(vr(_, _, temp, uniq), pop(temp(255)).CE, CE) :-
	/* Note that VT = temp */
	putl(user_error, "%	Warning:\n"),
	putl(user_error, "%	Unique variable on lhs of arithmetic expression.\n").

?- codegenIs2(const(_), _, _) when ever.
?- codegenIs2(vr(_, _, _, VI), _, _) when VI.				% Really index.
?- codegenIs2(string(_), _, _) when ever.
?- codegenIs2(list(_), _, _) when ever.
?- codegenIs2(struct(_), _, _) when ever.
codegenIs2(const(X), C0, C) :-
	( number(X) ->
		C0 = push(X).C
	; atom(X) ->
		codegenIsConstant(X, C0, C)
	; 	fail
	).
codegenIs2(vr(_, _, _, uniq), _, _) :-
	putl(user_error, "%	Unique variable in rhs of arithmetic expression.\n"),
	fail.
codegenIs2(vr(_, VR, VT, var), pushv(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenIs2(vr(_, VR, VT, val(_)), push(Var).CE, CE) :-
	codegenVariable(VT, VR, Var).
codegenIs2(struct(Op.Args), C, CE) :-
	codegenIs2(Op, Args, C, CE).
codegenIs2(list([Arg, []]), C, CE) :-
	codegenIs2(Arg, C, CE).
codegenIs2(string([Arg]), C, CE) :-
	codegenIs2(const(Arg), C, CE).

codegenIs2(Op, [A], C, CE) :-
	!,
	codegenIs2(A, C, CE1),
	once $arithNumber(Op, 1, F1, _),
	CE1 = arithFunc(F1).CE.
%	Clean up some of the grottier redundancies... 
codegenIs2(+, [A, const(1)], C, CE) :-
	!,
	codegenIs2('+1', [A], C, CE).
codegenIs2(+, [const(1), B], C, CE) :-
	!,
	codegenIs2('+1', [B], C, CE).
codegenIs2(-, [A, const(1)], C, CE) :-
	!,
	codegenIs2('-1', [A], C, CE).
codegenIs2(Op, [A, B], C, CE) :-
	(A = const(N), number(N) ->
		X.Y = B.A,
		F = F2
	;	X.Y = A.B,
		F = F1
	),
	once $arithNumber(Op, 2, F1, F2),
	codegenIs2(X, C, CE1),
	codegenIs2(Y, CE1, CE2),
	CE2 = arithFunc(F).CE.

codegenArithPred(Op, A, B, C, CE) :-
	codegenIs2(A, C, CE1),
	codegenIs2(B, CE1, CE2),
	once $arithPredNumber(Op, 2, F1, _),
	CE2 = arithP(F1).CE.

%	Make sure that constant expressions are evaluated when the compiler
%	is run rather than when it is compiled.  Thus the compiler binaries
%	are portable, though other compiled programs are not!
codegenIsConstant(X, push(Y).CE, CE) :-
	$arithConst(X),
	Y is X.

codegenIsLogicalOp(not).
codegenIsLogicalOp(=:=).
codegenIsLogicalOp(=\=).
codegenIsLogicalOp(<).
codegenIsLogicalOp(=<).
codegenIsLogicalOp(>).
codegenIsLogicalOp(>=).
codegenIsLogicalOp(and).
codegenIsLogicalOp(or).
