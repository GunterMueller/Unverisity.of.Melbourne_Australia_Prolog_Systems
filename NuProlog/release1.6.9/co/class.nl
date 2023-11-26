/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- classification database.

builtin(!).
builtin($label(_)).
builtin($cutd(_)).
builtin($softCut).
builtin($softCut(_)).
%builtin($arithCut).				% A cut that might have been.
%builtin($condition).			% A simple condition follows.
builtin(caramel(_)) :- fail.	% Internal builtin.
builtin(fail).
builtin(true).
%builtin(call(_)).
builtin($funcallBuiltin(_)).
builtin($applyBuiltin(_)).
%builtin($exect(_)).
builtin($execs(_, _)).
builtin(throw(_)).
%	builtin(repeat).			% Not worth the special case code.
builtin(var(_)).
builtin($del(_)).
builtin(nonvar(_)).
builtin($icn(_)).
builtin($ucn(_)).
builtin(compound(_)).
builtin(const(_)).
builtin(cons(_)).
builtin($string(_)).
builtin(term(_)).
builtin($struct(_)).
builtin($bmv(_)).
builtin(atom(_)).
builtin(integer(_)).
builtin(atomic(_)).
builtin($block(_)).
builtin($i32(_)).
builtin(number(_)).
builtin(float(_)).
builtin(_ = _).
builtin($is(_, _)).
%builtin($arithP(_, _, _)).
builtin($arithPred(_)).
builtin(_ == _).
builtin(_ \== _).
builtin($eRefBuiltin(_, _)).
builtin($mkObjBuiltin(_, _, _, _)).
builtin($compareBuiltin(_, _, _)).
builtin($sortBuiltin(_, _, _)).
builtin($symbolBuiltin(_, _)).
builtin($copyBuiltin(_, _, _, _)).
builtin($uncopyBuiltin(_, _, _)).
builtin(erase(_)).
builtin($abolishCode(_, _)).
builtin($makeBMTBuiltin(_, _, _, _)).
builtin($linkBMT(_, _, _)).
builtin($instanceBuiltin(_, _, _)).
builtin($proplistBuiltin(_, _, _)).
builtin($replacn(_, _, _)).
builtin(setarg(_, _, _)).
builtin($oncut(_, _)).
builtin($is_eqBuiltin(_, _, _)).
builtin('$copyVariablesToTopOfHeap:-)'(_)).
builtin($error(_, _)).
builtin($display(_)).
builtin($get0Builtin(_)).
builtin($getBuiltin(_)).
builtin(put(_)).
builtin($get0Builtin(_, _)).
builtin($getBuiltin(_, _)).
builtin($getTokenBuiltin(_, _, _)).
builtin($tokenizeBuiltin(_, _, _, _)).
builtin(put(_, _)).
builtin(putl(_, _)).
builtin($sprtBuiltin(_, _, _)).
builtin($printf(_, _, _)).
builtin($printNumberBuiltin(_, _, _, _)).
builtin($openBuiltin(_, _, _)).
builtin($closeBuiltin(_, _)).
builtin($getStreamBuiltin(_, _)).
builtin(flushOutput(_)).
builtin(clearIOError(_)).
builtin(setInput(_)).
builtin(setOutput(_)).
builtin($currentInputBuiltin(_)).
builtin($currentOutputBuiltin(_)).
builtin(_ =.. _).
builtin(occurs(_, _)).
builtin(name(_, _)).
builtin($listToString(_, _)).
builtin(plus(_, _, _)).
builtin($definedBuiltin(_, _, _, _)).
builtin($predicateAritiesBuiltin(_, _)).
builtin($flagsBuiltin(_)).
builtin(functor(_, _, _)).
builtin($argBuiltin(_, _, _)).
builtin($arefBuiltin(_, _, _, _)).
builtin($asetBuiltin(_, _, _, _, _)).
builtin(ground(_)).
builtin($fvarBuiltin(_, _)).
builtin($forkBuiltin(_, _, _)).
builtin($forkBuiltin(_, _, _, _)).
builtin($load(_)).
builtin($iload(_, _, _)).
builtin($floadBuiltin(_, _, _, _)).
builtin($syscallBuiltin(_, _, _, _, _)).
builtin($spy(_, _)).
builtin($nospy(_, _)).
builtin(abort).
builtin(exit(_)).
%	Database routines
builtin($pstotBuiltin(_, _)).
builtin($simc_hashBuiltin(_, _, _, _)).
builtin($simc_queryBuiltin(_, _, _, _)).
builtin($simc_nextBuiltin(_, _)).
builtin($simc_end(_)).
builtin($simc_assert(_, _)).
builtin($simc_delete(_)).
builtin($simc_abort(_)).
builtin($dsimc_openBuiltin(_, _, _, _)).
builtin($dsimc_free(_)).
builtin($dsimc_cvBuiltin(_, _, _)).
builtin($dsimc_sfbqueryBuiltin(_, _, _, _, _)).
builtin($dsimc_queryBuiltin(_, _, _, _)).
builtin($sql_queryBuiltin(_, _, _)).
builtin($sql_nextBuiltin(_, _)).
builtin($sql_end(_)).
builtin($sql_modify(_, _)).
builtin($sql_abort(_)).

%fence(call(_)).
fence(fence).

%	Select certain arguments of certain builtins to be put in temporaries
%	before the builtin is called.  Arguments marked with a - left alone.
% ?- complexBuiltin(X, Y, _) when X and Y.					% Really index
complexBuiltin($softCut, 1, [+]).			% No permanent var form available
complexBuiltin($display, 1, [+]).
complexBuiltin($get0Builtin, 1, [-]).
complexBuiltin($getBuiltin, 1, [-]).
complexBuiltin(put, 1, [+]).
complexBuiltin($sprtBuiltin, 3, [+, +, -]).
complexBuiltin($printf, 3, [+, +, +]).
complexBuiltin($printNumberBuiltin, 4, [+, +, +, -]).
%complexBuiltin($exect, 1, [+]).
complexBuiltin($execs, 2, [+, +]).
complexBuiltin(throw, 1, [+]).
complexBuiltin($get0Builtin, 2, [+, -]).
complexBuiltin($getBuiltin, 2, [+, -]).
complexBuiltin($getTokenBuiltin, 3, [+, -, -]).
complexBuiltin($tokenizeBuiltin, 4, [+, -, -, -]).
complexBuiltin(put, 2, [+, +]).
complexBuiltin(putl, 2, [+, +]).
complexBuiltin($openBuiltin, 3, [+, +, -]).
complexBuiltin($closeBuiltin, 2, [+, -]).
complexBuiltin($getStreamBuiltin, 2, [+, -]).
complexBuiltin(flushOutput, 1, [+]).
complexBuiltin(clearIOError, 1, [+]).
complexBuiltin(setInput, 1, [+]).
complexBuiltin(setOutput, 1, [+]).
complexBuiltin($currentInputBuiltin, 1, [-]).
complexBuiltin($currentOutputBuiltin, 1, [-]).
complexBuiltin(=.., 2, [+, +]).
complexBuiltin(==, 2, [+, +]).
complexBuiltin(\==, 2, [+, +]).
complexBuiltin($eRefBuiltin, 2, [+, -]).
complexBuiltin($mkObjBuiltin, 4, [+, +, +, -]).
complexBuiltin($compareBuiltin, 3, [-, +, +]).
complexBuiltin($sortBuiltin, 3, [+, +, -]).
complexBuiltin($symbolBuiltin, 2, [+, -]).
complexBuiltin(occurs, 2, [+, +]).
complexBuiltin(name, 2, [+, +]).
complexBuiltin($listToString, 2, [+, +]).
complexBuiltin($copyBuiltin, 4, [+, +, -, -]).
complexBuiltin($uncopyBuiltin, 3, [+, +, -]).
complexBuiltin(erase, 1, [+]).
complexBuiltin($abolishCode, 2, [+, +]).
complexBuiltin($makeBMTBuiltin, 4, [+, +, -, -]).
complexBuiltin($linkBMT, 3, [-, +, +]).
complexBuiltin($instanceBuiltin, 3, [+, -, -]).
complexBuiltin($proplistBuiltin, 3, [+, +, -]).
complexBuiltin($replacn, 3, [+, +, +]).
complexBuiltin(setarg, 3, [+, +, +]).
complexBuiltin($oncut, 2, [+, +]).
complexBuiltin($is_eqBuiltin, 3, [+, +, -]).
complexBuiltin('$copyVariablesToTopOfHeap:-)', 1, [+]).
complexBuiltin(plus, 3, [+, +, +]).
complexBuiltin($definedBuiltin, 4, [+, +, -, -]).
complexBuiltin($predicateAritiesBuiltin, 2, [+, -]).
complexBuiltin($flagsBuiltin, 1, [-]).
complexBuiltin(functor, 3, [+, +, +]).
complexBuiltin($argBuiltin, 3, [+, +, -]).
complexBuiltin($arefBuiltin, 4, [+, +, -, -]).
complexBuiltin($asetBuiltin, 5, [+, +, +, -, -]).
complexBuiltin(ground, 1, [+]).
complexBuiltin($fvarBuiltin, 2, [+, -]).			% Can return local var
complexBuiltin($forkBuiltin, 3, [-, -, -]).
complexBuiltin($forkBuiltin, 4, [-, -, -, -]).
complexBuiltin($load, 1, [+]).
complexBuiltin($iload, 3, [+, +, +]).
complexBuiltin($floadBuiltin, 4, [+, +, +, -]).
complexBuiltin($syscallBuiltin, 5, [-, +, +, +, -]).
complexBuiltin($spy, 2, [+, +]).
complexBuiltin($nospy, 2, [+, +]).
complexBuiltin(exit, 1, [+]).
%	Database routines
complexBuiltin($pstotBuiltin, 2, [+, -]).
complexBuiltin($simc_hashBuiltin, 4, [+, +, +, -]).
complexBuiltin($simc_queryBuiltin, 4, [+, +, +, -]).
complexBuiltin($simc_nextBuiltin, 2, [+, -]).
complexBuiltin($simc_end, 1, [+]).
complexBuiltin($simc_assert, 2, [+, +]).
complexBuiltin($simc_delete, 1, [+]).
complexBuiltin($simc_abort, 1, [+]).
complexBuiltin($dsimc_openBuiltin, 4, [+, +, +, -]).
complexBuiltin($dsimc_free, 1, [+]).
complexBuiltin($dsimc_cvBuiltin, 3, [+, +, -]).
complexBuiltin($dsimc_sfbqueryBuiltin, 5, [+, +, +, +, -]).
complexBuiltin($dsimc_queryBuiltin, 4, [+, +, +, -]).
complexBuiltin($sql_queryBuiltin, 3, [+, +, -]).
complexBuiltin($sql_nextBuiltin, 2, [+, -]).
complexBuiltin($sql_end, 1, [+]).
complexBuiltin($sql_modify, 2, [+, +]).
complexBuiltin($sql_abort, 1, [+]).

complexBuiltins(conj(C), conj(D), N0, N) :-
	complexBuiltinsL(C, D, [], N0, N).
complexBuiltins((conj(C1) -> conj(C2)), (conj(D1) -> conj(D2)), N0, N) :-
	complexBuiltinsL(C1, D1, [], N0, N1),
	complexBuiltinsL(C2, D2, [], N1, N).
complexBuiltins(disj(NSaved, NTemps, C), disj(NSaved, NTemps, D), N0, N) :-
	complexBuiltinsL(C, D, [], N0, N).

complexBuiltins(builtin(Pred, Arity, Args), D0, D, N0, N) :-
	complexBuiltin(Pred, Arity, Modes),
	!,
	putModedArgsInTemporaries(Modes, Args, NewArgs,
		D0, builtin(Pred, Arity, NewArgs).D,
		N0, N).
complexBuiltins(X, Y.D, D, N0, N) :-
	complexBuiltins(X, Y, N0, N),
	!.
complexBuiltins(X, X.D, D, N, N).

?- complexBuiltinsL(X, _, _, _, _) when X.
complexBuiltinsL([], CE, CE, N, N).
complexBuiltinsL(T.TT, C0, C, N0, N) :-
	complexBuiltins(T, C0, C1, N0, N1),
	complexBuiltinsL(TT, C1, C, N1, N).

?- putModedArgsInTemporaries(X, Y, Z, _, _, _, _) when X or Y or Z.
putModedArgsInTemporaries([], [], [], DE, DE, N, N).
putModedArgsInTemporaries(M.ME, Arg.Args, NewArg.NewArgs, D, DE, N, N2) :-
	putModedArgInTemporary(M, Arg, NewArg, D, DE1, N, N1),
	putModedArgsInTemporaries(ME, Args, NewArgs, DE1, DE, N1, N2).

putModedArgInTemporary(-, Arg, Arg, DE, DE, N, N).
putModedArgInTemporary(+, Arg, NewArg, D, DE, N, N1) :-
	putArgInTemporary(Arg, NewArg, D, DE, N, N1).

putArgInTemporary(TempVar, TempVar, init([vr(VN, VR, temp, _)]).DE, DE, N, N) :-
	TempVar = vr(VN, VR, temp, _),
	!.
putArgInTemporary(PermVar, vr(N, VR2, temp, _),
		[(vr(N, VR2, temp, _) = PermVar)|DE], DE,
		N, N1) :-
	PermVar = vr(VN1, VR1, perm, _),
	!,
	N1 is N + 1.
putArgInTemporary(const(Thing), vr(N, VR, temp, _),
		(vr(N, VR, temp, _) = const(Thing)).DE, DE, N, N1) :-
	atomic(Thing),
	!,
	N1 is N + 1.
putArgInTemporary(GroundTerm, vr(N, VR, temp, _),
		(vr(N, VR, temp, _) = ground(Tag, Label, GroundTerm)).DE, DE, N, N1) :-
	ground(GroundTerm),
	!,
	tagOfTerm(GroundTerm, Tag),
	makeLabel(Label),
	asserta(groundTerm(Tag, Label, GroundTerm)),
	N1 is N + 1.
putArgInTemporary(list(Args), vr(N, VR, temp, _), D, DE, N, N2) :-
	!,
	N1 is N + 1,
	putArgsForUnification(Args, NewArgs,
		D, (vr(N, VR, temp, _) = list(NewArgs)).DE, N1, N2).
putArgInTemporary(struct(Functor.Args), vr(N, VR, temp, _), D, DE, N, N2) :-
	!,
	N1 is N + 1,
	putArgsForUnification(Args, NewArgs,
		D, (vr(N, VR, temp, _) = struct(Functor.NewArgs)).DE, N1, N2).

putArgsForUnification([], [], DE, DE, N, N).
putArgsForUnification(Arg.Args, NewArg.NewArgs, D, DE, N, N2) :-
	putArgForUnification(Arg, NewArg, D, DE1, N, N1),
	putArgsForUnification(Args, NewArgs, DE1, DE, N1, N2).

putArgForUnification(const(Arg), const(Arg), DE, DE, N, N) :-
	atomic(Arg),
	!.
putArgForUnification(Arg, Arg, DE, DE, N, N) :-
	Arg = vr(_, _, _, _),
	!.
putArgForUnification(Arg, ground(Tag, Label, Arg), DE, DE, N, N) :-
	ground(Arg),
	!,
	tagOfTerm(Arg, Tag),
	makeLabel(Label),
	asserta(groundTerm(Tag, Label, Arg)).
putArgForUnification(Arg, NewArg, D, DE, N, N1) :-
	putArgInTemporary(Arg, NewArg, D, DE, N, N1).

$arithConst(pi).
$arithConst(maxint).
$arithConst(minint).

$arithNumber(+, 2, 0, 0).
$arithNumber(-, 2, 1, 38).
$arithNumber(*, 2, 2, 2).
$arithNumber(/, 2, 3, 39).
$arithNumber(//, 2, 4, 40).
$arithNumber(mod, 2, 5, 41).
$arithNumber(/\, 2, 6, 6).
$arithNumber(\/, 2, 7, 7).
$arithNumber(^, 2, 8, 8).
$arithNumber(<<, 2, 9, 42).
$arithNumber(>>, 2, 10, 43).
$arithNumber(and, 2, 11, 11).
$arithNumber(or, 2, 12, 12).
$arithNumber(not, 1, 13, 13).
$arithNumber(-, 1, 14, 14).
$arithNumber(\, 1, 15, 15).
$arithNumber(sin, 1, 16, 16).
$arithNumber(cos, 1, 17, 17).
$arithNumber(tan, 1, 18, 18).
$arithNumber(asin, 1, 19, 19).
$arithNumber(acos, 1, 20, 20).
$arithNumber(atan, 1, 21, 21).
$arithNumber(atan2, 2, 22, 44).
$arithNumber(exp, 1, 23, 23).
$arithNumber(log, 1, 24, 24).
$arithNumber(log10, 1, 25, 25).
$arithNumber(**, 2, 26, 45).
$arithNumber(sqrt, 1, 27, 27).
$arithNumber(integer, 1, 28, 28).
$arithNumber(float, 1, 29, 39).
$arithNumber(round, 1, 30, 30).
$arithNumber(<, 2, 31, 33).
$arithNumber(=<, 2, 32, 34).
$arithNumber(>, 2, 33, 31).
$arithNumber(>=, 2, 34, 32).
$arithNumber(=:=, 2, 35, 35).
$arithNumber(=\=, 2, 36, 36).
$arithNumber(+, 1, 37, 37).
$arithNumber('+1', 1, 46, 46).
$arithNumber('-1', 1, 47, 47).
$arithNumber(integer8At, 1, 48, 48).
$arithNumber(integer_8_at, 1, 48, 48).
$arithNumber(unsigned8At, 1, 49, 49).
$arithNumber(unsigned_8_at, 1, 49, 49).
$arithNumber(integer16At, 1, 50, 50).
$arithNumber(integer_16_at, 1, 50, 50).
$arithNumber(unsigned16At, 1, 51, 51).
$arithNumber(unsigned_16_at, 1, 51, 51).
$arithNumber(integerAt, 1, 52, 52).
$arithNumber(integer_at, 1, 52, 52).
$arithNumber(addressAt, 1, 53, 53).
$arithNumber(address_at, 1, 53, 53).
$arithNumber(singleAt, 1, 54, 54).
$arithNumber(single_at, 1, 54, 54).
$arithNumber(doubleAt, 1, 55, 55).
$arithNumber(double_at, 1, 55, 55).

$arithPredNumber(and, 2, 0, 0).
$arithPredNumber(or, 2, 1, 1).
$arithPredNumber(<, 2, 2, 4).
$arithPredNumber(=<, 2, 3, 5).
$arithPredNumber(>, 2, 4, 2).
$arithPredNumber(>=, 2, 5, 3).
$arithPredNumber(=:=, 2, 6, 6).
$arithPredNumber(=\=, 2, 7, 7).

$syscallNumber(access,			 0).
$syscallNumber(chdir,			 1).
$syscallNumber(chmod,			 2).
$syscallNumber(environment,		 3).
$syscallNumber(getegid,			 4).
$syscallNumber(getgid,			 5).
$syscallNumber(getgroups,		 6).
$syscallNumber(getlogin,		 7).
$syscallNumber(getpid,			 8).
$syscallNumber(getppid,			 9).
$syscallNumber(getpwuid,		10).
$syscallNumber(getpwnam,		11).
$syscallNumber(getuid,			12).
$syscallNumber(geteuid,			13).
$syscallNumber(getwd,			14).
$syscallNumber(hostname,		15).
$syscallNumber(kill,			16).
$syscallNumber(link,			17).
$syscallNumber(mkdir,			18).
$syscallNumber(random,			19).
$syscallNumber(rename,			20).
$syscallNumber(rmdir,			21).
$syscallNumber(sleep,			22).
$syscallNumber(stat,			23).
$syscallNumber(time,			24).
$syscallNumber(truncate,		25).
$syscallNumber(umask,			26).
$syscallNumber(unlink,			27).
$syscallNumber(wait,			28).
$syscallNumber(readdir,			29).
$syscallNumber(fork,			30).
$syscallNumber(system,			31).
$syscallNumber(exec,			32).
$syscallNumber(statistics,		33).
$syscallNumber(fseek,			34).
$syscallNumber(ftell,			35).
$syscallNumber(fesq,			36).
$syscallNumber(signal,			37).
