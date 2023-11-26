/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- code emitter.

?- emitCode(X, _, _) when X.
emitCode([], CodeEnd, CodeEnd).
emitCode(I.IT, Code, CodeEnd) :-
	emitInstruction(I, Code, CE1),
	emitCode(IT, CE1, CodeEnd).

emitInstruction(Inst, Code, CodeEnd) :-
	( emitInst(Inst, Code, CodeEnd) ->
		true
	;	Code = junk(Inst).CodeEnd
	).

emitInst(gvar(temp(VR1), temp(VR2)), emit(gvarx, [VR1, VR2]).C, C).
emitInst(gvara(temp(VR1), temp(VR2)), emit(gvara, [VR1, VR2]).C, C).
emitInst(gvar(perm(VR1), temp(VR2)), emit(gvary, [VR1, VR2]).C, C).
emitInst(glvar(perm(VR1), temp(VR2)), emit(glvary, [VR1, VR2]).C, C).
emitInst(gval(temp(VR1), temp(VR2)), emit(gvalx, [VR1, VR2]).C, C).
emitInst(gval(temp(VR1), perm(VR2)), emit(gvaly, [VR2, VR1]).C, C).
emitInst(gval(perm(VR1), temp(VR2)), emit(gvaly, [VR1, VR2]).C, C).
emitInst(gc(temp(VR1), Const), I.C, C) :-
	( Const == const([]) ->
		I = emit(gnil, [VR1])
	;	I = emit(gc, [VR1], Const)
	).
emitInst(gc(perm(VR1), Const), emit(pvaly, [VR1, 255]).I.C, C) :-
	( Const == const([]) ->
		I = emit(gnil, [255])
	;	I = emit(gc, [255], Const)
	).
emitInst(gs(temp(VR1), Pred/Arity), emit(gs, [VR1], Pred/Arity).C, C).
emitInst(gs(perm(VR1), Pred/Arity),
		emit(pvaly, [VR1, 255]).emit(gs, [255], Pred/Arity).C, C).
emitInst(gs1(temp(VR1), temp(VR2), Pred/Arity),
		emit(gsx1, [VR1, VR2], Pred/Arity).C, C).
emitInst(gs1(perm(VR1), temp(VR2), Pred/Arity),
		emit(pvaly, [VR1, 255]).emit(gsx1, [255, VR2], Pred/Arity).C, C).
emitInst(gs2(temp(VR1), temp(VR2), temp(VR3), Pred/Arity),
		emit(gsx2, [VR1, VR2, VR3], Pred/Arity).C, C).
emitInst(gs2(perm(VR1), temp(VR2), temp(VR3), Pred/Arity),
		emit(pvaly, [VR1, 255]).emit(gsx2, [255, VR2, VR3], Pred/Arity).C,
		C).
emitInst(gl(temp(VR1)), emit(gl, [VR1]).C, C).
emitInst(gl(perm(VR1)), emit(pvaly, [VR1, 255]).emit(gl, [255]).C, C).
emitInst(glv2(temp(VR1), temp(VR2), temp(VR3)),
		emit(glvx2, [VR1, VR2, VR3]).C, C).
emitInst(glv2(perm(VR1), temp(VR2), temp(VR3)),
		emit(pvaly, [VR1, 255]).emit(glvx2, [255, VR2, VR3]).C, C).
emitInst(use(_), C, C).
emitInst(init(temp(VR1)), emit(pvarx, [VR1, VR1]).C, C).
emitInst(init(perm(VR1)), emit(pvary, [VR1, 255]).C, C).
emitInst(pvar(temp(VR1), temp(VR2)), emit(pvarx, [VR1, VR2]).C, C).
emitInst(pvar(perm(VR1), temp(VR2)), emit(pvary, [VR1, VR2]).C, C).
emitInst(pval(perm(VR1), safe, temp(VR2)),
		emit(pvaly, [VR1, VR2]).C, C).
emitInst(pval(perm(VR1), unsafe, temp(VR2)),
		emit(puval, [VR1, VR2]).C, C).
emitInst(pc(Const, temp(VR1)), I.C, C) :-
	( Const == const([]) ->
		I = emit(pnil, [VR1])
	;	I = emit(pc, [VR1], Const)
	).
emitInst(pc(Const, perm(VR1)), I.emit(gvary, [VR1, 255]).C, C) :-
	( Const == const([]) ->
		I = emit(pnil, [255])
	;	I = emit(pc, [255], Const)
	).
emitInst(ps(Pred/Arity, temp(VR1)), emit(ps, [VR1], Pred/Arity).C, C).
emitInst(ps(Pred/Arity, perm(VR1)),
		emit(pvary, [VR1, 255]).emit(gs, [255], Pred/Arity).C, C).
emitInst(pl(temp(VR1)), emit(pl, [VR1]).C, C).
emitInst(pl(perm(VR1)), emit(pvary, [VR1, 255]).emit(gl, [255]).C, C).
emitInst(uvar(temp(VR1)), emit(uvarx, [VR1]).C, C).
emitInst(uvara(temp(VR1)), emit(uvara, [VR1]).C, C).
emitInst(uvar(perm(VR1)), emit(uvary, [VR1]).C, C).
emitInst(uval(temp(VR1), global), emit(uvalx, [VR1]).C, C).
emitInst(uval(perm(VR1), global), emit(uvaly, [VR1]).C, C).
emitInst(uval(temp(VR1), local), emit(ulvx, [VR1]).C, C).
emitInst(uval(perm(VR1), local), emit(ulvy, [VR1]).C, C).
emitInst(uvoid(N), emit(uvoid, [N]).C, C).
emitInst(uc(Const), emit(uc, [], Const).C, C).
emitInst(ul, emit(ul, [255]).C, C).
emitInst(us(Pred/Arity), emit(us, [255], Pred/Arity).C, C).
emitInst(arith, emit(arith, []).C, C).
emitInst(push(temp(VR1)), emit(pushx, [VR1]).C, C).
emitInst(push(perm(VR1)), emit(pushy, [VR1]).C, C).
emitInst(push(Const), emit(I, [], Arg).C, C) :-
	( integer(Const) ->
		I = pushi,
		Arg = integer(Const)
	; float(Const) ->
		I = pushf,
		Arg = const(Const)
	;	fail
	).
emitInst(apush(temp(VR1)), emit(apushx, [VR1]).C, C).
emitInst(apush(perm(VR1)), emit(apushy, [VR1]).C, C).
emitInst(apush(Const), emit(I, [], Arg).C, C) :-
	( integer(Const) ->
		I = apushi,
		Arg = integer(Const)
	; float(Const) ->
		I = apushf,
		Arg = const(Const)
	;	fail
	).
emitInst(pushv(temp(VR1)), emit(pushvx, [VR1]).C, C).
emitInst(pushv(perm(VR1)), emit(pushvy, [VR1]).C, C).
emitInst(pop(temp(VR1)), emit(popx, [VR1]).C, C).
emitInst(pop(perm(VR1)), emit(popy, [VR1]).C, C).
emitInst(popv(temp(VR1)), emit(popvx, [VR1]).C, C).
emitInst(popv(perm(VR1)), emit(popvy, [VR1]).C, C).
emitInst(popv(Const), emit(popc, [], const(Const)).C, C).
emitInst(jpopz(Label), emit(jpopz, [], &(Label)).C, C).
emitInst(arithFunc(Op), emit(afunc, [Op]).C, C).
emitInst(arithP(Op), emit(apred, [Op]).C, C).
emitInst(jArithP(Op, Label), emit(jpred, [Op], &(Label)).C, C).
emitInst(name(temp(VR1), temp(VR2)), emit(name, [VR1, VR2]).C, C).
emitInst(ltos(temp(VR1), temp(VR2)), emit(ltos, [VR1, VR2]).C, C).
emitInst(occurs(temp(VR1), temp(VR2)), emit(occurs, [VR1, VR2]).C, C).
emitInst(univ(temp(VR1), temp(VR2)), emit(univ, [VR1, VR2]).C, C).
emitInst(arg(temp(VR1), temp(VR2), temp(VR3)),
		emit(arg, [VR1, VR2, VR3]).C, C).
emitInst(plus(temp(VR1), temp(VR2), temp(VR3)),
		emit(plus, [VR1, VR2, VR3]).C, C).
emitInst(functor(temp(VR1), temp(VR2), temp(VR3)),
		emit(functor, [VR1, VR2, VR3]).C, C).
emitInst(call(Pred, Arity, PVars, seq),
		emit(call, [PVars, Arity], &(Pred, Arity)).C, C).
emitInst(exec(Pred, Arity), emit(exec, [Arity], &(Pred, Arity)).C, C).
emitInst(dallExec(Pred, Arity), emit(dallexe, [Arity], &(Pred, Arity)).C, C).
emitInst(execsot(Arity, temp(VR2), LV, LC, LL, LS),
		emit(execsot, [Arity, VR2], sotTable(LV, LC, LL, LS)).C, C).
emitInst(pro, emit(pro, []).C, C).
emitInst(dallPro, emit(dallpro, []).C, C).
emitInst(all(N), emit(all, [N]).C, C).
emitInst(dall, emit(dall, []).C, C).
emitInst(t(NTemps, Label), emit(t, [NTemps, 0], &(Label)).C, C).
emitInst(r(NTemps, Label), emit(r, [NTemps, 0], &(Label)).C, C).
emitInst(tr(NTemps, Label), emit(tr, [NTemps, 0], &(Label)).C, C).
emitInst(te(NSaved, NTemps, Label),
		emit(te, [NTemps, 0, NSaved], &(Label)).C, C).
emitInst(re(NTemps, Label), emit(re, [NTemps, 0], &(Label)).C, C).
emitInst(tre(NTemps), emit(tre, [NTemps, 0]).C, C).
emitInst(j(Label), emit(j, [], &(Label)).C, C).
emitInst(jTrue(Label), emit(jtrue, [], &(Label)).C, C).
emitInst(jFail(Label), emit(jfail, [], &(Label)).C, C).
emitInst(jv(temp(VR1), Label), emit(jvx, [VR1], &(Label)).C, C).
emitInst(jnv(temp(VR1), Label), emit(jnvx, [VR1], &(Label)).C, C).
emitInst(jc(temp(VR1), Const, Label), emit(jc, [VR1], Const, &(Label)).C, C).
emitInst(js(temp(VR1), Const, Label), emit(js, [VR1], Const, &(Label)).C, C).
emitInst(sot(temp(VR1), LV, LC, LL, LS),
		emit(sot, [VR1], sotTable(LV, LC, LL, LS)).C, C).
emitInst(sos(temp(VR1), Default, Table),
		emit(sos, [VR1], table(Default, Table)).C, C).
emitInst(sose(temp(VR1), Default, Table),
		emit(sose, [VR1], table(Default, Table)).C, C).
emitInst(soc(temp(VR1), Default, Table),
		emit(soc, [VR1], table(Default, Table)).C, C).
emitInst(soce(temp(VR1), Default, Table),
		emit(soce, [VR1], table(Default, Table)).C, C).
emitInst(mkdel(NTemps, Label), emit(mkdel, [NTemps], &(Label)).C, C).
emitInst(flags(temp(VR1)), emit(flags, [VR1]).C, C).
emitInst(defined(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(defined, [VR1, VR2, VR3, VR4]).C, C).
emitInst(predicateArities(temp(VR1), temp(VR2)),
		emit(arities, [VR1, VR2]).C, C).
emitInst(fvar(temp(VR1), temp(VR2)), emit(fvar, [VR1, VR2]).C, C).
emitInst(delay, emit(delay, []).C, C).
emitInst(mark(temp(VR1)), emit(mark, [VR1]).C, C).
emitInst($(Label), $(Label).C, C).
emitInst(neck(NTemps), emit(neck, [NTemps]).C, C).
emitInst(fail, emit(fail, []).C, C).
emitInst(label(temp(VR1)), emit(labelx, [VR1]).C, C).
emitInst(label(perm(VR1)), emit(labely, [VR1]).C, C).
emitInst(cutd(temp(VR1)), emit(cutx, [VR1]).C, C).
emitInst(cutd(perm(VR1)), emit(cuty, [VR1]).C, C).
emitInst(softCut(temp(VR1)), emit(softcut, [VR1]).C, C).
emitInst(error(Number, Action), emit(error, [Number, Action]).C, C).
emitInst(display(temp(VR1)), emit(dispx, [VR1]).C, C).
emitInst(id(temp(VR1), temp(VR2)), emit(id, [VR1, VR2]).C, C).
emitInst(notid(temp(VR1), temp(VR2)), emit(notid, [VR1, VR2]).C, C).
emitInst(identTest(temp(VR1), temp(VR2)),
		emit(idtest, [VR1, VR2]).C, C).
emitInst(eRef(temp(VR1), temp(VR2)), emit(eref, [VR1, VR2]).C, C).
emitInst(mkObj( temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(mkobj, [VR1, VR2, VR3, VR4]).C, C).
emitInst(compare(temp(VR1), temp(VR2), temp(VR3)),
		emit(compare, [VR1, VR2, VR3]).C, C).
emitInst(sort(temp(VR1), temp(VR2), temp(VR3)),
		emit(sort, [VR1, VR2, VR3]).C, C).
emitInst(get0(temp(VR1)), emit(get, [VR1, 0]).C, C).
emitInst(get(temp(VR1)), emit(get, [VR1, 1]).C, C).
emitInst(put(temp(VR1)), emit(put, [VR1]).C, C).
emitInst(get0(temp(VR1), temp(VR2)), emit(sget, [VR1, VR2, 0]).C, C).
emitInst(get(temp(VR1), temp(VR2)), emit(sget, [VR1, VR2, 1]).C, C).
emitInst(sprt(temp(VR1), temp(VR2), temp(VR3)),
		emit(sprt, [VR1, VR2, VR3]).C, C).
emitInst(getToken(temp(VR1), temp(VR2), temp(VR3)),
		emit(sgettok, [VR1, VR2, VR3]).C, C).
emitInst(tokenize(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(token, [VR1, VR2, VR3, VR4]).C, C).
emitInst(put(temp(VR1), temp(VR2)), emit(sput, [VR1, VR2]).C, C).
emitInst(putl(temp(VR1), temp(VR2)), emit(sputl, [VR1, VR2]).C, C).
emitInst(printf(temp(VR1), temp(VR2), temp(VR3)),
		emit(printf, [VR1, VR2, VR3]).C, C).
emitInst(printNumber( temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(prtnum, [VR1, VR2, VR3, VR4]).C, C).
emitInst(open(temp(VR1), temp(VR2), temp(VR3)),
		emit(open, [VR1, VR2, VR3]).C, C).
emitInst(close(temp(VR1), temp(VR2)), emit(close, [VR1, VR2]).C, C).
emitInst(getStream(temp(VR1), temp(VR2)),
		emit(getstr, [VR1, VR2]).C, C).
emitInst(flushOutput(temp(VR1)), emit(flush, [VR1]).C, C).
emitInst(clearIOError(temp(VR1)), emit(clrerr, [VR1]).C, C).
emitInst(setInput(temp(VR1)), emit(setstr, [0, VR1]).C, C).
emitInst(setOutput(temp(VR1)), emit(setstr, [1, VR1]).C, C).
emitInst(currentInput(temp(VR1)), emit(currstr, [0, VR1]).C, C).
emitInst(currentOutput(temp(VR1)), emit(currstr, [1, VR1]).C, C).
emitInst(funcall(N), emit(funcall, [N]).C, C).
emitInst(apply(N), emit(apply, [N]).C, C).
emitInst(execs(temp(VR1), temp(VR2)), emit(execs, [VR1, VR2]).C, C).
emitInst(throw(temp(VR1)), emit(throw, [VR1]).C, C).
emitInst(symbol(temp(VR1), temp(VR2)), emit(symbol, [VR1, VR2]).C, C).
emitInst(copy(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(copy, [VR1, VR2, VR3, VR4]).C, C).
emitInst(uncopy(temp(VR1), temp(VR2), temp(VR3)),
		emit(uncopy, [VR1, VR2, VR3]).C, C).
emitInst(erase(temp(VR1)), emit(erase, [VR1]).C, C).
emitInst(abolishCode(temp(VR1), temp(VR2)),
		emit(abolish, [VR1, VR2]).C, C).
emitInst(makeBMT(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(makebmt, [VR1, VR2, VR3, VR4]).C, C).
emitInst(linkBMT(N, temp(VR1), temp(VR2)), emit(linkbmt, [N, VR1, VR2]).C, C).
emitInst(instance(temp(VR1), temp(VR2), temp(VR3)),
		emit(inst, [VR1, VR2, VR3]).C, C).
emitInst(proplist(temp(VR1), temp(VR2), temp(VR3)),
		emit(props, [VR1, VR2, VR3]).C, C).
emitInst(replacn(temp(VR1), temp(VR2), temp(VR3)),
		emit(replacn, [VR1, VR2, VR3]).C, C).
emitInst(replacn_var(temp(VR1), temp(VR2), temp(VR3)),
		emit(replacv, [VR1, VR2, VR3]).C, C).
emitInst(setarg(temp(VR1), temp(VR2), temp(VR3)),
		emit(setarg, [VR1, VR2, VR3]).C, C).
emitInst(setarg_var(temp(VR1), temp(VR2), temp(VR3)),
		emit(setargv, [VR1, VR2, VR3]).C, C).
emitInst(aref(temp(VR1), temp(VR2), temp(VR3), N),
		emit(aref, [VR1, VR2, VR3, N]).C, C).
emitInst(aset(temp(VR1), temp(VR2), temp(VR3), temp(VR4), N),
		emit(aset, [VR1, VR2, VR3, VR4, N]).C, C).
emitInst(oncut(temp(VR1), temp(VR2)), emit(oncut, [VR1, VR2]).C, C).
emitInst(iseq(temp(VR1), temp(VR2), temp(VR3)),
		emit(iseq, [VR1, VR2, VR3]).C, C).
emitInst(copyVariablesToTopOfHeap(temp(VR1)), emit(cvttoh, [VR1]).C, C).
emitInst(fork(temp(VR1), temp(VR2), temp(VR3)),
		emit(fork, [0, VR1, VR2, VR3, 0]).C, C).
emitInst(fork(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(fork, [1, VR1, VR2, VR3, VR4]).C, C).
emitInst(load(temp(VR1)), emit(load, [VR1]).C, C).
emitInst(iload(temp(VR1), temp(VR2), temp(VR3)),
		emit(iload, [VR1, VR2, VR3]).C, C).
emitInst(fload(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(fload, [VR1, VR2, VR3, VR4]).C, C).
emitInst(spy(temp(VR1), temp(VR2)), emit(spy, [VR1, VR2]).C, C).
emitInst(nospy(temp(VR1), temp(VR2)), emit(nospy, [VR1, VR2]).C, C).
emitInst(syscall(SCN,
			temp(VR2), temp(VR3), temp(VR4), temp(VR5)),
		emit(syscall, [SCN, VR2, VR3, VR4, VR5]).C, C).
emitInst(abort, emit(abort, []).C, C).
emitInst(exit(temp(VR1)), emit(exit, [VR1]).C, C).
emitInst(pstot(temp(VR1), temp(VR2)), emit(pstot, [VR1, VR2]).C, C).
emitInst(simc_hash(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(simc, [0, VR1, VR2, VR3, VR4]).C, C).
emitInst(simc_query(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(simc, [1, VR1, VR2, VR3, VR4]).C, C).
emitInst(simc_next(temp(VR1), temp(VR2)),
		emit(simc, [2, VR1, VR2, 255, 255]).C, C).
emitInst(simc_end(temp(VR1)),
		emit(simc, [3, VR1, 255, 255, 255]).C, C).
emitInst(simc_assert(temp(VR1), temp(VR2)),
		emit(simc, [4, VR1, VR2, 255, 255]).C, C).
emitInst(simc_delete(temp(VR1)),
		emit(simc, [5, VR1, 255, 255, 255]).C, C).
emitInst(simc_abort(temp(VR1)),
		emit(simc, [6, VR1, 255, 255, 255]).C, C).
emitInst(dsimc_open(temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(dsimc, [0, VR1, VR2, VR3, VR4, 255]).C, C).
emitInst(dsimc_free(temp(VR1)),
		emit(dsimc, [1, VR1, 255, 255, 255, 255]).C, C).
emitInst(dsimc_cv(temp(VR1), temp(VR2), temp(VR3)),
		emit(dsimc, [2, VR1, VR2, VR3, 255, 255]).C, C).
emitInst(dsimc_sfbquery(
			temp(VR1), temp(VR2), temp(VR3), temp(VR4), temp(VR5)),
		emit(dsimc, [3, VR1, VR2, VR3, VR4, VR5]).C, C).
emitInst(dsimc_query(
			temp(VR1), temp(VR2), temp(VR3), temp(VR4)),
		emit(dsimc, [4, VR1, VR2, VR3, VR4, 255]).C, C).
emitInst(sql_query(temp(VR1), temp(VR2), temp(VR3)),
		emit(sql, [0, VR1, VR2, VR3]).C, C).
emitInst(sql_next(temp(VR1), temp(VR2)),
		emit(sql, [1, VR1, VR2, 255]).C, C).
emitInst(sql_end(temp(VR1)), emit(sql, [2, VR1, 255, 255]).C, C).
emitInst(sql_modify(temp(VR1), temp(VR2)),
		emit(sql, [3, VR1, VR2, 255]).C, C).
emitInst(sql_abort(temp(VR1)), emit(sql, [4, VR1, 255, 255]).C, C).
emitInst(type(temp(VR1), TypeCode), emit(typx, [VR1, TypeCode]).C, C).
emitInst(type(perm(VR1), TypeCode), emit(typy, [VR1, TypeCode]).C, C).
emitInst(ctype(temp(VR1), TypeCode), emit(ctypx, [VR1, TypeCode]).C, C).
emitInst(ctype(perm(VR1), TypeCode), emit(ctypy, [VR1, TypeCode]).C, C).
emitInst(jtype(temp(VR1), TypeCode, Label),
		emit(jtypx, [VR1, TypeCode], &(Label)).C, C).
emitInst(jtype(perm(VR1), TypeCode, Label),
		emit(jtypy, [VR1, TypeCode], &(Label)).C, C).
emitInst(jctype(temp(VR1), TypeCode, Label),
		emit(jctypx, [VR1, TypeCode], &(Label)).C, C).
emitInst(jctype(perm(VR1), TypeCode, Label),
		emit(jctypy, [VR1, TypeCode], &(Label)).C, C).
emitInst(term(Term), C0, C) :-
	emitTerm(Term, C0, C).
emitInst({Comment}, {Comment}.C, C).

emitConstant(Label, Term, $(Label).C) :-
	emitTerm(Term, C, []).

%	Emit pseudo-ops to construct a constant term.
%
%	Ugly.
emitTerm(Term, C, CE) :-
	emitTerm(Term, C, CE1, LeftOvers, []),
	( LeftOvers == [] ->
		CE1 = CE
	;	emitLeftOvers(LeftOvers, CE1, CE)
	).

emitTerm(struct(Pred.Args), emit('.word', [], Pred/Arity).C0, C, L0, L) :-
	length(Args, Arity),
	emitTermL(Args, C0, C, L0, L).
emitTerm(list(List), C0, C, L0, L) :-
	emitTermL(List, C0, C, L0, L).
emitTerm(string(String), emit('.string', [], string(String)).C, C, L, L).
emitTerm(const(Atomic), emit('.word', [], const(Atomic)).C, C, L, L) :-
	atomic(Atomic).

?- emitTermL(X, _, _, _, _) when X.
emitTermL([], C, C, L, L).
emitTermL(T.TT, C0, C, L0, L) :-
	( T = const(Const), atomic(Const) ->
		C0 = emit('.word', [], T).C1,
		emitTermL(TT, C1, C, L0, L)
	;	C0 = emit('.word', [], #(Tag, Label, _)).C1,
		L0 = leftOver(Label, T).L1,
		tagOfTerm(T, Tag),
		makeLabel(Label),
		emitTermL(TT, C1, C, L1, L)
	).

?- emitLeftOvers(X, _, _) when X.
emitLeftOvers([], C, C).
emitLeftOvers(leftOver(Label, Term).LeftOvers, $(Label).C, CE) :-
	emitTerm(Term, C, CE1),
	emitLeftOvers(LeftOvers, CE1, CE).

?- emitAsm([]) when ever.
?- emitAsm(X._) when X.
emitAsm([]).
emitAsm(emit(OpCode, Args).IT) :-
	emit(OpCode, Args),
	emitAsm(IT).
emitAsm(emit(OpCode, Args, Extension).IT) :-
	emit(OpCode, Args, Extension),
	emitAsm(IT).
emitAsm(emit(OpCode, Args, Extension1, Extension2).IT) :-
	emit(OpCode, Args, Extension1, Extension2),
	emitAsm(IT).
emitAsm($(Label).IT) :-
	emitLabel(Label),
	emitAsm(IT).
emitAsm({choiceEndsAt(Label)}.IT) :-
	!,
	putl("%\tChoicepoint ends at label "),
	writeln(Label),
	emitAsm(IT).
emitAsm({Comment}.IT) :-
	putl(user_error, "%\tMiscellaneous error.  Try running NIT.\n%\t"),
	writeln(user_error, Comment),
	emitAsm(IT).
emitAsm(junk(Junk).IT) :-
	putl(user_error, "%\tCan't emit code for "), writeln(user_error, Junk),
	emitAsm(IT).

emit(OpCode, Args) :-
	put(0'\t), display(OpCode),
	put(0'\t), emitArgs(Args),
	nl.

emit(OpCode, Args, Extension) :-
	put(0'\t), display(OpCode),
	put(0'\t), emitArgs(Args),
	(cons(Args) -> put(0',)), emitExtension(Extension),
	nl.

emit(OpCode, Args, Extension1, Extension2) :-
	put(0'\t), display(OpCode),
	put(0'\t), emitArgs(Args),
	(cons(Args) -> put(0',)), emitExtension(Extension1),
	put(0',), emitExtension(Extension2),
	nl.

emitArgs([]).
emitArgs(Arg.Args) :-
	( cons(Args) ->
		display(Arg), put(0',),
		emitArgs(Args)
	;	Args = [],
		display(Arg)
	).

emitExtension(const(Const)) :-
	put(0'$),
	( atom(Const) ->
		putIdentifier(Const)
	;	display(Const)
	).
emitExtension(integer(Const)) :-
	putl("$$"),
	display(Const).
emitExtension(string(String)) :-
	isPrintL(String),
	write(String).
emitExtension(#(Tag, Label, _)) :-
	putl("#("), display(Tag), putl(",&"), display(Label), put(0')).
emitExtension(Pred/Arity) :-
	put(0'$), putIdentifier(Pred), put(0'/), display(Arity).
emitExtension(&(Label)) :-
	put(0'&), display(Label).
emitExtension(&(Pred, Arity)) :-
	put(0'&), putIdentifier(Pred), put(0'/), display(Arity).
emitExtension(sotTable(L1, L2, L3, L4)) :-
	put(0'&), display(L1),
	putl(",&"), display(L2),
	putl(",&"), display(L3),
	putl(",&"), display(L4).
emitExtension(table(Default, Table)) :-
	putl("&("),
	emitTable(Table),
	putl("),&"),
	display(Default).

?- emitTable([]) when ever.
?- emitTable(X._) when X.
emitTable([]).
emitTable(entry(Key, Label).Table) :-
	put(0'$),
	( atom(Key) ->
		putIdentifier(Key)
	;	display(Key)
	),
	putl(":&"), display(Label),
	( Table == [] ->
		true
	;	put(0',),
		emitTable(Table)
	).
emitTable(entry(Pred, Arity, Label).Table) :-
	put(0'$), putIdentifier(Pred), put(0'/), display(Arity),
	putl(":&"), display(Label),
	( Table == [] ->
		true
	;	put(0',),
		emitTable(Table)
	).

emitLabel(Label) :-
	display(Label), putl(": ").
