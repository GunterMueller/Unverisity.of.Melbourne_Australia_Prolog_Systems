/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- main loop.

?- dynamic co$compilationErrors/0.
?- dynamic co$unexplainedAbort/0.

main(_.Files) :-
	( co$unexplainedAbort ->
		format(user_error, "~NError -- Unexplained abort.~n", []),
		exit(1)
	),
	initCompiler,
	mainL(Files),
	( co$compilationErrors ->
		exit(1)
	).

nuc(File) :-
	initCompiler,
	mainL([File]).

mainL([]).
mainL('-F'.Option.Files) :-
	!,
	( Option == pnc ->
		op(900, fy, $pcall)
	),
	asserta(option(Option)),
	mainL(Files).
mainL(['-D']) :-
	!,
	repeat,
		read(X),
		( isEof(X) ->
			true
		;	expandTerm(X, Y),
			portraycl(Y),
			putl(". \n"),
			fail
		).
mainL(File.Args) :-
	format(user_output, "Reading and compiling ~a.~n", [File]),
	reinitCompiler,
	( see(File) ->
		true
	;	format(user_error, "Unable to read ~w.~n", [File]),
		exit(1)
	),
	( Args = '-o'.Obj.RestOfArgs ->
		true
	;	Args = RestOfArgs,
		name(File, FileName),
		append(BaseName, ".nl", FileName),
		append(BaseName, ".ns", ObjName),
		name(Obj, ObjName)
	),
	( tell(Obj) ->
		true
	;	format(user_error, "Unable to write ~w.~n", [Obj]),
		exit(1)
	),
	fileErrors,
	co$doRead.
mainL(_._) :-
	noFileErrors,
	seen,
	co$retrieveInits,
	compile,			% Catch any leftovers, including $init/0.
	fail.
mainL(_.Args) :-
	nl,
	told,
	( Args = '-o'.Obj.RestOfArgs ->
		mainL(RestOfArgs)
	;	mainL(Args)
	).

initCompiler :-
	assert(co$unexplainedAbort),
	noFileErrors,
	name(vr, VR),
	$replacn(1, VR, 1),				% Temporary fudge to avoid name clashes.
	name(fence, Fence),
	$replacn(1, Fence, 1),
	name(conj, Conj),
	$replacn(1, Conj, 1),
	name(disj, Disj),
	$replacn(1, Disj, 1),
	true.

reinitCompiler :-
	noFileErrors,
	putprop(label, $label, 0),
	\+  (
		getprop($predicates, $compiling, P1/A1),
		remprop($clauses, $compileClause(P1, A1)),
		remprop($whens, $compileWhen(P1, A1)),
		fail
	),
	\+	(
		getprop($alreadyCompiledPredicates, $compiled, P2/_),
		remprop(P2, $compiled),
		fail
	),
	remprop($alreadyCompiledPredicates, $compiled),
	remprop($predicates, $compiling),
	remprop($inits, $compilerInit).

compile :-
	properties($predicates, $compiling, Props),
	reverse(Props, Preds),						% Compile things in order
	\+ ((
		member(Pred/Arity, Preds),
		compile(Pred/Arity),
		fail
	)).

compileSoFar(P, A) :-
	\+ getprop($predicates, $compiling, P/A),
	( getprop(P, $compiled, A) ->
		assert(co$compilationErrors),
		putl("THIS .NS FILE IS NOT VALID\n"),
		remprop($alreadyCompiledPredicates, $compiled, P/A),
		format(user_error,
			"~NError: ~a/~d has clauses separated by another predicate.~n",
			[P, A])
	),
	properties($predicates, $compiling, Preds),
	\+ (
		member(Pred/Arity, Preds),
		remprop($predicates, $compiling, Pred/Arity),
		addpropa(Pred, $compiled, Arity),
		addpropa($alreadyCompiledPredicates, $compiled, Pred/Arity),
		compile(Pred/Arity),
		fail
	),
	putprop(label, $label, 0).

compile(Pred/Arity) :-
	retractall(groundTerm, 3),
	collectClauses(Pred, Arity, Clauses, HasCut),
	collectWhens(Pred, Arity, HasCut, Whens),
	( cons(Whens), Clauses == [] ->
		format(user_error,
			"~NError in compiling ~a/~d -- \c
				when declaration(s), but no clauses given.~n",
			[Pred, Arity])
	;	compile1(Pred, Arity, Clauses, HasCut, Whens)
	).

%	We turn on characterEscapes because the assembler needs a consistent
%	interface.
compile1(Pred, Arity, Clauses, HasCut, Whens) :-
	prologFlag(characterEscapes, OldCharEsc, on),
	putl("\n\t.pred\t"), putIdentifier(Pred), put(0',), display(Arity), nl,
	compileWhens(Whens, HasCut, Clauses, Prelude),
	choicesOfPrelude(Prelude, Choices0, []),
	uniqueChoices(Choices0, Choices),
	clausesUsed(Choices, 0, NArgs, ClauseInstances, []),
	labelsUsed(ClauseInstances, LabelsUsed),
	makeMap(LabelsUsed, Map),
	once compilePrelude(Prelude, Arity, Map, Whens, HasCut, PreludeCode, []),
	polishCode(PreludeCode, PreludeCode1),
	optimizePrelude(PreludeCode1, PreludeCode2),
	once emitCode(PreludeCode2, PreludeAsm, []),
	once emitAsm(PreludeAsm),
	keyClauses(Clauses, KeyedClauses),
	once makeKeyedLookupTree(KeyedClauses, ClausesTree),
	\+ 
	(	member(L1, LabelsUsed),
		getClauseForLabel(L1, ClausesTree, Map, L2, Head, Body),
		catchCompilerError(
			compileAndEmit1Clause(L2, Pred, Arity,
				NArgs, Head, Body, PreludeCode2),
			Error,
			(	format(user_error,
					"~NError in compiling (~w).~n", [(Head :- Body)]),
				call(Error)
			)
		),
		fail
	),
	putl("\tlast\n"),
	compileConstants,
	prologFlag(characterEscapes, _, OldCharEsc),
	true.

getClauseForLabel(L1, ClausesTree, Map, L2, Head, Body) :-
	( integer(L1) ->
		L2 = L1,
		lookupKeyedTree(L1, ClausesTree, clause(_, _, Head, Body, _))
	;	mapLabel(L1, Map, L2),
		$last(L1, L3),
		lookupKeyedTree(L3, ClausesTree, clause(_, _, BaseHead, Body, _)),
		expandHead(L1, BaseHead, Head)
	).

compileAndEmit1Clause(L2, Pred, Arity, NArgs, Head, Body, PreludeCode2) :-
	( compile1SimpleClause(Head, Body, NArgs, Code) ->
		true
	;	compile1Clause(Head, Body, NArgs, Code1),
		( PreludeCode2 = $(_).Branch._, isSmallBranchInstruction(Branch) ->
			polishCode(Code1, Pred, Arity, Branch, Code)
		;	polishCode(Code1, Code)
		)
	),
	putl("\t.clause\n"),
	display(L2), put(0':),
	emitCode(Code, Asm, []),
	emitAsm(Asm).

collectClauses(Pred, Arity, Clauses, HasCut) :-
	properties($clauses, $compileClause(Pred, Arity), Props),
	remprop($clauses, $compileClause(Pred, Arity)),
	( member(clause(_, _, _, cut(_)), Props) ->
		addCuts(Props, Clauses),
		HasCut = yes(Arity)
	;	noCuts(Props, Clauses),
		HasCut = no
	).

?- addCuts(X, Y) when X or Y.
addCuts([], []).
addCuts(
		clause(Line, L, (Head :- Body), Cut).Props,
		clause(Line, L, HeadWithCut, Body, Cut).Clauses) :-
	( Cut = cut(CutLabel) -> true),	% Not every clause has a cut
	Head =.. Args,
	append(Args, [CutLabel], ArgsWithCut),
	HeadWithCut =.. ArgsWithCut,
	addCuts(Props, Clauses).

?- noCuts(X, Y) when X or Y.
noCuts([], []).
noCuts(	
		clause(Line, L, (Head :- Body), _).Props,
		clause(Line, L, Head, Body, no).Clauses) :-
	noCuts(Props, Clauses).

collectWhens(Pred, Arity, HasCut, Whens) :-
	properties($whens, $compileWhen(Pred, Arity), Props),
	remprop($whens, $compileWhen(Pred, Arity)),
	(HasCut = no ->
		Props = Whens
	;	addCutsToWhens(Props, Whens)
	).

?- addCutsToWhens(X, Y) when X or Y.
addCutsToWhens([], []).
addCutsToWhens(
		when(Line, Pattern, Vars).Props,
		when(Line, PatternWithCut, Vars).Whens) :-
	append(Pattern, [_], PatternWithCut),
	addCutsToWhens(Props, Whens).

?- keyClauses(X, Y) when X or Y.
keyClauses([], []).
keyClauses(X.XT, (Key - X).YT) :-
	X = clause(_, Key, _, _, _),
	keyClauses(XT, YT).

compileConstants :-
	\+
	(	groundTerm(_Tag, Label, Term),
		emitConstant(Label, Term, ConstantAsm),
		emitAsm(ConstantAsm),
		fail
	).

compile1SimpleClause(Head, true, NArgs, Code) :-
	compile1Head(Head, NArgs, Code0),
	codegenCleanUp(Code0, Code).

compile1Clause(Head, Body, NArgs, Code) :-
	canonicalize(Head, Body, CC, NVars, EnvNeeded),
	varsOfClause(CC, VCC),
	addInitializers(VCC),
	detVars(VCC),
	complexBuiltins(CC, BCC, NVars, NVars1),
	varsOfClause(BCC, VBCC),
	varInstances(VBCC),
%	functor(Head, _, Arity),
	allocateTemps(BCC, NArgs),
	allocatePerms(BCC, NPerms),
	local(BCC, NVars1),
	codegen(BCC, NArgs, EnvNeeded, NPerms, Code),
	!,
	true.

allocateTemps(BCC, NArgs) :-
	tempsOfClause(BCC, TVCC),
	desireList(BCC, Desires),
%	desireCount(Desires, DC),
%	constrainRegs(TVCC),
	constrainRegs(TVCC, Constraints),
	fulfillDesires(Desires),
	allocateTemps(Constraints, NArgs, _MaxTemp),
	true.

/*	Incremental compilation NYI
compileClause(Head, Body, Code) :-
	retractall(groundTerm, 3),
	canonicalize(Head, Body, CC, NVars),
	varsOfClause(CC, VCC),
	addInitializers(VCC),
	detVars(VCC),
	complexBuiltins(CC, BCC, NVars, _NVars1),
	varsOfClause(BCC, VBCC),
	varInstances(VBCC),
	tempsOfClause(BCC, TVCC),
	desireList(BCC, Desires),
%	desireCount(Desires, DC),
	constrainRegs(TVCC),
	tempList(TVCC, Temps),
	fulfillDesires(Desires),
	allocateTemps(Temps, _MaxTemp),
	allocatePerms(BCC, NPerms),
	local(BCC),
	codegen(BCC, NPerms, Code),
	true.
*/

co$doRead :-
	currentInput(Stream),
	repeat,
	lineCount(Stream, Line),
	( read1(Clause) ->
		true
	;	assert(co$compilationErrors),
		fail
	),
	( \+ isEof(Clause) ->
		co$storeClause(Clause, Line),
		fail
	),
	!,
	fail.

co$storeClause(C, Line) :-
	expandTerm(C, D),
	co$storeClause2(D, Line).

co$storeClause2(:-(when(Pattern, Vars)), Line) :-
	functor(Pattern, Pred, Arity),
	compileSoFar(Pred, Arity),
	fail.
co$storeClause2(:-(when(Pattern, Vars)), Line) :-
	!,
	co$storeWhen(Pattern, Vars, Line).
co$storeClause2(?-(when(Pattern, Vars)), Line) :-
	functor(Pattern, Pred, Arity),
	compileSoFar(Pred, Arity),
	fail.
co$storeClause2(?-(when(Pattern, Vars)), Line) :-
	!,
	co$storeWhen(Pattern, Vars, Line).
co$storeClause2(:-(G), Line) :-
	!,
	co$storeGoal(G, Line).
co$storeClause2(?-(G), Line) :-
	!,
	co$storeGoal(G, Line).
co$storeClause2((H :- B), _Line) :-
	functor(H, Pred, Arity),
	compileSoFar(Pred, Arity),
	fail.
co$storeClause2((H :- B), Line) :-
	functor(H, Pred, Arity),
	dynamic(Pred, Arity),
	!,
	assertz((H :- B)),
	co$storeInit((assertz((H :- B)), fail), Line).
co$storeClause2((H :- B), Line) :-
	!,
	$neg_trf((H :- B), (Head :- Body), Defns),
	co$storeClause(Head, Body, Line),
	co$storeAuxPreds(Defns, Line).
co$storeClause2(Head, _Line) :-
	functor(Head, Pred, Arity),
	compileSoFar(Pred, Arity),
	fail.
co$storeClause2(Head, Line) :-
	functor(Head, Pred, Arity),
	dynamic(Pred, Arity),
	!,
	assertz(Head),
	co$storeInit((assertz(Head), fail), Line).
co$storeClause2(Head, Line) :-
	co$storeClause(Head, true, Line).

when(Pattern, Vars) :-
	co$storeWhen(Pattern, Vars, Line).

co$storeWhen(Pattern, Vars, Line) :-
	Pattern =.. (Pred.Args),
	functor(Pattern, Pred, Arity),
	addpropz($whens, $compileWhen(Pred, Arity), when(Line, Args, Vars)),
	( getprop($predicates, $compiling, Pred/Arity) ->
		true
	;	addpropa($predicates, $compiling, Pred/Arity)
	).

co$storeGoal(G, Line) :-
	co$runGoal(G),
	(	G = (useIf _)
	;	G = useElse
	;	G = useEnd
	;	co$storeInit((once G, fail), Line)
	),
	!.

co$runGoal(G) :-
	currentOutput(Old),
	setOutput(user_output),
	(	once G, fail
	;	true
	),
	setOutput(Old).

co$storeClause(Head, Body, Line) :-
	functor(Head, Pred, Arity),
	makeLabel(L),
	preFlatten(Body, FlatBody, Cut),
	( Cut = no -> true),
	addpropz($clauses, $compileClause(Pred, Arity),
		clause(Line, L, (Head :- FlatBody), Cut)),
	( getprop($predicates, $compiling, Pred/Arity) ->
		true
	;	addpropa($predicates, $compiling, Pred/Arity)
	).

%	Push an initializer aside for later use.
%	Mainly done to avoid line number clashes.
co$storeInit(Body, Line) :-
	addpropz($inits, $compilerInit, init(Body, Line)).

co$retrieveInits :-
	\+ (
		getprop($inits, $compilerInit, init(Body, Line)),
		co$storeClause($init, Body, Line),
		fail
	),
	remprop($inits, $compilerInit).

%	Store a list of auxilliary predicates created by the quantifier processors.
co$storeAuxPreds(L, Line) :-
	\+ (
		member(defn(G, H, B), L),
		co$storeAuxPred(G, H, B, Line),
		fail
	).

co$storeAuxPred(G, Head, Body, Line) :-
	( G \== [] ->							% Doesn't happen currently
		co$ltogwhens(G, Wheng),
		co$storeWhen(Head, Wheng, Line)
	),
	co$storeClause(Head, Body, Line).

%	convert from list of vars to body of when declaration
co$ltogwhens([G], G).
co$ltogwhens(G.G1.GL, (G and Wheng)) :-
	co$ltogwhens(G1.GL, Wheng).

catchCompilerError(Goal, Error, Action) :-
	catch(Goal, co$error(Error)),
	(nonvar(Error) ->
		!,
		assert(co$compilationErrors),
		format(user_error, "~NError in compilation.~n", []),
		call(Action),
		!,
		fail
	).

throwCompilerError(Error) :-
	throw(co$error(Error)).
