/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- prelude generator.

%	Split a specified element of an argList into its subterms
%	leaving the first subterm in place and appending the rest to the
%	end of the arg list.
splitArgN(N, Term.Args, NewArgs) :-
	( N > 0 ->
		NewArgs = Term.NewArgsT,
		N1 is N - 1,
		splitArgN(N1, Args, NewArgsT)
	;	Term =.. (_.S.ST),
		NewArgs = S.NewArgsT,
		append(Args, ST, NewArgsT)
	).

%	Apply splitArgN to a list of argLists.
:- splitArgNList(_, X, Y) when X or Y.
splitArgNList(_, [], []).
splitArgNList(N, ArgList.ArgLists, NewArgList.NewArgLists) :-
	splitArgN(N, ArgList, NewArgList),
	splitArgNList(N, ArgLists, NewArgLists).

%	Split a specified element of an argList into its subterms
%	leaving a new variable in its place and appending the subterms
%	to the end of the arg list.
splitArgNWithResidue(N, Term.Args, NewArgs) :-
	( N > 0 ->
		NewArgs = Term.NewArgsT,
		N1 is N - 1,
		splitArgNWithResidue(N1, Args, NewArgsT)
	;	Term =.. (_.ST),
		NewArgs = _.NewArgsT,
		append(Args, ST, NewArgsT)
	).

%	Apply splitArgNWithResidue to a list of argLists.
:- splitArgNWithResidueList(_, X, Y) when X or Y.
splitArgNWithResidueList(_, [], []).
splitArgNWithResidueList(N, ArgList.ArgLists, NewArgList.NewArgLists) :-
	splitArgNWithResidue(N, ArgList, NewArgList),
	splitArgNWithResidueList(N, ArgLists, NewArgLists).

%	Forget a specified element of an argList.
%	I.e. replace it with a variable.
forgetArgN(N, Args, NewArgs) :-
	( N > 0 ->
		Args = Term.ArgsT,
		NewArgs = Term.NewArgsT,
		N1 is N - 1,
		forgetArgN(N1, ArgsT, NewArgsT)
	;	Args = _.ArgsT,
		NewArgs = _.ArgsT
	).

%	Apply forgetArgN to a list of argLists.
:- forgetArgNList(_, X, Y) when X or Y.
forgetArgNList(_, [], []).
forgetArgNList(N, ArgList.ArgLists, NewArgList.NewArgLists) :-
	forgetArgN(N, ArgList, NewArgList),
	forgetArgNList(N, ArgLists, NewArgLists).

%	Extend each list in a list of lists
%	by appending a list of N new variables.
:- extendListList(_, X, Y) when X or Y.
extendListList(_, [], []).
extendListList(N, L.LT, EL.ELT) :-
	length(Extension, N),
	append(L, Extension, EL),
	extendListList(N, LT, ELT).

%	Filter a list of things into sublists of var, const, list, and struct
%	arguments depending on argument N of a corresponding list of argLists.
:- filterThings(_, X, Y, _, _, _, _) when X or Y.
filterThings(_, [], [], [], [], [], []).
filterThings(N, Thing.Things, ArgList.ArgLists, VarL, ConstL, ListL, StructL) :-
	nth0(N, ArgList, Arg),
	filterThings(Arg, Things, ArgLists,
		VarL, ConstL, ListL, StructL, N, Thing).

filterThings(Arg, Things, ArgLists,
		VarL0, ConstL, ListL, StructL, N, Thing) :-
	var(Arg),
	VarL0 = Thing.VarL,
	filterThings(N, Things, ArgLists, VarL, ConstL, ListL, StructL).
filterThings(Arg, Things, ArgLists,
		VarL, ConstL0, ListL, StructL, N, Thing) :-
	const(Arg),
	ConstL0 = Thing.ConstL,
	filterThings(N, Things, ArgLists, VarL, ConstL, ListL, StructL).
filterThings(Arg, Things, ArgLists,
		VarL, ConstL, ListL0, StructL, N, Thing) :-
	cons(Arg),
	ListL0 = Thing.ListL,
	filterThings(N, Things, ArgLists, VarL, ConstL, ListL, StructL).
filterThings(Arg, Things, ArgLists,
		VarL, ConstL, ListL, StructL0, N, Thing) :-
	$struct(Arg),
	StructL0 = Thing.StructL,
	filterThings(N, Things, ArgLists, VarL, ConstL, ListL, StructL).

%	Filter a list of heads into sublists of var, const, list, and struct
%	arguments depending on argument N of head.
filterHeads(N, Heads, VarL, ConstL, ListL, StructL) :-
	N1 is N + 1,
	filterHeads1(N1, Heads, VarL, ConstL, ListL, StructL).

:- filterHeads1(_, X, _, _, _, _) when X.
filterHeads1(_, [], [], [], [], []).
filterHeads1(N, Head.Heads, VarL, ConstL, ListL, StructL) :-
	Head = clause(_, _, H, _, _),
	arg(N, H, Arg),
	filterHeads(Arg, Heads, VarL, ConstL, ListL, StructL, N, Head).

filterHeads(Arg, Heads, VarL0, ConstL, ListL, StructL, N, Head) :-
	var(Arg),
	VarL0 = Head.VarL,
	filterHeads1(N, Heads, VarL, ConstL, ListL, StructL).
filterHeads(Arg, Heads, VarL, ConstL0, ListL, StructL, N, Head) :-
	const(Arg),
	ConstL0 = Head.ConstL,
	filterHeads1(N, Heads, VarL, ConstL, ListL, StructL).
filterHeads(Arg, Heads, VarL, ConstL, ListL0, StructL, N, Head) :-
	cons(Arg),
	ListL0 = Head.ListL,
	filterHeads1(N, Heads, VarL, ConstL, ListL, StructL).
filterHeads(Arg, Heads, VarL, ConstL, ListL, StructL0, N, Head) :-
	$struct(Arg),
	StructL0 = Head.StructL,
	filterHeads1(N, Heads, VarL, ConstL, ListL, StructL).

%	Select the least argument N from a list of argLists such that either
%		1) all the heads have the same type of non-variable object
%		in this position
%	or, failing that
%		2) all the heads have a non-variable object in this position.
selectFilter(N, Things, ArgLists, ConstL, ListL, StructL) :-
	ArgLists = ArgList._,
	length(ArgList, Arity),
	selectFilter1(ConstL, ListL, StructL),
	Arity1 is Arity - 1,
	between(0, Arity1, N),
	filterThings(N, Things, ArgLists, [], ConstL, ListL, StructL),
%	delete(_, [ConstL, ListL, StructL], [[], []]),
	!.
selectFilter(N, Things, ArgLists, ConstL, ListL, StructL) :-
	ArgLists = ArgList._,
	length(ArgList, Arity),
	Arity1 is Arity - 1,
	between(0, Arity1, N),
	filterThings(N, Things, ArgLists, [], ConstL, ListL, StructL),
	!.

%	About the same thing as
%	p(A, B, C) :-
%		(if A = _._ then
%			B = [], C = []
%		else if B = _._ then
%			C = []
%		else
%			C = _._
%		).
%	Good enough in this case because the cons is the first binding
%	made by filterThings/7.
:- selectFilter1(A, B, C) when A or B or C.
selectFilter1(_._, [], []).
selectFilter1([], _._, []).
selectFilter1([], [], _._).

compileWhens(NumberedWhens, HasCut, Heads, $(Loop) + Prelude) :-
	( HasCut == no ->
		Enter = nop
	;	HasCut = yes(CutVar),		% CutVar is determined by compilePrelude
		Enter = label(temp(CutVar))
	),
	stripLineNumbersFromWhens(NumberedWhens, Whens),
	catchCompilerError(
		compileWhens(Loop, Whens, Enter, Heads, Prelude),
		Error,
		(	unableToCompileWhens(NumberedWhens, Heads),
			call(Error)
		)),
	!.
compileWhens(NumberedWhens, _, Heads, _) :-
	unableToCompileWhens(NumberedWhens, Heads),
	fail.

:- stripLineNumbersFromWhens(X, Y) when X or Y.
stripLineNumbersFromWhens([], []).
stripLineNumbersFromWhens(when(_, Pattern, Vars).XT, when(Pattern, Vars).YT) :-
	stripLineNumbersFromWhens(XT, YT).

unableToCompileWhens(Whens, Heads) :-
	( Heads = clause(_, _, H, _, _)._ ->
		functor(H, F, _)
	;	F = ?			% never happens?
	),
	putl(user_error, "Unable to compile the following when declarations.\n"),
	writeWhens(F, Whens).

%	Write out list of whens in readable format with line numbers.
writeWhens(_, []).
writeWhens(F, when(Line, A, B).Whens) :-
	H =.. F.A,
	(	numberVars((H when B), 1, _),
		format(user_error, "~t~d~+: ~w.~n", [Line, :-(H when B)]),
		fail			% to undo numberVars - yuk
	;	true
	),
	writeWhens(F, Whens).

compileWhens(_, [], _, Heads, Code) :-
	!,
	indexHeads(clauses(Heads, _), Code).
compileWhens(Loop, Whens, Enter, Heads,
		Enter + sot(N, delay([N], Loop), CP, LP, SP)) :-
	splitWhens(Whens, Patterns, _),
	selectFilter(N, Whens, Patterns, CW, LW, SW),
	!,
	filterHeads(N, Heads, VH, CH, LH, SH),
	compileConstantWhens(N, CW, Enter, VH, CH, CP),
	compileListWhens(N, LW, Enter, VH, LH, LP),
	compileStructWhens(N, SW, Enter, VH, SH, SP).
compileWhens(Loop, Whens, Enter, Heads, Prelude) :-
	splitWhens(Whens, [Pattern], [Vars]),
	co$numberWhenDeclVars(0, Pattern),
	co$checkWhenBody(Vars),
	compileWhen(Loop, Vars, go, [], 255, Enter, clauses(Heads, _), Prelude).

:- co$numberWhenDeclVars(_, Pattern) when Pattern.
co$numberWhenDeclVars(N, []).
co$numberWhenDeclVars(N, vr(N).Pattern) :-
	N1 is N + 1,
	co$numberWhenDeclVars(N1, Pattern).

co$checkWhenBody(Var) :-
	var(Var),
	!,
	fail.
co$checkWhenBody(vr(V)) :-
	integer(V).
co$checkWhenBody(ground(vr(V))) :-
	integer(V).
co$checkWhenBody(ever).
co$checkWhenBody(V1 and V2) :-
	co$checkWhenBody(V1),
	co$checkWhenBody(V2).
co$checkWhenBody(V1 or V2) :-
	co$checkWhenBody(V1),
	co$checkWhenBody(V2).

:- compileWhen(_, Var, _, _, _, _, _, _) when Var.				% Really index
%compileWhen(Loop, vr(Var), go, OtherVars, _GReg, Enter, Heads, Code0) :-
%	compileSOT(Var, delay(Var.OtherVars, Loop), Enter, Heads, Code).
%compileWhen(Loop, vr(Var), Or, OtherVars, GReg, Enter, Heads, Code0) :-
%	Or \== go,
%	compileWhen(Loop, Or, go, Var.OtherVars, GReg, Enter, Heads, Prelude),
%	compileSOT(Var, Prelude, Enter, Heads, Code).
compileWhen(Loop, go, go, OtherVars, _GReg, _Enter, _Heads,
		delay(OtherVars, Loop)).
compileWhen(Loop, vr(Var), Or, OtherVars, GReg, Enter, Heads, Code) :-
	compileWhen(Loop, Or, go, Var.OtherVars, GReg, Enter, Heads, Prelude),
	compileSOT(Var, Prelude, Enter, Heads, Code).
compileWhen(Loop, ground(vr(Var)), Or, OtherVars, GReg, Enter, Heads,
		fvar(Var, GReg) + jv(GReg, Prelude, IndexCode)) :-
	compileSOT(Var, fail, Enter, Heads, IndexCode),
	GReg1 is GReg - 1,
	compileWhen(Loop, Or, go, GReg.OtherVars, GReg1, Enter, Heads, Prelude).
%	RHS containing ever must be exactly that.
compileWhen(_Loop, ever, go, [], _GReg, Enter, Heads, Enter + Heads).
compileWhen(Loop1, W1 and W2, Or, OtherVars, GReg,
		Enter, Heads, P1 + $(Loop2) + P2) :-
	compileWhen(Loop1, W1, Or, OtherVars, GReg, Enter, j(Loop2), P1),
	compileWhen(Loop2, W2, Or, OtherVars, GReg, Enter, Heads, P2).
compileWhen(Loop, W1 or W2, go, OtherVars, GReg, Enter, Heads, Prelude) :-
	compileWhen(Loop, W1, W2, OtherVars, GReg, Enter, Heads, Prelude).
compileWhen(Loop, W1 or W2, Or, OtherVars, GReg, Enter, Heads, Prelude) :-
	Or \== go,
	compileWhen(Loop, W1, Or or W2, OtherVars, GReg, Enter, Heads, Prelude).

compileSOT(Var, IfVar, Enter, clauses(Heads, _),
		Enter + sot(Var, IfVar, VCP, VLP, VSP)) :-
	Heads = _._._,
	filterHeads(Var, Heads, VL, CL, LL, SL),
%	headOpt(Var, +, _, Heads, VarHeads),
	headOpt(Var, _, const, VL, VLC),
	headOpt(Var, _, _._, VL, VLL),
	headOpt(Var, _, struct(_), VL, VLS),
	(	cons(CL)
	;	cons(LL)
	;	cons(SL)
%	;	Heads \== VarHeads
	;	VL \== VLC
	;	VL \== VLL
	;	VL \== VLS
	),
	!,
	indexConstantHeads(Var, VLC, CL, VCP),
	indexListHeads(Var, VLL, LL, VLP),
	indexStructHeads(Var, VLS, SL, VSP).
compileSOT(_, fail, Enter, Else, Code) :-
	!,
	( Else = clauses(_._, _) ->
		Code = Enter + Else
	;	Code = Else
	).
compileSOT(Var, IfVar, Enter, Else, Code) :-
	( Else = clauses(_._, _) ->
		Code = Enter + jv(Var, IfVar, Else)
	;	Code = jv(Var, IfVar, Else)
	).

indexHeads(clauses(Heads, _), sot(0, clauses(VarHeads, _), VCP, VLP, VSP)) :-
	Heads = _._._,
	filterHeads(0, Heads, VL, CL, LL, SL),
	headOpt(0, +, _, Heads, VarHeads),
	headOpt(0, _, const, VL, VLC),
	headOpt(0, _, _._, VL, VLL),
	headOpt(0, _, struct(_), VL, VLS),
	(	cons(CL)
	;	cons(LL)
	;	cons(SL)
	;	Heads \== VarHeads
	;	VL \== VLC
	;	VL \== VLL
	;	VL \== VLS
	),
	!,
	indexConstantHeads(0, VLC, CL, VCP),
	indexListHeads(0, VLL, LL, VLP),
	indexStructHeads(0, VLS, SL, VSP).
indexHeads(Heads, Heads).

compileConstantWhens(Pos, Whens, Enter, VH, CH, Prelude) :-
	sortConstantWhens(Pos, Whens, CH, Groups, LeftOvers),
	( VH == [] ->
		Prelude = soce(Pos, clauses([], _), Table),
		constantWhens(Pos, Groups, Enter, VH, Table)
	; CH == [], exhaustedWhens(Pos, Whens) ->
		Prelude = clauses(VH, _)
	;	Prelude = soce(Pos, clauses(VH, _), Table),
		constantWhens(Pos, Groups, Enter, VH, Table)
	).

constantWhens(_, [], Enter, _, []).
constantWhens(N, group(Const, Whens, Heads).Groups, Enter, VarHeads, 
		entry(Const, $(Loop) + Prelude).Table) :-
	splitWhens(Whens, Patterns, Vars),
	forgetArgNList(N, Patterns, NewPatterns),
	splitWhens(NewWhens, NewPatterns, Vars),
	mergeHeads(VarHeads, Heads, NewHeads),
	compileWhens(Loop, NewWhens, Enter, NewHeads, Prelude),
	constantWhens(N, Groups, Enter, VarHeads, Table).

compileStructWhens(Pos, Whens, Enter, VH, SH, SOS) :-	% LEE
	sortStructWhens(Pos, Whens, SH, Groups, LeftOvers),
%	mergeHeads(LeftOvers, VH, Others),	% LEE Leftovers=[] currently
	( VH == [] ->
		% sos is ok if we do a gs straight away
		SOS = sos(Pos, clauses([], _), Table),
		structWhens(Pos, Groups, Enter, VH, Table)
	; SH == [], exhaustedWhens(Pos, Whens) ->
		SOS = clauses(VH, _)
	;	SOS = sose(Pos, clauses(VH, _), Table),
		structWhens(Pos, Groups, Enter, VH, Table)
	).

structWhens(_, [], Enter, _, []).
structWhens(N, group(SPred/SArity, Whens, Heads).Groups, Enter, VarHeads,
		entry(SPred, SArity,
			gs(N, SPred/SArity, UArgs, $(Loop) + Prelude)).Table) :-
	( Whens = when(Pattern, _)._ ->
		length(Pattern, NArgs)
	;	% if there are no whens there must be nonvar heads LEE
		Heads = clause(_, _, H, _, _)._,
		functor(H, _, NArgs)
	),
	splitWhens(Whens, Patterns, Vars),
	splitHeads(Heads, Labels, Terms),
	termsToArgLists(Terms, Pred, ArgLists),
	( VarHeads == [] ->
		LArgs is NArgs + SArity - 2,
		UArgs = N.Iota,
		iotaL(NArgs, LArgs, Iota),
		splitArgNList(N, Patterns, NewPatterns),
		NewVarHeads = [],
		splitArgNList(N, ArgLists, NewArgLists),
		relabel(N, Labels, NewLabels)
	;	LArgs is NArgs + SArity - 1,
		iotaL(NArgs, LArgs, UArgs),
		splitArgNWithResidueList(N, Patterns, NewPatterns),
		splitHeads(VarHeads, VarLabels, VarTerms),
		termsToArgLists(VarTerms, Pred, VarArgLists),
		extendListList(SArity, VarArgLists, NewVarArgLists),
		termsToArgLists(NewVarTerms, Pred, NewVarArgLists),
		relabel(fill(SArity), VarLabels, NewVarLabels),
		reassembleHeads(VarHeads, NewVarLabels, NewVarTerms, NewVarHeads),
		splitArgNWithResidueList(N, ArgLists, NewArgLists),
		relabel(residue(N), Labels, NewLabels)
	),
	splitWhens(NewWhens, NewPatterns, Vars),
	termsToArgLists(NewTerms, Pred, NewArgLists),
	reassembleHeads(Heads, NewLabels, NewTerms, Heads2),
	mergeHeads(NewVarHeads, Heads2, NewHeads),
	compileWhens(Loop, NewWhens, Enter, NewHeads, Prelude),
	structWhens(N, Groups, Enter, VarHeads, Table).

% compileListWhens(_, [], Enter, _, _, fail).
compileListWhens(_, [], Enter, VarHeads, Heads, clauses(MatchHeads, _)) :-	% LEE
	mergeHeads(VarHeads, Heads, MatchHeads).
compileListWhens(N, Whens, Enter, VarHeads, Heads,
		gl(N, UArgs, $(Loop) + Prelude)) :-
	Whens = when(Pattern, _)._,
	length(Pattern, NArgs),
	splitWhens(Whens, Patterns, Vars),
	splitHeads(Heads, Labels, Terms),
	termsToArgLists(Terms, Pred, ArgLists),
	( VarHeads == [] ->
		UArgs = [N, NArgs],
		splitArgNList(N, Patterns, NewPatterns),
		NewVarHeads = [],
		splitArgNList(N, ArgLists, NewArgLists),
		relabel(N, Labels, NewLabels)
	;	LArgs is NArgs + 1,
		iotaL(NArgs, LArgs, UArgs),
		splitArgNWithResidueList(N, Patterns, NewPatterns),
		splitHeads(VarHeads, VarLabels, VarTerms),
		termsToArgLists(VarTerms, Pred, VarArgLists),
		extendListList(2, VarArgLists, NewVarArgLists),
		termsToArgLists(NewVarTerms, Pred, NewVarArgLists),
		relabel(fill(2), VarLabels, NewVarLabels),
		reassembleHeads(VarHeads, NewVarLabels, NewVarTerms, NewVarHeads),
		splitArgNWithResidueList(N, ArgLists, NewArgLists),
		relabel(residue(N), Labels, NewLabels)
	),
	splitWhens(NewWhenGroup, NewPatterns, Vars),
	termsToArgLists(NewTerms, Pred, NewArgLists),
	reassembleHeads(Heads, NewLabels, NewTerms, Heads2),
	mergeHeads(NewVarHeads, Heads2, NewHeads),
	compileWhens(Loop, NewWhenGroup, Enter, NewHeads, Prelude).

:- exhaustedWhens(_, Whens) when Whens.
exhaustedWhens(_, []).
exhaustedWhens(Pos, when(Pattern, Vars).Whens) :-
	Vars == ever,
	\+ (
		nth0(N, Arg, Pattern),
		( N == Pos ->
			functor(Arg, _, Arity),
			between(1, Arity, I),
			arg(I, Arg, A),
			nonvar(A)
		;	nonvar(Arg)
		)
	),
	exhaustedWhens(Pos, Whens).

%	Note that clauses with a common constant are not modified
%	as those with a common list or structure are.
indexConstantHeads(_, [], [], clauses([], _)) :-
	!.
indexConstantHeads(_, [], ConstantHeads, clauses(ConstantHeads, _)) :-
	ConstantHeads = [_],
	!.
indexConstantHeads(N, VarHeads, ConstantHeads, Switch) :-
	cons(ConstantHeads),
	( cons(VarHeads) ->
		Switch = soce(N, clauses(VarHeads, _), Table)
	;	Switch = soc(N, clauses([], _), Table)
	),
	!,
	makeConstantTable(N, VarHeads, ConstantHeads, Table).
indexConstantHeads(_, VarHeads, [], clauses(VarHeads, _)).

indexStructHeads(_, [], [], clauses([], _)) :-
	!.
indexStructHeads(_, [], StructHeads, clauses(StructHeads, _)) :-
	StructHeads = [_],
	!.
indexStructHeads(Pos, VarHeads, StructHeads, Switch) :-
	cons(StructHeads),
	( cons(VarHeads) ->
		Switch = sose(Pos, clauses(VarHeads, _), Table)
	;	Switch = sos(Pos, clauses([], _), Table)
	),
	makeStructTable(Pos, VarHeads, StructHeads, Table),
	!.
indexStructHeads(N, VarHeads, [], clauses(VarHeads, _)).

indexListHeads(_, [], [], clauses([], _)) :-
	!.
indexListHeads(N, VarHeads, ListHeads, clauses(Heads, _)) :-
	cons(VarHeads),
	mergeHeads(VarHeads, ListHeads, Heads).
indexListHeads(N, [], ListHeads, gl(N, [N, Arity], clauses(Prelude, _))) :-
	ListHeads = clause(_, _, H, _, _)._._,		% Two or more clauses
	functor(H, _, Arity),
	splitHeadsOnN(N, ListHeads, Prelude).
indexListHeads(_, [], ListHeads, clauses(ListHeads, _)) :-
	ListHeads = [_].

makeConstantTable(Pos, VarHeads, Heads, Table) :-
	sortConstantHeads(Pos, Heads, HeadGroups),
	makeConstantTable1(Pos, VarHeads, HeadGroups, Table).

:- makeConstantTable1(_, _, X, Y) when X or Y.
makeConstantTable1(_, _, [], []).
makeConstantTable1(
		Pos,
		VarHeads,
		group(Const, ConstHeads).Groups,
		entry(Const, clauses(AllHeads, _)).Table) :-
	mergeHeads(ConstHeads, VarHeads, Heads),
	headOpt(Pos, +, Const, Heads, AllHeads),
	makeConstantTable1(Pos, VarHeads, Groups, Table).

makeStructTable(Pos, VarHeads, Heads, Table) :-
	sortStructHeads(Pos, Heads, HeadGroups),
	makeStructTable1(Pos, VarHeads, HeadGroups, Table).

:- makeStructTable1(_, _, X, Y) when X or Y.
makeStructTable1(_, _, [], []).
makeStructTable1(
		Pos,
		VarHeads,
		group(SPred/SArity, StructHeads).Groups,
		entry(SPred, SArity, clauses(AllHeads, _)).Table) :-
	functor(Term, SPred, SArity),
	mergeHeads(StructHeads, VarHeads, Heads),
	headOpt(Pos, +, Term, Heads, AllHeads),
	makeStructTable1(Pos, VarHeads, Groups, Table).

headOpt(Pos, Tag, Term, Heads, NewHeads) :-
	Pos1 is Pos + 1,
	headOpt1(Pos1, Tag, Term, Heads, NewHeads).

:- headOpt1(_, _, _, Heads, _) when Heads.
headOpt1(Pos, Tag, Term, [], []).
headOpt1(Pos, Tag, Term, Head.Heads, NewHeads0) :-
	Head = clause(_, _, H, Body, Cut),
	arg(Pos, H, Arg),
	( Cut = cut(Label),
	  steadfast(Pos, H),
	  \+ bodyDoesntCommit(Arg, Tag, Label, Term, steadfast(Pos, H), Body) ->
		NewHeads0 = [Head]
	; \+ headUnificationUnsafe(Arg, Term, H),
	  bodyImpossible(Arg, Tag, Term, Body) ->
		NewHeads0 = NewHeads
	;	NewHeads0 = Head.NewHeads
	),
	headOpt1(Pos, Tag, Term, Heads, NewHeads).

steadfast(Pos, H) :-
	\+ \+ steadfast1(Pos, H).

steadfast1(Pos, H) :-
	H =.. _.HArgs,
	Pos1 is Pos - 1,
	nth0(Pos1, HArgs, Arg, OtherArgs),
	( var(Arg) ->
		Arg = 0,
		N1 = 1
	;	Arg =.. _.AArgs,
		steadfastNumbering(AArgs, 0, N1)
	),
	steadfastNumbering(OtherArgs, N1, _).

steadfastNumbering([], N, N).
steadfastNumbering(X.XT, N0, N) :-
	var(X),
	X = N0,
	N1 is N0 + 1,
	steadfastNumbering(XT, N1, N).

%	Check if head unification may change the type of Term.
headUnificationUnsafe(Arg, Term, Head) :-
	var(Term),
	var(Arg),
	functor(Head, _, Arity),
	\+ headUnificationSafe(Arity, Head).

%	Binds variables in Head.
headUnificationSafe(N, Head) :-
	( N > 0 ->
		arg(N, Head, X),
		var(X),
		X = N,
		N1 is N - 1,
		headUnificationSafe(N1, Head)
	).

bodyDoesntCommit(Var, Tag, Cut, Term, Steadfast0, Body) :-
	\+ (
		( var(Term) ->
			var(Var),
			(var(Term), Steadfast0) = Steadfast
		;	Steadfast0 = Steadfast
		),
		Var = Term,
		once bodyCommits(Tag, Term, Cut, Steadfast, Body, Commit),
		Commit == true
	).

%	Determine if clause body must execute a cut.
%
%	Succeeds with Commit = true if so,
%	with Commit = short if can't tell,
%	with Commit unbound if body must succeed and doesn't commit,
%	and fails if body must fail.
:- bodyCommits(_, _, _, _, X, _) when X.
bodyCommits(_, _, _, _, _, Commit) :-
	nonvar(Commit),
	!.
bodyCommits(Tag, Term, Cut, Steadfast, (B1 , B2), Commit) :-
	!,
	bodyCommits(Tag, Term, Cut, Steadfast, B1, Commit),
	bodyCommits(Tag, Term, Cut, Steadfast, B2, Commit).
bodyCommits(Tag, Term, Cut, Steadfast, (C -> B1; B2), Commit) :-
	!,
	( bodyCommits(Tag, Term, Cut, Steadfast, C, Commit) ->
		bodyCommits(Tag, Term, Cut, Steadfast, B1, Commit)
	;	bodyCommits(Tag, Term, Cut, Steadfast, B2, Commit)
	).
bodyCommits(Tag, Term, Cut, Steadfast, (B1 ; B2), Commit) :-
	!,
	(	bodyCommits(Tag, Term, Cut, Steadfast, B1, Commit)
	;	bodyCommits(Tag, Term, Cut, Steadfast, B2, Commit)
	).
bodyCommits(Tag, Term, Cut, Steadfast, (B1 -> B2), Commit) :-
	!,
	( bodyCommits(Tag, Term, Cut, Steadfast, B1, Commit) ->
		bodyCommits(Tag, Term, Cut, Steadfast, B2, Commit)
	).
bodyCommits(_, Y, _, _, var(X), _) :-
	X == Y,
	!,
	var(X).
bodyCommits(_, Y, _, _, nonvar(X), Commit) :-
	!,
	( var(X) ->
		X \== Y,
		Commit = short
	).
bodyCommits(Tag, Y, _, _, $icn(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	$icn(X)
	).
bodyCommits(Tag, _, _, _, $ucn(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	$ucn(X)
	).
bodyCommits(Tag, _, _, _, $smallInt(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	$smallInt(X)
	).
bodyCommits(Tag, _, _, _, integer(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	integer(X)
	).
bodyCommits(Tag, _, _, _, atom(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	atom(X)
	).
bodyCommits(Tag, _, _, _, atomic(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	atomic(X)
	).
bodyCommits(_, Y, _, _, const(X), Commit) :-
	!,
	( var(X), X \== Y ->
		Commit = short
	;	const(X)
	).
bodyCommits(Tag, _, _, _, $block(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	$block(X)
	).
bodyCommits(Tag, _, _, _, float(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	float(X)
	).
bodyCommits(Tag, _, _, _, $i32(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	$i32(X)
	).
bodyCommits(Tag, _, _, _, number(X), Commit) :-
	!,
	( ( var(Tag) ; var(X), X \== Y) ->
		Commit = short
	;	number(X)
	).
bodyCommits(_, Y, _, _, cons(X), Commit) :-
	!,
	( var(X), X \== Y ->
		Commit = short
	;	cons(X)
	).
bodyCommits(_, Y, _, _, $list(X), Commit) :-
	!,
	( var(X) ->
		X \== Y
	;	cons(X)
	),
	Commit = short.		% NB
bodyCommits(_, Y, _, _, compound(X), Commit) :-
	!,
	( var(X), X \== Y ->
		Commit = short
	;	compound(X)
	).
bodyCommits(_, Y, _, _, term(X), Commit) :-
	!,
	( var(X), X \== Y ->
		Commit = short
	;	term(X)
	).
bodyCommits(_, Y, _, _, $struct(X), Commit) :-
	!,
	( var(X), X \== Y ->
		Commit = short
	;	$struct(X)
	).
bodyCommits(Tag, _, _, Steadfast, functor(T, F, A), Commit) :-
	!,
	( nonvar(Tag), ( nonvar(T) ; nonvar(F), nonvar(A) ) ->
		functor(T, F, A),
		( call(Steadfast) ->
			true
		;	Commit = short
		)
	;	Commit = short
	).
bodyCommits(Tag, Y, _, _, X1 == X2, Commit) :-
	!,
	termCompare(X1, X2, C),
	( var(C) ->
		Commit = short
	; nonvar(Tag) ->
		C == (=)
	;	sameType(X1, X2),
		Commit = short
	).
bodyCommits(Tag, _, _, _, X1 \== X2, Commit) :-
	!,
	termCompare(X1, X2, C),
	( var(C) ->
		Commit = short
	; nonvar(Tag) ->
		C \== (=)
	; sameType(X1, X2) ->
		Commit = short
	% note that nonvar(C) implies nonvar(X1), nonvar(X2)
	).
bodyCommits(Tag, _, _, _, X1 \= X2, Commit) :-
	!,
	termCompare(X1, X2, C),
	( var(C) ->
		Commit = short
	; nonvar(Tag) ->
		C \== (=)
	; sameType(X1, X2) ->
		Commit = short
	% note that nonvar(C) implies nonvar(X1), nonvar(X2)
	).
bodyCommits(Tag, _, _, Steadfast, X1 = X2, Commit) :-
	!,
	( var(Tag), nonvar(X1), nonvar(X2) ->
		sameType(X1, X2),
		Commit = short
	;	X1 = X2
	),
	( call(Steadfast) ->
		true
	;	Commit = short
	).
/*
bodyCommits(_, _, _, _, $label(_), _) :-
	!.
*/
bodyCommits(_, _, Cut, _, $cutd(X), Commit) :-
	Cut == X,
	!,
	Commit = true.
bodyCommits(_, _, _, _, true, _) :-
	!.
bodyCommits(_, _, _, _, fail, _) :-
	!,
	fail.
bodyCommits(_, _, _, _, $arithPred(Exp), _) :-
	ground(Exp),
	isExpression(Exp),
	!,
	call(Exp).
bodyCommits(Tag, _, _, Steadfast, $is(Exp, X), Commit) :-
	!,
	( ( var(Tag) ; \+ ground(Exp) ; nonvar(X), \+ number(X) ) ->
		Commit = short
	;	X is Exp,
		( call(Steadfast) ->
			true
		;	Commit = short
		)
	).
bodyCommits(_, _, _, _, _, short).

bodyImpossible(Var, Tag, Term, Body) :-
	var(Var),			% nonvars are filtered by the head-selection routines
	\+ (
		Var = Term,
		bodyPossible(Tag, Term, Body, _)
	).

:- bodyPossible(_, _, X, _) when X.
bodyPossible(_, _, _, Short) :-
	nonvar(Short),
	!.
bodyPossible(Tag, Term, (B1 , B2), Short) :-
	!,
	bodyPossible(Tag, Term, B1, Short),
	bodyPossible(Tag, Term, B2, Short).
bodyPossible(Tag, Term, (B1 ; B2), Short) :-
	!,
	(	bodyPossible(Tag, Term, B1, Short)
	;	bodyPossible(Tag, Term, B2, Short)
	).
bodyPossible(Tag, Term, (B1 -> B2), Short) :-
	!,
	% It's a little hard to reason about -> properly.
	bodyPossible(Tag, Term, B1, Short),
	bodyPossible(Tag, Term, B2, Short).
bodyPossible(_, _, var(X), _) :-
	!,
	var(X).
bodyPossible(_, _, $ref(X), _) :-
	!,
	var(X).
bodyPossible(_, _, $del(X), _) :-
	!,
	var(X).
bodyPossible(_, Y, nonvar(X), _) :-
	!,
	( var(X) ->
		Y \== X
	).
bodyPossible(Tag, Y, $icn(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	$icn(X)
	).
bodyPossible(Tag, Y, $ucn(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	$ucn(X)
	).
bodyPossible(Tag, Y, $smallInt(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	$smallInt(X)
	).
bodyPossible(Tag, Y, integer(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	integer(X)
	).
bodyPossible(Tag, Y, atom(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	atom(X)
	).
bodyPossible(Tag, Y, atomic(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	atomic(X)
	).
bodyPossible(_, Y, const(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	const(X)
	).
bodyPossible(Tag, Y, $block(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	$block(X)
	).
bodyPossible(Tag, Y, float(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	float(X)
	).
bodyPossible(Tag, Y, $i32(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	$i32(X)
	).
bodyPossible(Tag, Y, number(X), _) :-
	!,
	( var(Tag), Y == const ->
		true
	; var(X) ->
		Y \== X
	;	number(X)
	).
bodyPossible(_, Y, cons(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	cons(X)
	).
bodyPossible(_, Y, $string(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	cons(X)
	).
bodyPossible(_, Y, $list(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	cons(X)
	).
bodyPossible(_, Y, compound(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	compound(X)
	).
bodyPossible(_, Y, term(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	term(X)
	).
bodyPossible(_, Y, $struct(X), _) :-
	!,
	( var(X) ->
		Y \== X
	;	$struct(X)
	).
bodyPossible(Tag, _, functor(T, F, A), Short) :-
	!,
	( nonvar(Tag), ( nonvar(T) ; nonvar(F), nonvar(A) ) ->
		functor(T, F, A)
	;	Short = short
	).
bodyPossible(Tag, Y, X1 == X2, _) :-
	!,
	( nonvar(Tag) ->
		( var(Y), ( Y == X1, nonvar(X2) ; Y == X2, nonvar(X1)) ->
			fail
		;	true, ($is_eq(X1, X2, true) ; true)
		)
	; var(X1) ->
		true
	; var(X2) ->
		true
	;	sameType(X1, X2)
	).
bodyPossible(Tag, _, X1 \== X2, _) :-
	!,
	( nonvar(Tag) ->
		true, ($is_eq(X1, X2, fail) ; fail)
	).
bodyPossible(Tag, _, X1 \= X2, _) :-
	!,
	( nonvar(Tag) ->
		true, ($is_eq(X1, X2, fail) ; fail)
	).
bodyPossible(Tag, Y, X1 = X2, Short) :-
	!,
	( var(Tag) ->
		( (	var(X1) ; var(X2)) ->
			X1 = X2		% c.f. with case below.  var(Tag) implies nonvar(Y).
		;	sameType(X1, X2)
		)
	; var(Y), occurs(Y, X1.X2) ->
		X1 = X2,
		( var(Y) ->
			% we no longer know whether Y will be a variable at run-time 
			Short = short
		)
	;	X1 = X2
	).
bodyPossible(_, _, $label(_), _) :-
	!.
bodyPossible(_, _, true, _) :-
	!.
bodyPossible(_, _, fail, _) :-
	!,
	fail.
bodyPossible(_, _, $arithPred(Exp), _) :-
	!,
	( ground(Exp), isExpression(Exp) ->
		call(Exp)
	).
bodyPossible(Tag, Y, $is(Exp, X), Short) :-
	!,
	( ( var(Tag) ; \+ ground(Exp) ; nonvar(X), \+ number(X) ) ->
		Short = short
	; var(Y), Y == X ->
		X is Exp,
		( var(Y) ->
			% we no longer know whether Y will be a variable at run-time 
			Short = short
		)
	;	X is Exp
	).
bodyPossible(_, _, _, short).

sameType(X, Y) :-
	var(X), var(Y).
sameType(X, Y) :-
	const(X), const(Y).
sameType(X, Y) :-
	cons(X), cons(Y).
sameType(X, Y) :-
	$struct(X), $struct(Y).

differentType(X, Y) :-
	var(X), nonvar(Y).
differentType(X, Y) :-
	const(X), \+ const(Y).
differentType(X, Y) :-
	cons(X), \+ cons(Y).
differentType(X, Y) :-
	$struct(X), \+ $struct(Y).

%	Split the n'th argument of a list of heads, adjusting the labels.
splitHeadsOnN(N, Heads, SplitHeads) :-
	splitHeads(Heads, Labels, Terms),
	termsToArgLists(Terms, Pred, ArgLists),
	splitArgNList(N, ArgLists, NewArgLists),
	relabel(N, Labels, NewLabels),
	termsToArgLists(NewTerms, Pred, NewArgLists),
	reassembleHeads(Heads, NewLabels, NewTerms, SplitHeads).

:- splitWhens(X, Y, Z) when X or Y or Z.
splitWhens([], [], []).
splitWhens(when(Pattern, Vars).Whens, Pattern.Patterns, Vars.VarsL) :-
	splitWhens(Whens, Patterns, VarsL).

:- splitHeads(X, Y, Z) when X or Y or Z.
splitHeads([], [], []).
splitHeads(clause(_, L, Head, _, _).X, L.Ls, Head.Heads) :-
	splitHeads(X, Ls, Heads).

:- reassembleHeads(W, X, Y, Z) when W or X or Y or Z.
reassembleHeads([], [], [], []).
reassembleHeads(clause(Line, _, _, Body, Cut).OldHeads,
		L.Ls, Head.Heads, clause(Line, L, Head, Body, Cut).NewHeads) :-
	reassembleHeads(OldHeads, Ls, Heads, NewHeads).

:- termsToArgLists(X, _, Y) when X or Y.
termsToArgLists([], _, []).
termsToArgLists(T.TT, Pred, A.AT) :-
	T =.. (Pred.A),
	termsToArgLists(TT, Pred, AT).

:- relabel(_, X, _) when X.
relabel(_, [], []).
relabel(N, L.LT, NL) :-
	(integer(L) ->
		NL = [N, L].NLT,
		relabel(N, LT, NLT)
	;	NL = (N.L).NLT,
		relabel(N, LT, NLT)
	).

expandHead(L, BaseHead, Head) :-
	BaseHead =.. (Pred.BaseArgs),
	expandHead2(L, BaseArgs, Args),
	Head =.. (Pred.Args).

:- expandHead2(X, _, _) when X.
expandHead2([_], Head, Head).
expandHead2(N.L, BaseHead, Head) :-
	integer(N),
	expandHead2(L, BaseHead, H2),
	splitArgN(N, H2, Head).
expandHead2(fill(Fill).L, BaseHead, Head) :-
	expandHead2(L, BaseHead, H2),
	length(Extension, Fill),
	append(H2, Extension, Head).
expandHead2(residue(N).L, BaseHead, Head) :-
	expandHead2(L, BaseHead, H2),
	splitArgNWithResidue(N, H2, Head).

:- choicesOfPrelude(X, _, _) when X.
choicesOfPrelude(clauses(Heads, Label), Clauses0, Clauses) :-
	!,
	Clauses0 = clauses(Heads, Label).Clauses.
choicesOfPrelude(P1+P2, Clauses0, Clauses) :-
	!,
	choicesOfPrelude(P1, Clauses0, Clauses1),
	choicesOfPrelude(P2, Clauses1, Clauses).
choicesOfPrelude(jv(_, CV, CNV), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(CV, Clauses0, Clauses1),
	choicesOfPrelude(CNV, Clauses1, Clauses).
choicesOfPrelude(sot(_, VP, CP, LP, SP), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(VP, Clauses0, Clauses1),
	choicesOfPrelude(CP, Clauses1, Clauses2),
	choicesOfPrelude(LP, Clauses2, Clauses3),
	choicesOfPrelude(SP, Clauses3, Clauses).
choicesOfPrelude(soc(_, Default, Table), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Default, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).
choicesOfPrelude(soce(_, Else, Table), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Else, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).
choicesOfPrelude(sos(_, Default, Table), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Default, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).
choicesOfPrelude(sose(_, Else, Table), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Else, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).
choicesOfPrelude(gl(_, _, Prelude), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Prelude, Clauses0, Clauses).
choicesOfPrelude(gs(_, _, _, Prelude), Clauses0, Clauses) :-
	!,
	choicesOfPrelude(Prelude, Clauses0, Clauses).
choicesOfPrelude(_, Clauses, Clauses).

:- choicesOfTable([], _, _) when ever.
:- choicesOfTable(Entry._, _, _) when Entry.				% Really index
choicesOfTable([], Clauses, Clauses).
choicesOfTable(entry(_, Entries).Table, Clauses0, Clauses) :-
	choicesOfPrelude(Entries, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).
choicesOfTable(entry(_, _, Entries).Table, Clauses0, Clauses) :-
	choicesOfPrelude(Entries, Clauses0, Clauses1),
	choicesOfTable(Table, Clauses1, Clauses).

uniqueChoices(Choices0, Choices) :-
	sort(Choices0, Choices1),
	$bindDuplicateChoices(Choices1, Choices).

:- $bindDuplicateChoices(X, _) when X.
$bindDuplicateChoices([], []).
$bindDuplicateChoices(X.XT, X.YT) :-
	X = clauses(H, L),
	$bindDuplicateChoices(H, L, XT, YT).

:- $bindDuplicateChoices(_, _, X, _) when X.
$bindDuplicateChoices(_, _, [], []).
$bindDuplicateChoices(H1, L1, X2.XT, Y) :-
	X2 = clauses(H2, L2),
	( H1 == H2 ->
		L1 = L2,
		$bindDuplicateChoices(H1, L1, XT, Y)
	;   Y = X2.YT,
		$bindDuplicateChoices(H2, L2, XT, YT)
	).

:- clausesUsed(X, _, _, _, _) when X.
clausesUsed([], A, A, Clauses, Clauses).
clausesUsed(clauses(Heads, _).Choices, A0, A, Clauses0, Clauses) :-
	unfillHeads(Heads, A0, A1, Clauses0, Clauses1),
	clausesUsed(Choices, A1, A, Clauses1, Clauses).

% LEE head/2 is now stripped off earlier
labelsUsed(Labels, UniqLabels) :-
	uniqLabels(Labels, UniqLabels).

uniqLabels(X, Y) :-
	sort(X, Y).

makeMap(List, Map) :-
	makeMapKeys(List, Keys),
	makeKeyedLookupTree(Keys, Map).

:- makeMapKeys(X, _) when X.
makeMapKeys([], []).
makeMapKeys(L.LT, Map) :-
	(integer(L) ->
		makeMapKeys(LT, Map)
	;	makeLabel(L2),
		Map = (L - L2).MapT,
		makeMapKeys(LT, MapT)
	).

mapLabel(L1, Map, L2) :-
	unfill(L1, L1U),
	( integer(L1U) ->
		L1U = L2
	;	lookupKeyedTree(L1U, Map, L2)
	).

:- unfillHeads(A, _, _, _, _) when A.
unfillHeads([], A, A, M, M).
unfillHeads(clause(_, L, H, _, _).Heads, A0, A, L1.M0, M) :-
	functor(H, _, N),
	co$max(N, A0, A1),
	unfill(L, L1),
	unfillHeads(Heads, A1, A, M0, M).

%	strip fill/1 off start of list YUK
unfill(fill(_).L, L1) :-
	!,
	unfill(L, L1).
unfill([N], N) :-
	!.
unfill(L, L).

:- compilePrelude(_, _, _, _, X, _, _) when X.		 			% Really index
compilePrelude(Prelude, Arity, Map, _Whens, no, Code0, Code) :-
	compilePrelude2(Prelude, Arity, Map, Code0, Code).
compilePrelude(Prelude, Arity, Map, Whens, yes(CutVar), Code0, Code) :-
	CutVar1 is CutVar + 1,
	co$max(Arity, CutVar1, Arity1),
	( cons(Whens) ->
		compilePrelude2(Prelude, Arity1, Map, Code0, Code)
	;	Code0 = label(temp(CutVar)).Code1,
		compilePrelude2(Prelude, Arity1, Map, Code1, Code)
	).
compilePrelude(Prelude, Arity, Map, Whens, Cut, _, _) :-
	format(user_error,
		"~N%\tUnable to compile prelude (~w) with ~w, ~w, ~w, ~w.~n",
		[Prelude, Arity, Map, Whens, Cut]),
	fail.

:- compilePrelude2(X, _, _, _, _) when X.
compilePrelude2(clauses(Heads, Label), Arity, Map, Code0, Code) :-
	( var(Label) ->
		splitHeads(Heads, Labels, _),
%		( cons(Heads) ->
%			Code0 = $(Label).Code1
%		;	Code0 = $(Label).Code1
%		),
		Code0 = $(Label).Code1,
		compileCP(Labels, Label, Arity, Map, Code1, Code)
	;	Code0 = j(Label).Code
	).
compilePrelude2(P1+P2, Arity, Map, Code0, Code) :-
	compilePrelude2(P1, Arity, Map, Code0, Code1),
	compilePrelude2(P2, Arity, Map, Code1, Code).
compilePrelude2(label(X), _, _, label(X).Code, Code).
compilePrelude2(nop, _, _, Code, Code).
%compilePrelude2(enter, _, _, Enter, Enter.Code, Code).
compilePrelude2(fvar(V1, V2), _Arity, _,
		fvar(temp(V1), temp(V2)).Code, Code).
compilePrelude2(delay(Vars, Loop), Arity, _,
		mkdel(Arity, Loop).Code, CodeEnd) :-
	compileMarks(Vars, Code, delay.CodeEnd).
compilePrelude2(jv(N, CV, CNV), Arity, Map,
		jv(temp(N), LCV).Code0, Code) :-
	labelOf(CV, LCV, Map),
	compilePrelude2(CNV, Arity, Map, Code0, $(LCV).Code1),
	compilePrelude2(CV, Arity, Map, Code1, Code).
compilePrelude2(j(L), _, _, j(L).Code, Code).
compilePrelude2($(L), _, _, $(L).Code, Code).
compilePrelude2(fail, _, _, fail.Code, Code).
compilePrelude2(sot(N, VP, CP, LP, SP), Arity, Map, 
		sot(temp(N), LV, LC, LL, LS).Code0, Code) :-
	compileSwitchBranch(VP, LV, Arity, Map, Code0, Code1),
	compileSwitchBranch(CP, LC, Arity, Map, Code1, Code2),
	compileSwitchBranch(LP, LL, Arity, Map, Code2, Code3),
	compileSwitchBranch(SP, LS, Arity, Map, Code3, Code).
compilePrelude2(soc(_, clauses([], _), []), _Arity, _Map, 
		fail.Code, Code).
compilePrelude2(soc(_, clauses([], _), [entry(_, Prelude)]), Arity, Map, 
		Code, CodeEnd) :-
	compilePrelude2(Prelude, Arity, Map, Code, CodeEnd).
compilePrelude2(soc(_, Default, []), Arity, Map, Code, CodeEnd) :- % LEE
	compilePrelude2(Default, Arity, Map, Code, CodeEnd).
compilePrelude2(soc(N, Default, Table), Arity, Map, 
		soc(temp(N), DefaultLabel, MTable).Code, CodeEnd) :-
	compileSOC(Table, Arity, Map, MTable, Code, CE1),
	compileSwitchBranch(Default, DefaultLabel, Arity, Map, CE1, CodeEnd).
%		LEE some of these may not be useful
compilePrelude2(soce(_, clauses([], _), []), _Arity, _Map, 
		fail.Code, Code).
compilePrelude2(soce(_, clauses([], _), [entry(_, Prelude)]),
		Arity, Map, Code, CodeEnd) :-
	compilePrelude2(Prelude, Arity, Map, Code, CodeEnd).
compilePrelude2(soce(_, Else, []), Arity, Map, Code, CodeEnd) :-
	compilePrelude2(Else, Arity, Map, Code, CodeEnd).
compilePrelude2(soce(N, Else, [entry(Const, Prelude)]), Arity, Map, 
		[	jc(temp(N), const(Const), Label),
			j(ElseLabel)
		|	Code
		],
		CodeEnd) :-
	compileSwitchBranch(Else, ElseLabel, Arity, Map, Code, $(Label).CE1),
	compilePrelude2(Prelude, Arity, Map, CE1, CodeEnd).
compilePrelude2(soce(N, Else, Table), Arity, Map, 
		soce(temp(N), ElseLabel, MTable).Code, CodeEnd) :-
	compileSOC(Table, Arity, Map, MTable, Code, CE1),
	compileSwitchBranch(Else, ElseLabel, Arity, Map, CE1, CodeEnd).
compilePrelude2(sos(_, clauses([], _), []), _Arity, _Map, 
		fail.Code, Code).
compilePrelude2(sos(_, clauses([], _), [entry(_, _, Prelude)]),
		Arity, Map, Code, CodeEnd) :-
	compilePrelude2(Prelude, Arity, Map, Code, CodeEnd).
compilePrelude2(sos(_, Default, []), Arity, Map, Code, CodeEnd) :- % LEE
	compilePrelude2(Default, Arity, Map, Code, CodeEnd).
compilePrelude2(sos(N, Default, Table), Arity, Map, 
		sos(temp(N), DefaultLabel, MTable).Code, CodeEnd) :-
	compileSOS(Table, Arity, Map, MTable, Code, CE1),
	compileSwitchBranch(Default, DefaultLabel, Arity, Map, CE1, CodeEnd).
%	LEE Same caveat as for soce above.
compilePrelude2(sose(_, clauses([], _), []), _Arity, _Map, 
		fail.Code, Code).
compilePrelude2(sose(_, clauses([], _), [entry(_, _, Prelude)]),
		Arity, Map, Code, CodeEnd) :-
	compilePrelude2(Prelude, Arity, Map, Code, CodeEnd).
compilePrelude2(sose(_, Else, []), Arity, Map, Code, CodeEnd) :-
	compilePrelude2(Else, Arity, Map, Code, CodeEnd).
compilePrelude2(sose(N, Else, [entry(SPred, SArity, Prelude)]),
		Arity, Map, 
		[	js(temp(N), SPred/SArity, Label),
			j(ElseLabel)
		|	Code
		],
		CodeEnd) :-
	compileSwitchBranch(Else, ElseLabel, Arity, Map, Code, $(Label).CE1),
	compilePrelude2(Prelude, Arity, Map, CE1, CodeEnd).
compilePrelude2(sose(N, Else, Table), Arity, Map, 
		sose(temp(N), ElseLabel, MTable).Code, CodeEnd) :-
	compileSOS(Table, Arity, Map, MTable, Code, CE1),
	compileSwitchBranch(Else, ElseLabel, Arity, Map, CE1, CodeEnd).
compilePrelude2(gl(N, [N1, N2], Prelude), Arity, Map, 
		glv2(temp(N), temp(N1), temp(N2)).Code, CodeEnd) :-
	co$max(N1, N2, MaxN),
	MaxN1 is MaxN + 1,
	co$max(MaxN1, Arity, NewArity),
	compilePrelude2(Prelude, NewArity, Map, Code, CodeEnd).
compilePrelude2(gs(N, SPred/SArity, Args, Prelude),
		Arity, Map, Code, CodeEnd) :-
	co$max(MaxN, Args),
	MaxN1 is MaxN + 1,
	co$max(MaxN1, Arity, NewArity),
	compilePreludeGS(N, Args, SPred, SArity, Code, CE1),
	compilePrelude2(Prelude, NewArity, Map, CE1, CodeEnd).

:- compilePreludeGS(_, _, _, A, _, _) when A.				% Really index.
compilePreludeGS(N, [N1, N2], Pred, 2) -->
	[gs2(temp(N), temp(N1), temp(N2), Pred/2)].
compilePreludeGS(N, [N1], Pred, 1) -->
	[gs1(temp(N), temp(N1), Pred/1)].
compilePreludeGS(N, Args, Pred, Arity) -->
	{ Arity > 2 },
	[gs(temp(N), Pred/Arity)],
	compilePreludeArgs(Args).

:- compilePreludeArgs(X, _, _) when X.
compilePreludeArgs([]) -->
	[].
compilePreludeArgs(Arg1.Args) -->
	[uvar(temp(Arg1))],
	compilePreludeArgs(Args).

compileSwitchBranch(clauses(_, L), L, _, _, Code, Code) :-
	nonvar(L),
	!.
compileSwitchBranch(clauses([clause(_, Label, _, _, _)], _),
		L, _, Map, Code, Code) :-
	mapLabel(Label, Map, L),
	!.
compileSwitchBranch(Prelude, Label, Arity, Map, 
		$(Label).Code0, Code) :-
	compilePrelude2(Prelude, Arity, Map, Code0, Code).

:- compileSOC(X, _, _, Y, _, _) when X or Y.
compileSOC([], _, _, [], Code, Code).
compileSOC(entry(Const, Prelude).Table, Arity, Map,
		entry(Const, Label).MTable, Code0, Code) :-
	compileSwitchBranch(Prelude, Label, Arity, Map, Code0, Code1),
	compileSOC(Table, Arity, Map, MTable, Code1, Code).

:- compileSOS(X, _, _, Y, _, _) when X or Y.
compileSOS([], _, _, [], Code, Code).
compileSOS(entry(SPred, SArity, Prelude).Table, Arity, Map,
		entry(SPred, SArity, Label).MTable, Code, CodeEnd) :-
	compileSwitchBranch(Prelude, Label, Arity, Map, Code, CE1),
	compileSOS(Table, Arity, Map, MTable, CE1, CodeEnd).

:- compileCP(X, _, _, _, _, _) when X.
compileCP([], T, _, _, fail.Code, Code) :-
	makeLabel(T).
compileCP(Label.LT, T, Arity, Map, Code0, Code) :-
	mapLabel(Label, Map, L),
	( cons(LT) ->
		makeLabel(T),
		Code0 = t(Arity, L).Code1,
		compileCP1(LT, Arity, Map, Code1, Code)
	;	% Don't makeLabel(T) -- leave it to the optimizer to decide
		Code0 = j(L).Code
	).

:- compileCP1(_.X, _, _, _, _) when X.
compileCP1(Label.LT, Arity, Map, C0, Code) :-
	mapLabel(Label, Map, L),
	( cons(LT) ->
		C0 = r(Arity, L).C1,
		compileCP1(LT, Arity, Map, C1, Code)
	;	LT = [],
		C0 = tr(Arity, L).Code
	).

:- mergeHeads(X, Y, _) when X and Y.		% Not quite correct.
mergeHeads([], Heads2, Heads) :-
	!,
	Heads2 = Heads.
mergeHeads(Heads1, [], Heads) :-
	!,
	Heads1 = Heads.
mergeHeads(H1.HT1, H2.HT2, In) :-
	H1 = clause(_, L1, _, _, _),
	baseLabel(L1, BL1),
	H2 = clause(_, L2, _, _, _),
	baseLabel(L2, BL2),
	( BL1 < BL2 ->
		In = H1.Out,
		mergeHeads(HT1, H2.HT2, Out)
	;	In = H2.Out,
		mergeHeads(H1.HT1, HT2, Out)
	).

:- compileMarks(X, _, _) when X.
compileMarks([], Code, Code).
compileMarks(V.VT, mark(temp(V)).Code, CodeEnd) :-
	compileMarks(VT, Code, CodeEnd).

labelOf(j(Label), L, _) :-
	!,
	Label = L.
labelOf([clause(_, Label, _, _, _)], L, Map) :-
	!,
	mapLabel(Label, Map, L).
labelOf(_, L, _) :-
	makeLabel(L).

baseLabel(Label, BaseLabel) :-
	(integer(Label) ->
		Label = BaseLabel
	;	co$lastElement(Label, BaseLabel)
	).

:- co$lastElement(_.X, _) when X.
co$lastElement([X], X).
co$lastElement(_.X, Y) :-
	co$lastElement(X, Y).

%	Clean up the code for preludes somewhat.
optimizePrelude(C, O) :-
	optimizePrelude2(C, D),
	once
	(	redundantPrelude(D, O)
	;   O = D
	).

:- optimizePrelude2([], _) when ever.
:- optimizePrelude2(X._, _) when X.			% Not really correct.
optimizePrelude2([], []).
optimizePrelude2(jv(Var, L1).j(L2).$(L1).CT, jnv(Var, L2).$(L1).OT) :-
	!,
	optimizePrelude2(CT, OT).
optimizePrelude2(jnv(Var, L1).j(L2).$(L1).CT, jv(Var, L2).$(L1).OT) :-
	!,
	optimizePrelude2(CT, OT).
optimizePrelude2(j(L1).$(L1).CT, $(L1).OT) :-
	!,
	optimizePrelude2(CT, OT).
optimizePrelude2(C.CT, C.OT) :-
	optimizePrelude2(CT, OT).

:- redundantPrelude(X._, _) when X.						% Really index.
redundantPrelude([j(_)], []).
redundantPrelude(label(L).P, label(L).Q) :-
	redundantPrelude(P, Q).
redundantPrelude($(L).P, $(L).Q) :-
	redundantPrelude(P, Q).
