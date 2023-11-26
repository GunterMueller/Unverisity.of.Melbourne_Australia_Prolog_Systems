/*
 * dsimc.nl  -  external database interface for DSIMC
 *
 * Copyright (C) 1986, The University of Melbourne
 */


%## $dsimc_newrel('LIB/db.NUVERSION/simc/newrel').
$dsimc_newrel('/mip/jas/lib/nuprolog/db/simc/newrel').

% Builtins

$dsimc_open(A,B,C,D) :- $dsimc_open(A,B,C,D).
$dsimc_sfbquery(A,B,C,D,E) :- $dsimc_sfbquery(A,B,C,D,E).
$dsimc_query(A,B,C,D) :- $dsimc_query(A,B,C,D).
$dsimc_cv(A,B,C) :- $dsimc_cv(A,B,C).

%
% Map some DSIMC preds to the corresponding SIMC preds
%
$dsimc_next(Trans, Ans) :- $simc_next(Trans, Ans).
$dsimc_delete(Trans) :- $simc_delete(Trans).
$dsimc_end(Trans) :- $simc_end(Trans).

%
% provide a list of choice vectors for calculating sfb vectors
%
$dsimc_choice_vecs(Qbuffers, Query, ChoiceVecs) :-
	termToString([cons,string,quote,prefix], Query, Qstring),
	$dsimc_cv(Qbuffers, Qstring, CVstring),
	sread(CVstring, ChoiceVecs).

%
% get answers to a query, one at a time using sfbvector
%
$dsimc_sfbretrieve(Qbuffers, Query, Sfbvector) :-
	termToString([cons,string,quote,prefix], Query, QString),
	$dsimc_sfbquery(Qbuffers, QString, Sfbvector, 4, Qtrans),
	% BUG!  Should be 4.
	$oncut(2,Qtrans),		/* temporarily call $simc_abort */
%	$oncut(4,Qtrans),		/* call $dsimc_abort if aborted */
	$dsimc_sfbretrieve1(Qtrans, Query).

$dsimc_sfbretrieve1(Qtrans, Query) :-
	repeat,
	$dsimc_next(Qtrans, Ans),
%	$pstot(Ans, Answer),
	sread(Ans, Answer),
	(	Answer = dbEnd,
		$dsimc_end(Qtrans),
		!,
		fail
	;	% check for valid match and
		% set up answer bindings
		Query = Answer
	).

%
% get answers to a query, one at a time
%
$dsimc_retrieve(Db, Query) :-
	functor(Query, Qfunctor, Qarity),
	$dsimc_open(Db, Qfunctor, Qarity, Qbuffers),
	termToString([cons,string,quote,prefix], Query, QString),
	$dsimc_query(Qbuffers, QString, 4, Qtrans),
	% BUG!  Should be 4.
	$oncut(2,Qtrans),		/* temporarily call $simc_abort */
%	$oncut(4,Qtrans),		/* call $dsimc_abort if aborted */
	$dsimc_retrieve1(Qtrans, Query).

$dsimc_retrieve1(Qtrans, Query) :-
	repeat,
	$dsimc_next(Qtrans, Ans),
%	$pstot(Ans, Answer),
	sread(Ans, Answer),
	(	Answer = dbEnd,
		$dsimc_end(Qtrans),
		!,
		fail
	;	% check for valid match and
		% set up answer bindings
		Query = Answer
	).

%
% $dsimc_insert:
%	add a new record to the database
%
$dsimc_insert(D, Fact) :-
	termToString([cons,string,quote,prefix], Fact, Fstring),
	!,
	$simc_assert(D, Fstring).
/*
$dsimc_insert(Db, Fact) :-
	termToString([cons,string,quote,prefix], Fact, Fstring),
	functor(Fact, Ffunctor, Farity),
	$dsimc_open(Db, Ffunctor, Farity, Fbuffers),
	!,
	$dsimc_assert(Fbuffers, Fstring).
*/

%
% dsimc_retract:
%	delete matching records one by one from database
%
$dsimc_retract(Db, Fact) :-
	functor(Fact, Ffunctor, Farity),
	$dsimc_open(Db, Ffunctor, Farity, Fbuffers),
	termToString([cons,string,quote,prefix], Fact, Fstring),
	$dsimc_query(Fbuffers, Fstring, 1, Ftrans),
	% BUG!  Should be 4.
	$oncut(2,Ftrans),		/* call $dsimc_abort if aborted */
	$dsimc_retract1(Ftrans, Fact).

$dsimc_retract1(Ftrans, Fact) :-
	repeat,
	$dsimc_next(Ftrans, Q1),
	sread(Q1, Q),
	(	Q = dbEnd,
		$dsimc_end(Ftrans),
		!,
		fail
	;	Fact = Q,
		$dsimc_delete(Ftrans)
	).

$dsimc_rall(D, P) :-
	$dsimc_retract(D, P), fail.
$dsimc_rall(_, _).
/*
%
% dsimc_rall:
%	delete all matching records from database
%	invoked by ``retractall(DB_relation(args)).''
%
$dsimc_rall(D, Term) :-
	functor(Term, Tfunctor, Tarity),
	$dsimc_open(Db, Tfunctor, Tarity, Tbuffers),
	termToString([cons,string,quote,prefix], Term, Tstring),
	$dsimc_query(Tbuffers, Tstring, 1, Ttrans),
	% BUG!  Should be 4.
	$oncut(2,Ttrans),		% call $dsimc_abort if aborted
	$dsimc_rall1(Ttrans).

$dsimc_rall1(Ttrans) :-
	repeat,
	$dsimc_next(Ttrans, Q1),
	sread(Q1, Q),
	(	Q = dbEnd,
	;	P = Q,
		$dsimc_delete(Ttrans),
		fail
	),
	$dsimc_end(Ttrans).
*/

%
% $dsimc_define:
%	create a clean, new relation by calling the "newrel" program
%	"newrel Db Rel dsimc Br Bs Kr Ks Ur Us Nr Lc Nrecs Avrec"
%
$dsimc_define(Db, Functor, Arity) :-
	dbParam(Db, Functor, Arity, br = Br),
	dbParam(Db, Functor, Arity, bs = Bs),
	dbParam(Db, Functor, Arity, kr = Kr),
	dbParam(Db, Functor, Arity, ks = Ks),
	dbParam(Db, Functor, Arity, ur = Ur),
	dbParam(Db, Functor, Arity, us = Us),
	dbParam(Db, Functor, Arity, nr = Nr),
	dbParam(Db, Functor, Arity, lc = Lc),
	dbParam(Db, Functor, Arity, nrec = Nrecs),
	dbParam(Db, Functor, Arity, avrec = Avrec),
	dbParam(Db, Functor, Arity, template = TemplateL),
	name(Template, TemplateL),
	$dsimc_newrel(Newrel),
	$sprintf(Cmd, "% '%' '%' % % % % % % % % % % % '%'", [Newrel,Db,Functor,dsimc,Br,Bs,Kr,Ks,Ur,Us,Nr,Lc,Nrecs,Avrec,Template]),
%dbug	writeln(user, Cmd),
	system(Cmd, 0).

%
% dsimc_undefine:
%	Remove a relation
%
$dsimc_undefine(Db, R) :-
	append("rm -rf ", Db, S1),
	name(R, RS),
	append(S1, 0'/.RS, Cmd),
%dbug	writeln(user, Cmd),
	system(Cmd, 0).

%
% $optimize/2
%
%	goal _2 is goal _1 optimized by reordering and superjoin
%
$optimize(GoalIn, GoalOut) :-
	$goalToList(GoalIn, InList),
	duplicate(InList, CopyList),
	$reorder(InList, CopyList, OutList),
	$goalToList(GoalReordered, OutList),
	$db_trf(GoalReordered, GoalOut).

%
% $reorder/3
%
%	conjunction _3 is conjunction _1 reordered
%	_2 must be a duplicate of _1
%
$reorder(In.[], Copy.[], In.[]) :- !.
$reorder(InList, CopyList, Elem.OutList) :-
	$minCost(InList, CopyList, OutList1, CopyOutList, Elem, CopyElem, Cost),
	$makevarsground(CopyElem),
	$reorder(OutList1, CopyOutList, OutList).

%
% $makevarsground/1
%
%	any variables in the term _1 are unified with the constant $ground
%
$makevarsground(A) :-
	var(A),
	!,
	A = $ground.
$makevarsground(A) :-
	atomic(A),
	!.
$makevarsground(A) :-
	A =.. B.C,
	$makevarlistground(C).

%
% $makevarsground/1
%
%	any variables in the list of terms _1 are unified with the constant $ground
%
?- $makevarlistground(X) when X.
$makevarlistground([]).
$makevarlistground(A.B) :-
	$makevarsground(A),
	$makevarlistground(B).
%
% $estCost/2
%
%	_2 is an estimate of the "cost" of expr _1
%	estimate is (approx) based on expected number of solutions
%
$estCost(Expr, Estimate) :-
	$onDisc(Db, Expr, dsimc),
	!,
	functor(Expr, Functor, Arity),
	Expr =.. Functor.Args,
	$convars(Args, ConvArgs),
	dbParam(Db, Functor, Arity, solns(ConvArgs) = Estimate).
$estCost((Expr1 ; Expr2), Estimate) :-
	maxint(Estimate),
	!.
$estCost(RelExpr, Cost) :-
	RelExpr =.. RelOp.Exprs,
	member(RelOp, [<, =<, >, >=, =:=, =\=, and, or]),
	(
		ground(Exprs),
		Cost = 0
	;
		maxint(Cost)
	),
	!.
$estCost(Expr, 1).

%
% $convars/2
%
%	list _2 is list _1 with ground terms replaced by 0
%	and nonground terms replaced with 1
%
$convars([], []).
$convars(A.B, 0.C) :-
	ground(A),
	!,
	$convars(B, C).
$convars(A.B, 1.C) :-
	$convars(B, C).

%
% $goalToList/2
%
%	converts conjunctive goal _1 to/from list of subgoals _2
%
$goalToList(Goal, List) :-
	\+ var(Goal),
	Goal = (X,Y),
	!,
	$goalToList(Y, Y1),
	List = X.Y1.
$goalToList(X, X.[]) :- !.
$goalToList(Goal, List) :-
	\+ var(List),
	List = X.Y,
	!,
	$goalToList(Y1, Y),
	Goal = (X, Y1).
%
% $mincost/7
%
%	_1 must be a list of goals
%	_2 must be a duplicate of _1
%	_3 is _1 with the element which has the minimum cost removed
%	_4 is a duplicate of _3
%	_5 is the removed element
%	_6 is the duplicate of the removed element
%	_7 is the estimated cost of the removed element
%
$minCost(In.[], Copy.[], [], [], In, Copy, Cost) :-
	$estCost(Copy, Cost).
$minCost(In.IList, Copy.CList, OutList, CopyOutList, Elem, CopyElem, Cost) :-
	$minCost(IList, CList, MinList, CopyMinList, E, CopyE, ECost),
	$estCost(Copy, InCost),
	(if ECost < InCost then
		Elem = E,
		CopyElem = CopyE,
		Cost = ECost,
		OutList = In.MinList,
		CopyOutList = Copy.CopyMinList
	else
		Elem = In,
		CopyElem = Copy,
		Cost = InCost,
		OutList = IList,
		CopyOutList = CList
	).
/*
 * dbtrf  -  external database interface predicates for superjoin
 *
 */

%--dsimcQuery/1

?- dsimcQuery(Expr) when Expr.

dsimcQuery(Expr) :-
	$db_trf(Expr, Pools, Vs, Bits, DbVars, DbSizes, Expr_trf),
	$sfb(Vs, Bits, DbVars, DbSizes, Sfb, TotCost),
	!,
	printf(user, "May require up to %d buffers\n", [TotCost]),
	$db_sfbgenerate(Pools, Sfb),
	Expr_trf.
dsimcQuery(Expr).

%--$db_trf/2
$db_trf(InExpr, OutExpr) :-
	$db_trf(InExpr, Pools, Vs, Bits, DbVars, DbSizes, Expr_trf),
	(
					% didnt find any dsimc predicates
		Bits = [],
		OutExpr = InExpr,
		!
	;
		$sfb(Vs, Bits, DbVars, DbSizes, Sfb, TotCost),
		!,
		printf(user, "May require up to %d buffers\n", [TotCost]),
		OutExpr = ($db_sfbgenerate(Pools, Sfb), Expr_trf)
	).

%--$db_trf/7

?- $db_trf(Expr, Pools, Vs, Bits, DbVars, DbSizes, Expr_trf) when Expr.

$db_trf((Expr1, Expr2), Pools, Vs, Bits, DbVars, DbSizes, (Expr1_trf, Expr2_trf)) :-
	!,
	$db_trf(Expr1, Pools1, Vs1, Bits1, DbVars1, DbSizes1, Expr1_trf),
	$db_trf(Expr2, Pools2, Vs2, Bits2, DbVars2, DbSizes2, Expr2_trf),
	append(Pools1, Pools2, Pools),
	append(DbVars1, DbVars2, DbVars),
	append(DbSizes1, DbSizes2, DbSizes),
	$common(Vs1, Vs2, Vs, Bits1, Bits2, Bits).

$db_trf((Expr1; Expr2), Pools, Vs, Bits, DbVars, DbSizes, (Expr1_trf; Expr2_trf)) :-
	!,
	$db_trf(Expr1, Pools1, Vs1, Bits1, DbVars1, DbSizes1, Expr1_trf),
	$db_trf(Expr2, Pools2, Vs2, Bits2, DbVars2, DbSizes2, Expr2_trf),
	append(Pools1, Pools2, Pools),
	append(DbVars1, DbVars2, DbVars),
	append(DbSizes1, DbSizes2, DbSizes),
	$common(Vs1, Vs2, Vs, Bits1, Bits2, Bits).
	
	
$db_trf(Goal, Pool.[], DbVars, VBits, DbVars.[], VSizes.[], $$dsimc_sfbretrieve(Pool, Goal, Sfb)) :-
	$onDisc(D, Goal, dsimc),
	!,
	functor(Goal, Functor, Arity),
	$dsimc_open(D, Functor, Arity, Pool),
	$dsimc_choice_vecs(Pool, Goal, Choicevecs),
	$get_cv_term(Goal, Choicevecs, DbVars, VBits, VSizes, Sfb).
	

$db_trf(Goal, [], [], [], [], [], Goal).

%--$get_cv_term/6

$get_cv_term(Term, Choicevecs, [], [], [], []) :-
	atomic(Term),
	!.
$get_cv_term(Term, (CvUsed, CvExtra), DbVars, VBits.[], VSizes.[], Sfb) :-
	var(Term),
	!,
	DbVars = Term.[],
	$get_cv_vars(CvUsed, VUsed, VSizes),
	$get_cv_vars(CvExtra, VExtra, _),
	append(VUsed, VExtra, VBits),
	append(CvUsed, CvExtra, Sfb).
$get_cv_term(Term, Choicevecs, DbVars, VBits, VSizes, Sfb) :-
	Term =.. F.List,
	!,
	Choicevecs =.. F.CvList,
	$get_cv_list(List, CvList, DbVars, VBits, VSizes, Sfb).

$get_cv_vars([], [], 0).
$get_cv_vars(Cv.Choicevecs, Cv.VBits, VSizes) :-
	var(Cv),
	$get_cv_vars(Choicevecs, VBits, VSize),
	VSizes is VSize + 1.
$get_cv_vars(Cv.Choicevecs, VBits, VSize) :-
	nonvar(Cv),
	$get_cv_vars(Choicevecs, VBits, VSize).

$get_cv_list([], [], [], [], [], []).
$get_cv_list(L.List, Cv.Choicevecs, DbVars, VBits, VSizes, Sfb) :-
	$get_cv_term(L, Cv, DbV, Vb, Vsz, S),
	$get_cv_list(List, Choicevecs, DbVs, Vbs, Vszs, Ss),
	append(DbV, DbVs, DbVars),	% prob should check for common vars
	append(Vb, Vbs, VBits),
	append(Vsz, Vszs, VSizes),
	$combine_sfb(S, Ss, Sfb).

$combine_sfb(Sfb, [], Sfb) :- !.
$combine_sfb([], Sfb, Sfb) :- !.
$combine_sfb(Sfb1, Sfb2, Sfb) :-
	$combine(Sfb1, Sfb2, Sfb).

$combine([], [], []).
$combine(S1.Sfb1, S2.Sfb2, S.Sfb) :-
	(
		S1 == 0'*,
		S = S2
	;
		S2 == 0'*,
		S = S1
	),
	$combine(Sfb1, Sfb2, Sfb).


%--$$dsimc_sfbretrieve/4

$$dsimc_sfbretrieve(Pool, Goal, Sfb) :-
	$sfbconv(Sfb, SfbString),
	$dsimc_sfbretrieve(Pool, Goal, SfbString).

%--$sfbconv/3

$sfbconv([], []).
$sfbconv(S.Sfb, SS.SfbString) :-
	(
		var(S),
		SS = 0'*
	;
		nonvar(S),
		SS = S
	),
	$sfbconv(Sfb, SfbString).
	
%--$common/6

$common([], Vs, Vs, [], Bits, Bits).
$common(V1.Vs1, Vs2, V1.Vs, B1.Bits1, Bits2, V1Bits.Bits) :-
	$remove(V1, Vs2, Vs2left, B1, Bits2, Bits2left, V1Bits),
	$common(Vs1, Vs2left, Vs, Bits1, Bits2left, Bits).
	
%--$remove/7

$remove(Var, [], [], Bits, [], [], Bits).
$remove(V, Var.VarL, VarL, B, Bits.BitsL, BitsL, VBits) :-
	V == Var,
	$linkbits(B, Bits, VBits).
$remove(V, Var.VarL, Var.VarLleft, B, Bits.BitsL, Bits.BitsLleft, VBits) :-
	V \== Var,
	$remove(V, VarL, VarLleft, B, BitsL, BitsLleft, VBits).

%--$linkbits/2

$linkbits(Bits, [], Bits).
$linkbits([], Bits, Bits).
$linkbits(B.Bits1, B.Bits2, B.Bits) :-
	$linkbits(Bits1, Bits2, Bits).

%--$db_sfbgenerate/2

$db_sfbgenerate(Pools, []).
$db_sfbgenerate(Pools, 0'0.Sfb) :-
	$db_sfbgenerate(Pools, Sfb).
$db_sfbgenerate(Pools, 0'1.Sfb) :-
	$db_sfbgenerate(Pools, Sfb).


%
%	Calculate sfb-vector for superjoin (greedy algorithm)
%
%	?- $sfb([v,...],[[x,...],...],[[n,...],...],Sfb,Cost).
%
%			[v,...] is list of variables in expression
%			[[x,...],...] is list of variables in each predicate
%			[[n,...],...] is list of sizes of attributes in
%					choice vectors in each predicate
%			Sfb is the resulting sfb-vector
%			Cost is the resulting number of buffer pages
%

$sfb([], [], [], [], [], 0).
$sfb(Vs, Bits, Preds, Sizes, Bit.Sfb, TotCost) :-
	$choose(Vs, Preds, NewPreds, Sizes, NewSizes, Choice, FixedCost, FreeCost),
	$allocbit(Vs, Bits, NewBits, Choice, Bit),
	length(NewPreds, L),
	(if FreeCost > L then
		$sfb(Vs, NewBits, NewPreds, NewSizes, Sfb, Cost),
		TotCost is FixedCost + Cost
	else
		Sfb = [],
		TotCost is FixedCost + FreeCost
	).

$allocbit(V.Vs, [VBit|VBits].Bits, VBits.Bits, Choice, VBit) :-
	V == Choice.
$allocbit(V.Vs, VBits.Bits, VBits.NewBits, Choice, XBit) :-
	$allocbit(Vs, Bits, NewBits, Choice, XBit).
$allocbit([], _, _, _, _) :-
	writeln(user_error, "$allocbit: panic"),
	exit(1).

%
% choose next bit for sfb-vector from list V.Vs so that cost is minimized
%	 when that variable is used for partitioning
%
$choose(V.[], Preds, NewPreds, Sizes, NewSizes, V, FixedCost, FreeCost) :-
	$partition(V, Preds, NewPreds, Sizes, NewSizes, FixedCost, FreeCost).
$choose(V.Vs, Preds, NewPreds, Sizes, NewSizes, Choice, FixedCost, FreeCost) :-
	$choose(Vs, Preds, XPreds, Sizes, XSizes, X, XFixedC, XFreeC),
	Xcost is XFixedC + XFreeC,
	$partition(V, Preds, VPreds, Sizes, VSizes, VFixedC, VFreeC),
	Vcost is VFixedC + VFreeC,
	(if Vcost =< Xcost; Vcost = Xcost, VFixedC =< XFixedC then
		Choice = V,
		NewPreds = VPreds,
		NewSizes = VSizes,
		FixedCost = VFixedC,
		FreeCost = VFreeC
	else
			% Vcost > Xcost; Vcost = Xcost, VFixedC > XFixedC
		Choice = X,
		NewPreds = XPreds,
		NewSizes = XSizes,
		FixedCost = XFixedC,
		FreeCost = XFreeC
	).

%
% partition join using V
%
$partition(V, [], [], [], [], 0, 0).
$partition(V, P.Preds, NewPreds, S.Sizes, NewSizes, FixedCost, FreeCost):-
	$partition(V, Preds, NewPs, Sizes, NewSs, FixedC, FreeC),
		% (if some Cost $pred_partition(V, P, S, PartitionSize) then
	( $pred_partition(V, P, S, PartitionSize) ->
		NewPreds = P.NewPs,
		NewSizes = PartitionSize.NewSs,
		FixedCost = FixedC,
		$sum(PartitionSize, Sum),
		FreeCost is FreeC + 1 << Sum
	% else
	;
		/* no arguments of P match with V so make P fixed
		   and remove from further partitioning */
		NewPreds = NewPs,
		NewSizes = NewSs,
		$sum(S, Sum),
		FixedCost is FixedC + 1 << Sum,
		FreeCost = FreeC
	).

%
% partition a predicate using V
%
$pred_partition(V, Arg.Args, Size.Sizes, NewSize.NewSizes) :-
	V == Arg,
	/* this argument matches */
	(if (Size > 0) then
		NewSize is Size - 1
	else
		NewSize is 0
	),
	% (if some Newsizes1 $pred_partition(V, Args, Sizes, NewSizes1) then
	( $pred_partition(V, Args, Sizes, NewSizes1) ->
		/* other arguments of this predicate also match */
		NewSizes = NewSizes1
	% else
	;
		/* no other arguments of this predicate match */
		NewSizes = Sizes
	).
$pred_partition(V, Arg.Args, Size.Sizes, Size.NewSizes) :-
	V \== Arg,
/*
	(
		V \== Arg
	;
		Size = 0
	),
*/
	/* this argument doesnt match - see if any other matches */
	$pred_partition(V, Args, Sizes, NewSizes).

%
%	Sum a list of integers
%
$sum([], 0).
$sum(L.List, Sum) :-
	$sum(List, S),
	Sum is S + L.

%
% timing preds
%

startQstats(Time0) :-
	statistics(Stats),
	member(time=Time0._, Stats),
	remprop(qstats, open),
	remprop(qstats, unparse),
	remprop(qstats, startup),
	remprop(qstats, scan),
	remprop(qstats, parse),
	addprop(qstats, open, 0),
	addprop(qstats, unparse, 0),
	addprop(qstats, startup, 0),
	addprop(qstats, scan, 0),
	addprop(qstats, parse, 0),
	!.

endQstats(Time0) :-
	statistics(Stats),
	member(time=Time1._, Stats),
	Time is Time1 - Time0,
	getprop(qstats, open, OpenTime),
	getprop(qstats, unparse, UnparseTime),
	getprop(qstats, startup, StartupTime),
	getprop(qstats, scan, ScanTime),
	getprop(qstats, parse, ParseTime),
	printf(user, "%8s %8s %8s %8s %8s %8s %8s\n",
		["Time","total","open","unparse","startup","index","parse"]),
	printf(user, "%8s %8d %8d %8d %8d %8d %8d (pstot)\n",
		["(ms)",Time,OpenTime,UnparseTime,StartupTime,ScanTime,ParseTime]).

?- startPhase(P,T) when P.
startPhase(Phase, Time) :-
	statistics(Stats),
	member(time=Time._, Stats).

?- endPhase(P,T) when P and T.
endPhase(Phase, Time0) :-
	statistics(Stats),
	member(time=Time1._, Stats),
	Time is Time1 - Time0,
	once getprop(qstats, Phase, SoFar),
	ThusFar is SoFar + Time,
	putprop(qstats, Phase, ThusFar).
