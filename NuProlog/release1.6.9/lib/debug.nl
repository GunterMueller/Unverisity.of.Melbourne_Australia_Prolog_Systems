%	Copyright (C) 1988, The University of Melbourne
%
%	NU-Prolog debugging library
%
%	Authors: Philip Dart, Giles Lean, Lee Naish, Justin Zobel
%	February 1988


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					%
%	Initializing Predicates		%
%					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%?- dynamic processGoal/4.		% For user-defined goal processing.
% check instead for existence with currentPredicate/2

%	dConsult(File)
%	Like consult/1 but clauses are saved in the debugging area and do
%	not effect other code.

dConsult([]).
dConsult(File.Rest) :-
	dConsult(File),
	dConsult(Rest).
dConsult(File) :-
	atom(File),
	( File = user
	->	File1 = user
	;	applySuffix(File, ".nl", _, File1),
		( open(File1, read, Stream)
		->	format(user_error, "~a being loaded ...\n", [File1]),
			currentInput(Current),
			setInput(Stream)
		;	format(user_error, "~a not found.\n", [File1]),
			fail
		)
	),
	$remove_at(File1, OrigFile),
	remprop($debug, $dSeen),	% dConsulting new file
	repeat,
	readTerm(Term, NameList, VarList),
	( isEof(Term)
	->	( File1 = user
		->	clearIOError(user_input)
		;	clearIOError(Stream)
		)
	;	once (
			expandTerm(Term, Term1),
			$dProcessTerm(OrigFile, Term1, NameList, VarList)
		),
		fail
	),
	!,
	( File1 \= user -> close(Stream), setInput(Current) ),
	%
	% maybe should make this public - used by nit, for instance
	( \+ getprop($debug, $dConsulted, OrigFile)
	->	addprop($debug, $dConsulted, OrigFile)
	),
	( getprop($debug, $calls, $junk)
	->	% call graph now out of date so fix it
		forall( ( getprop($debug, $dSeen, F/A),
			  dDCalls(F, A, Calls),
			  member(F1/A1, Calls)
			  ),
			( dProp(F1, A1, $dDCed, AC)
			->	dPutProp(F1, A1, $dDCed, (F/A).AC)
			;	dPutProp(F1, A1, $dDCed, (F/A).[])
		  	)
		),
		forall( getprop($debug, $dSeen, F/A), (
			dProp(F, A, $dDCed, C),
			sort(C, C1),
			dPutProp(F, A, $dDCed, C1)
		))
	),
	putl(user_error, "done.\n").

%	dLoad(File)
%	Same as dConsult/1, but if File.no exists it is also loaded.

dLoad([]).
dLoad(File.Rest) :-
	dLoad(File),
	dLoad(Rest).
dLoad(File) :-
	atom(File),
	applySuffix(File, ".nl", File1, File1S),
	applySuffix(File1, ".no", _, File2),
	( stat(File1S, Status)			% modified time of File.nl
	->	member(mtime = Time1, Status),
		( stat(File2, Status2)		% modified time of File.no
		->	member(mtime = Time2, Status2),
			( Time1 =< Time2
			->	format(user_error,
					"Loading ~a ...\n", [File2]),
				load(File2),
				putl(user_error, "done.\n")
			;	format(user_error,
					"~a out of date ... not loaded\n",
					[File2])
			)
		;	% File.no not found
			format(user_error, "~a not found.\n", [File2])
		),
		dConsult(File1)
	;	% File.nl not found
		format(user_error, "~a not found.\n", [File1S])	
	).

$dProcessTerm(File, Term, _NameList, _VarList) :-
	var(Term),
	!,
	currentInput(S), lineCount(S, LC),
	format(user_error,
		"Error: variable term in ~a about line ~d.\n", [File, LC]).
$dProcessTerm(File, (:- G), NameList, VarList) :-
	!,
	$dProcessTerm(File, (?- G), NameList, VarList).
$dProcessTerm(File, (?- G), NameList, VarList) :-
	!,
	$dFixCalls(G, G1),
	$dProcessGoal(File, G1, NameList, VarList).
$dProcessTerm(_File, foreign(FFunc, Pred), _NameList, _VarList) :-
	!,
	functor(Pred, F, A),
	dAddDec(F, A, foreign, foreign(FFunc, Pred)).
$dProcessTerm(File, (H :- _B), _NameList, _VarList) :-
	var(H),
	!,
	currentInput(S), lineCount(S, LC),
	format(user_error,
		"Error: variable head in ~a about line ~d.\n", [File, LC]).
$dProcessTerm(File, (H :- B), NameList, VarList) :-
	!,
	$dCheckPred(File, H),
	$dRedefCheck(File, H),
	$dFixCalls(B, B1),
	dAddClause(cl((H :- B1), NameList, VarList)).
$dProcessTerm(File, Fact, NameList, VarList) :-
	!,
	$dCheckPred(File, Fact),
	$dRedefCheck(File, Fact),
	dAddClause(cl((Fact :- true), NameList, VarList)).

	% should index better!
$dProcessGoal(File, Term, _NameList, _VarList) :-
	var(Term),
	!,
	currentInput(S), lineCount(S, LC),
	format(user_error,
		"Error: variable goal in ~a about line ~d.\n", [File, LC]).
$dProcessGoal(_File, op(X, Y, Z), _NameList, _VarList) :-
	once op(X, Y, Z),
	fail.			% fall through to dAddGoal/1
$dProcessGoal(File, Term, NameList, VarList) :-		% HOOK
	currentPredicate(processGoal, 4),
	processGoal(File, Term, NameList, VarList),
	!.
$dProcessGoal(_File, (useIf G), _NameList, _VarList) :-
	!,
	(useIf G).
$dProcessGoal(_File, useElse, _NameList, _VarList) :-
	!,
	useElse.
$dProcessGoal(_File, useEnd, _NameList, _VarList) :-
	!,
	useEnd.
$dProcessGoal(File, H when B, _NameList, _VarList) :-
	!,
	$dCheckPred(File, H),
	$dRedefCheck(File, H),
	functor(H, F, A),
	dAddDec(F, A, (when), H when B).
$dProcessGoal(File, pure T, _NameList, _VarList) :-
	!,
	$dGoalDec(File, (pure), T).
$dProcessGoal(File, dynamic T, _NameList, _VarList) :-
	!,
	$dGoalDec(File, (dynamic), T).
		% type checkers use something different to this
%$dProcessGoal(File, pred T, _NameList, _VarList) :-
%	!,
%	functor(T, F, A),
%	dAddDec(F, A, (type), T).
$dProcessGoal(File, Term, NameList, VarList) :-
	dAddGoal(File, goal(Term, NameList, VarList)).

	% converts Var to call(Var) in goals
$dFixCalls(Var, call(Var)) :-
	var(Var),
	!.
$dFixCalls(call(Var), call(Var)) :-
	var(Var),
	!.
$dFixCalls(Term, Term1) :-
	$meta_call(Term, SubGoals, SubGoals1, Term1),
	!,
	$dFixListCalls(SubGoals, SubGoals1).
$dFixCalls(Term, Term).

$dFixListCalls([], []).
$dFixListCalls(G.List, G1.List1) :-
	$dFixCalls(G, G1),
	$dFixListCalls(List, List1).

	% Here abolish predicate if dConsulted again.
$dRedefCheck(File, H) :-
	functor(H, F, A),
	( \+ getprop($debug, $dSeen, F/A)
	->	( dPred(F, A)
		->	( dProp(F, A, $dDCed, Preds)
			->	dAbolishPred(F, A),
				dPutProp(F, A, $dDCed, Preds)
							% update this shortly
			;	dAbolishPred(F, A)
			),
			format(user_error,
				"Warning: ~a/~d redefined.\n", [F, A])
		),
		addprop($debug, $dSeen, F/A),
		putprop(F, $dFile(A), File)
	).

	% Kludgy (but useful) extra error detecting for eg. incomplete clauses.
$dCheckPred(File, H) :-
	( (functor(H, F, 2), occurs(F, set((,), (;), (->), (:-))))
	->	currentInput(S), lineCount(S, LC),
		format(user_error,
	"Warning: System construct ~a/2 redefined in ~a about line ~d.\n",
			[F, File, LC])
	).

	% remove "@" from file name (it is inserted by revise
	% system)
$remove_at(AtFile, File) :-
        atomToString(AtFile, AtStr),
	(append(Base, "@.nl", AtStr) ->
		append(Base, ".nl", Str)
	;
		Str = AtStr
	),
	atomToString(File, Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				%
%	Access Predicates	%
%				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%	dPred(F, A)
%	A procedure with functor F and arity A exists in the debugging area.

dPred(F, A) :-
	getprop($debug, $dPredicate, F/A).

%	FNList is the list of procedures in the debugging area, in the
%	form [F1/A1, F2/A2, ...].

dPreds(FNList) :-
	properties($debug, $dPredicate, FNList).

%	dClause(F, A, Clause)
%	Clause is a clause of procedure F/A in th debugging area.
%	All clauses are represented by terms of the form
%		cl((Head :- Body), NameList, VarList)
%	where NameList and VarList are as in readTerm/3.
%	F and/or A may be variables.

dClause(F, A, Clause) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	getprop(F, $dClause(A), Clause).

%	dClauses(F, A, ClauseList)
%	ClauseList is the list of clauses (in cl/3 format) in
%	the procedure F/A.
%	F and/or A may be variables.

dClauses(F, A, ClauseList) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	properties(F, $dClause(A), ClauseList).

%	dFile(F, A, File)
%	Predicate F/A has been loaded from File.nl
% 	FIX re '@' and revise

dFile(F, A, File) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	once getprop(F, $dFile(A), File).

%	dGoal(File, Goal)
%	Goal is a goal in File in the debugging area.

dGoal(File, Goal) :-
	( var(File) -> getprop($debug, $dConsulted, File) ),
	getprop($debug, $dGoal(File), Goal).

%	dGoals(File, GoalList)
%	GoalList is the list of goals in File in the debugging area.

dGoals(File, GoalList) :-
	( var(File) -> getprop($debug, $dConsulted, File) ),
	properties($debug, $dGoal(File), GoalList).

%	dGoals(GoalList)
%	GoalList is the list of all the goals in the debugging area.

dGoals(GoalList) :-
	properties($debug, $dConsulted, FileList),
	$dGoals(FileList, GoalList).

%	dDec(F, A, DType, Dec)
%	Dec is a declaration of type DType for procedure F/A.

dDec(F, A, DType, Dec) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	getprop(F, $dDeclaration(A, DType), Dec).

%	dDecs(F, A, DType, DecList)
%	DecList is the list of declarations of type DType for procedure F/A.

dDecs(F, A, DType, DecList) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	properties(F, $dDeclaration(A, DType), DecList).

%	dProp(F, A, PType, Prop)
%	Prop is a property of type PType for procedure F/A.

dProp(F, A, PType, Prop) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	getprop(F, $dProperty(A, PType), Prop).

%	dProps(F, A, PType, PropList)
%	PropList is the list of properties of type PType for procedure F/A.

dProps(F, A, PType, PropList) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	properties(F, $dProperty(A, PType), PropList).

%	dDCall(F, A, CF, CA)
%	Predicate F/A has a call to CF/CA in the body of one of its clauses.

?- dDCall(F, A, CF, CA) when (F and A) or (CF and CA).
dDCall(F, A, CF, CA) :-
	( var(F)		% then CF and CA must be instantiated
	->	$dFindAllCalls,
		dProp(CF, CA, $dDCed, Calls),
		member(F/A, Calls)
	;	dDCalls(F, A, Calls),
		member(CF/CA, Calls)
	).

%	dDCalls(F, A, FNList)
%	FNList is the list of procedures directly called by procedure F/A.

dDCalls(F, A, Calls) :-
	( (var(F) ; var(A)) -> dPred(F, A) ),
	( dProp(F, A, $dDC, Calls)
	->	true
	;	findall(CF/CN, (
			dClause(F, A, cl((_ :- B), _, _)),
			dGoalDCall(B, CF, CN),
			\+ systemPredicate(CF, CN)
		), Calls1),
		sort(Calls1, Calls),
		dPutProp(F, A, $dDC, Calls)
	).

%	dDAncs(F, A, Preds)
%	Preds are the predicates which call F/A directly.

dDAncs(F, A, Preds) :-
	$dFindAllCalls,
	( (var(F) ; var(A)) -> dPred(F, A) ),
	dProp(F, A, $dDCed, Preds).

%	dICall(F, A, CF, CA)
%	Predicate F/A calls CF/CA, possibly indirectly via other procedures.
%	may return same answer more than once (but not likely)
%	-> could make another level above dICalls which calls member

dICall(F, N, F1, N1) :-
	dDCalls(F, N, L),
	member(F2/N2, L),
	(	F1 = F2,
		N1 = N2
	;
		$dICallAnc(L, F2, N2, F1, N1)
	).

	% descends using ancestors and uncles etc to prevent loops
	% and some duplicate answers
$dICallAnc(R, F, N, F1, N1) :-
	dDCalls(F, N, L),
	member(F2/N2, L),
	\+ member(F2/N2, R),	% ignore repeats in ancestor list
	(	F1 = F2,
		N1 = N2
	;
		append(L, R, RR),
		$dICallAnc(RR, F2, N2, F1, N1)
	).

%	dICalls(F, A, FNList)
%	FNList is the list of procedures called (possibly indirectly)
%	by procedure F/A.

dICalls(F, N, FNLS) :-
	dPred(F, N),
	findall(F1/N1, dICall(F, N, F1, N1), FNL),
	sort(FNL, FNLS).

	% returns preds directly called by a goal
dGoalDCall(G, F, A) :-
	sub_goal(G, C),
	functor(C, F, A),
	\+ systemPredicate(F, A).

	% returns preds indirectly called by goal
dGoalICall(G, F, A) :-
	$dGetCalls(G, Calls1),
	sort(Calls1, Calls2),	% (to remove duplicates)
	findall(F1 / A1,
			(member(F2 / A2, Calls2),
			(	F1 = F2,
				A1 = A2
			;
				dICall(F2, A2, F1, A1)
			)),
		Preds),
	sort(Preds, Preds1),	% (to remove duplicates)
	member(F / A, Preds1).

%	dLastPreds(PredList)
%	[F1/A1, F2/A2, ...] in PredList loaded from the most
%	recently dConsulted file.

dLastPreds(P) :-
	properties($debug, $dSeen, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					%
%	Manipulating Predicates		%
%					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	dAddPred(F, A)
%	assert the existence of F/A in the debugging environment

dAddPred(F, A) :-
	( \+ getprop($debug, $dPredicate, F/A) 
	->	addprop($debug, $dPredicate, F/A),
		( systemPredicate(F, A)
		->	format(user_error,
				"Warning: system predicate ~a/~d redefined.\n",
				[F, A])
		)
	).

%	dAddPred(F, A, File)
%	assert the existence of F/A in the debugging environment and
%	associate it with File.
%	Fails if F/A associated with another file.

dAddPred(F, A, File) :-
	( dFile(F, A, File1)
	->	File = File1
	;	dAddPred(F, A),
		putprop(F, $dFile(A), File)
	).

%	dAddPreds(PredList)
%	assert the existence of the predicates [F1/A1, F2/A2, ...] in
%	PredList in the debugging environment

?- dAddPreds(List) when List.
dAddPreds([]).
dAddPreds((F/A).List) :-
	dAddPred(F, A),
	dAddPreds(List).

%	dAddPreds(List, File)
%	assert the existence of the predicates [F1/A1, F2/A2, ...] in
%	PredList in the debugging environment and associate with File

?- dAddPreds(List, File) when List.
dAddPreds([], _File).
dAddPreds((F/A).List, File) :-
	dAddPred(F, A, File),
	dAddPreds(List, File).

%	dAbolishPred(F, A)
%	Remove F/A and all associated information from the debugging environment

dAbolishPred(F, A) :-
	dRmClauses(F, A),
	dRmDecs(F, A),
	dRmProps(F, A),			% BUG indirect calls junked also ...
	remprop($debug, $dPredicate, F/A).

%	dAbolishPreds(Preds)
%	Remove Preds and associated information from the debugging environment

?- dAbolishPreds(Preds) when Preds.
dAbolishPreds([]).
dAbolishPreds((F/A).Rest) :-
	dAbolishPred(F, A),
	dAbolishPreds(Rest).



?- $dGoalDec(_, _, T) when T.
$dGoalDec(File, Type, F/A) :-
	functor(H, F, A),
	$dCheckPred(File, H),
	$dRedefCheck(File, H),
	dAddDec(F, A, Type, F/A).
$dGoalDec(_File, _Type, []).
$dGoalDec(File, Type, (F/A).List) :-
	functor(H, F, A),
	$dCheckPred(File, H),
	$dRedefCheck(File, H),
	dAddDec(F, A, Type, F/A),
	$dGoalDec(File, Type, List).

$dGoals([], []).
$dGoals(F.List, GoalList) :-
	dGoals(F, GL),
	append(GL, Rest, GoalList),
	$dGoals(List, Rest).

%	dAddClause(Clause)
%	Adds Clause (in cl/3 format) to the end of a procedure.

dAddClause(cl((H :- B), NameList, VarList)) :-
	functor(H, F, A),
	dAddPred(F, A),
	( \+ dFile(F, A, _File) -> putprop(F, $dFile(A), $dNoFile) ),
	addpropz(F, $dClause(A), cl((H :- B), NameList, VarList)),
	( dProp(F, A, $dDC, Calls)	% Direct calls list
	->	findall(C, $dGetCalls(B, C), Calls1),
		sort(Calls1, Calls2),
		merge(Calls2, Calls, Calls3),
		dPutProp(F, A, $dDC, Calls3),
		( getprop($debug, $calls, $junk)
		->	forall( (	member(F1/A1, Calls2),
					\+ member(F1/A1, Calls)
				), (
					dProp(F1, A1, $dDCed, C),
					merge([F/A], C, C1),	% Yuk addElement
					dPutProp(F1, A1, $dDCed, C1)
				)
			)
		)
	).

%	dAddClauses(CList)
%	Adds list of clauses (in cl/3 format) to the end of a procedure.

?- dAddClauses(List) when List.
dAddClauses([]).
dAddClauses(Clause.Rest) :-
	dAddClause(Clause),
	dAddClauses(Rest).

%	dRmClause(F, A, Clause)
%	Removes a clause from the procedure F/A.

dRmClause(F, A, Clause) :-
	remprop(F, $dClause(A), Clause),
	dRmProps(F, A, $dDC).

%	dRmClauses(F, A)
%	Removes all clauses from the procedure F/A.

dRmClauses(F, A) :-
	remprop(F, $dClause(A)),
	dRmProps(F, A, $dDC).

%	dAddDec(F, A, DType, Dec)
%	Adds a declaration Dec of type DType to procedure F/A.
%	For example dAddDec(len, 2, type, len(list, int)).

dAddDec(F, A, DType, Dec) :-
	dAddPred(F, A),
	addpropz(F, $dDeclaration(A, DType), Dec).

%	dAddDecs(F, A, DType, DecList)
%	Adds a list of declarations DecList to type DType to procedure F/A.

?- dAddDecs(_F, _A, _DType, DecList) when DecList.
dAddDecs(_F, _A, _DType, []).
dAddDecs(F, A, DType, Dec.Rest) :-
	dAddDec(F, A, DType, Dec),
	dAddDecs(F, A, DType, Rest).

%	dRmDec(F, A, DTyp, Dec)
%	Removes declaration Dec of type DType from procedure F/A

dRmDec(F, A, DType, Dec) :-
	remprop(F, $dDeclaration(A, DType), Dec).

%	dRmDecs(F, A, DType)
%	Removes all declarations of type DType from procedure F/A

dRmDecs(F, A, DType) :-
	remprop(F, $dDeclaration(A, DType)).

%	dRmDecs(F, A)
%	Removes all declarations from procedure F/A

dRmDecs(F, A) :-
	remprop(F, $dDeclaration(A, _)).

%	dAddGoal(File, Goal)
%	Adds a Goal to the debugging area.
%	For example dAddGoal('count.nl', goal(assert(count(0)), [], [])).

dAddGoal(File, Goal) :-
	addpropz($debug, $dGoal(File), Goal).

%	dAddGoals(File, GoalList)
%	Adds a list of goals to the debugging area.

?- dAddGoals(_, GoalList) when GoalList.
dAddGoals(_File, []).
dAddGoals(File, Goal.Rest) :-
	dAddGoal(File, Goal),
	dAddGoals(File, Rest).

%	dRmGoal(Goal)
%	Removes a goal from the debugging area.

dRmGoal(File, Goal) :-
	remprop($debug, $dGoal(File), Goal).

%	dRmGoals(File)
%	Removes all goals in File from the debugging environment.

dRmGoals(File) :-
	remprop($debug, $dGoal(File)).

%	dRmGoals
%	Removes all goals from the debugging environment.

dRmGoals :-
	getprop($debug, $dConsulted, File),
	dRmGoals(File),
	fail.
dRmGoals.

%	dPutProp(F, A, PType, Prop)
%	Puts a property Prop of type PType with procedure F/A.
%	For example dPutProp(perm, 2, direct_calls, [append/3, delete/3]).

dPutProp(F, A, PType, Prop) :-
	% should delete next line?
	% causes called but undefined preds to be defined
	% dAddPred(F, A),
	putprop(F, $dProperty(A, PType), Prop).

%	dAddProp(F, A, PType, Prop)
%	Adds a property Prop of type PType to procedure F/A.
%	For example dAddProp(perm, 2, direct_calls, [append/3, delete/3]).

dAddProp(F, A, PType, Prop) :-
	% should delete next line?
	% causes called but undefined preds to be defined
	% dAddPred(F, A),
	addpropz(F, $dProperty(A, PType), Prop).

%	dAddProps(F, A, PType, PropList)
%	Adds a list of properties PropList to type PType to procedure F/A.

?- dAddProps(_F, _A, _PType, PropList) when PropList.
dAddProps(_F, _A, _PType, []).
dAddProps(F, A, PType, Prop.Rest) :-
	dAddProp(F, A, PType, Prop),
	dAddProps(F, A, PType, Rest).

%	dRmProp(F, A, PType, Prop)
%	Removes Prop of type PType from procedure F/A

dRmProp(F, A, PType, Prop) :-
	remprop(F, $dProperty(A, PType), Prop).

%	dRmProps(F, A, PType)
%	Removes all properties of type PType from procedure F/A

dRmProps(F, A, PType) :-
	remprop(F, $dProperty(A, PType)).

%	dRmProps(F, A)
%	Removes all properties from procedure F/A

dRmProps(F, A) :-
	remprop(F, $dProperty(A, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				%
%	Utility stuff		%
%				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

$dGetCalls(B, Calls) :-
	nonvar(B),
	findall(F/A,
		( sub_goal(B, Call),
		  \+ systemPredicate(Call),
		  functor(Call, F, A) ),
		Calls).

$dFindAllCalls :-
	( \+ getprop($debug, $calls, $junk)
	->	forall( dPred(F, A), dPutProp(F, A, $dDCed, []) ),
		forall( ( dDCalls(F, A, Calls), member(F1/A1, Calls), dPred(F1, A1) ),
			( dProp(F1, A1, $dDCed, AC)
			->	dPutProp(F1, A1, $dDCed, (F/A).AC)
			;	dPutProp(F1, A1, $dDCed, (F/A).[])
			)
		),
		forall( dPred(F, A), (
			dProp(F, A, $dDCed, C),
			sort(C, C1),
			dPutProp(F, A, $dDCed, C1)
		)),
		putprop($debug, $calls, $junk)
	).

	% returns all atomic subgoals of a goal
	% (including system predicates)
sub_goal(B, C) :-
        nonvar(B),
        $meta_call(B, Gs, _, _),
        !,
        member(G, Gs),
        sub_goal(G, C).
sub_goal(G, G1) :-
	nonvar(G),
	G1 = G.		% a bit safer

	% list of preds with preds as args (which need to be transformed)
	% second and third args are subterms which need to be transformed
	% in first and last args respectively
?- $meta_call(A,  _, _, _) when A.
%
%	Declarative Constructs
%
%	Standard Constructs
%
$meta_call(call(A), [A], [A1], call(A1)).
$meta_call((A, B), [A, B], [A1, B1], (A1, B1)).
$meta_call((A ; B), [A, B], [A1, B1], (A1 ; B1)).
%
%	Quantifiers
%
$meta_call(some(A, B), [B], [B1], some(A, B1)).
$meta_call(all(A, B), [B], [B1], all(A, B1)).
%
%	Negation, Impliction and Related Constructs
%
$meta_call(not(A), [A], [A1], not(A1)).
$meta_call((A => B), [A, B], [A1, B1], (A1 => B1)).
$meta_call((A <= B), [A, B], [A1, B1], (A1 <= B1)).
$meta_call((A <=> B), [A, B], [A1, B1], (A1 <=> B1)).
$meta_call(if(A), [A], [A1], if(A1)).
$meta_call((A then B), [A, B], [A1, B1], (A1 then B1)).
$meta_call((A else B), [A, B], [A1, B1], (A1 else B1)).
%
%	Aggregate Functions
%
$meta_call(solutions(A, B, C), [B], [B1], solutions(A, B1, C)).
$meta_call(count(A, B, C), [B], [B1], count(A, B1, C)).
$meta_call(max(A, B, C), [B], [B1], max(A, B1, C)).
$meta_call(min(A, B, C), [B], [B1], min(A, B1, C)).
$meta_call(sum(A, B, C, D), [C], [C1], sum(A, B, C1, D)).
%
%	Non-declarative Constructs
%
%	Non-logical Quantifiers
$meta_call(gSome(A, B), [B], [B1], gSome(A, B1)).
$meta_call(gAll(A, B), [B], [B1], gAll(A, B1)).
%
%	Non-logical Constructs
%
$meta_call(\+(A), [A], [A1], \+(A1)).
$meta_call((A -> B), [A, B], [A1, B1], (A1 -> B1)).
$meta_call(once(A), [A], [A1], once(A1)).
$meta_call(setof(A, B, C), [B], [B1], setof(A, B1, C)).
$meta_call(bagof(A, B, C), [B], [B1], bagof(A, B1, C)).
$meta_call(findall(A, B, C), [B], [B1], findall(A, B1, C)).
$meta_call(^(A, B), [B], [B1], ^(A, B1)).
	% may not want these
	% BUG - delete?
$meta_call(forall(G1, G2), [G1, G2], [G3, G4], forall(G3, G4)).
$meta_call((A :- B), [B], [B1], (A :- B1)).
$meta_call(?-(A), [A], [A1], ?-(A1)).
$meta_call(:-(A), [A], [A1], :-(A1)).
% $meta_call(whenGround(A, B), [B], [B1], whenGround(A, B1)).
% $meta_call(freeze(A, B), [B], [B1], freeze(A, B1)).

applySuffix(In, Suffix, Out, OutS) :-
	atomToString(In, InStr),
	( append(OutStr, Suffix, InStr)
	->	OutS = In,
		atomToString(Out, OutStr)
	;	Out = In,
		append(InStr, Suffix, OutSStr),
		atomToString(OutS, OutSStr)
	).

	% BUG - delete (or put it elsewhere)?
forall(G1, G2) :- \+ (call(G1), \+ call(G2)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				%
%	Miscellaneous		%
%				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	dPortraycl(C)
%	portray clause in cl/3 format (fix var names etc)

dPortraycl(cl(C, VN, V)) :-
	\+ \+ (name_vars(VN, V),	% avoid clobbering vars
		listOfVars(C, UVars),
		mk_uscore(UVars),
		portraycl(C)).

	% Bind all vars in list to name term for underscore.
?- mk_uscore(A) when A.
mk_uscore([]).
mk_uscore($VAR("_").L) :-
	mk_uscore(L).

	% bind vars to $VAR(Name)
?- name_vars(A, B) when A and B.
name_vars([], []).
name_vars(N.VN, $VAR(N).V) :-
	name_vars(VN, V).

%	dListing(F, A)
%	list all clauses of F/A.

dListing(F, A) :-
	forall( dDec(F, A, Type, Dec),
		print_dec(Type, Dec) ),
	forall( dClause(F, A, C),
		(dPortraycl(C), putl(".\n")) ),
	nl.

%	dListing
%	list all preds

dListing :-
	forall( dGoal(_File, goal(G, VN, V)),
		( name_vars(VN, V),
		  listOfVars(C, UVars),
		  mk_uscore(UVars),
		  write((?- G)),
		  writeln(.)
		)),
	nl,
	forall( dPred(F, A),
		dListing(F, A) ).

	% print declaration of given type
print_dec(pure, F/A) :-
	printf("?- pure %s/%d.\n", [F, A]).
print_dec(dynamic, F/A) :-
	printf("?- dynamic %s/%d.\n", [F, A]).
print_dec(when, (WH when WB)) :-
	portraycl((?- WH when WB)),
	writeln(.).
print_dec(pred, pred_type(H, V, G)) :-
	( G == true ->
		portraycl((?- pred H)),
		writeln(.)
	;
		portraycl((?- pred H where (V : G))),
		writeln(.)
	).
	% edit predicate in the debugging environment
dEdit(Pred, Arity) :-
	dFile(Pred, Arity, File),
	sformat("revise -p ~a ~a", [Pred, File], Cmd),
	% sformat("/usr/local/bin/revise -p ~a ~a", [Pred, File], Cmd),
	system(Cmd),
	atomToString(File, FileS),
	append(FileBase, ".nl", FileS),
	append(FileBase, "@.nl", FileS2),
	atomToString(File2, FileS2),
	dLoad(File2).
