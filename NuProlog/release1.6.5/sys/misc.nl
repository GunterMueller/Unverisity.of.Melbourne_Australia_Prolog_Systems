/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog parameters.

restart :-
	abort.

iota(N, M, I) :-
	between(N, M, I).

?- between(N, M, I) when N and M or I.
between(N, M, I) :-
	( integer(I) ->
		N =< I,
		I =< M
	;	integer(N),
		integer(M),
		$between(N, M, I)
	).

$between(N, M, I) :-
	( N < M ->
		(	N = I
		;	N1 is N + 1,
			$between(N1, M, I)
		)
	;	N = M,
		N = I
	).

useIf X :-
	\+ X,
	repeat,
	read(Y),
	nonvar(Y),
	(	Y = (?- useElse)
	;	Y = (:- useElse)
	;	Y = (?- useEnd)
	;	Y = (:- useEnd)
	;	eof(Y),	
		format(user_error,
			"~NError in ~w.~n\c
			End of input encountered before matching useEnd.~n",
			[useIf X])
	;	Y = (?- useIf _),
		useElse,
		fail
	;	Y = (:- useIf _),
		useElse,
		fail
	),
	! .
useIf _.

useElse :-
	repeat,
	read(Y),
	nonvar(Y),
	(	Y = (?- useEnd)
	;	Y = (:- useEnd)
	;	eof(Y),	
		writeln('Unmatched "use"')
	;	Y = (?- useIf _),
		useElse,
		fail
	;	Y = (:- useIf _),
		useElse,
		fail
	),
	!.

useEnd.

?- $nameToAtom(X, _) when ground(X).
$nameToAtom(Name, Atom) :-
	( atom(Name) ->
		Name = Atom
	;	name(Atom, Name)
	).

?- $nameToString(X, _) when ground(X).
$nameToString(Name, String) :-
	( atom(Name) ->
		name(Name, String)
	;	$listToString(Name, String)
	).

?- $sameName(X, Y) when ground(X) and ground(Y).
$sameName(X, Y) :-
	$nameToString(X, S),
	$nameToString(Y, S).

%	View of compare/3's first argument for @=</2 and @>=/2.
$compareLE(=).
$compareLE(<).

%	Ignore type declarations.
type(_).

%	Ignore pred declarations.
pred(_).

%	Declare a predicate pure.
pure(Pred/Arity) :-
	( getprop(Pred, $property(Arity), pure) ->
		true
	;	addpropa(Pred, $property(Arity), pure)
	).

?- initializing, $dynamic(option/1).

when(Pattern, Vars) :-
	nonvar(Pattern),
	functor(Pattern, Pred, Arity),
	atom(Pred),
	!,
	$makeDynamicWithWhens(Pred, Arity),
	addpropz(Pred, $when(Arity), when(Pattern, Vars)).
when(Pattern, Vars) :-
	format(user_error,
		"~NError in ~w.~n\c
		LHS of a when declaration must be about a predicate.~n",
		[when(Pattern, Vars)]),
	fail.

dynamic D :-
	$applyToEachPredicate(X, D, $dynamic(X)).

$dynamic(X) :-
	$dynamic(X, fail).

$dynamic(Pred/Arity, CanDelay) :-
	$loadingPredicate(Pred, Arity),
	addpropa(Pred, $property(Arity), dynamic),
	( Arity =:= 0 ->
		TmpCode = [
			emit(pc, [255], $(Pred))		% Rather inefficient
		]
	;	TmpCode = [
			emit(ps, [255], Pred / Arity)
		|	Unifies
		],
		iload$unifies(0, Arity, Unifies)
	),
	( CanDelay == fail ->
		append(TmpCode, 
			[	emit(gvarx, [0, 255]),
				emit(exec, [1], &(sys$callDynamic, 1))
			],
			Code)
	;	append(TmpCode, 
			[	emit(all, [2]),
				emit(gvary, [1, 255]),
				emit(pc, [0], $(Pred)),
				emit(pc, [1], $(Arity)),
				emit(pvary, [0, 2]),
				emit(call, [2, 3], &(whens, 3)),
				emit(pvaly, [0, 0]),
				emit(pvaly, [1, 1]),
				emit(dallexe, [2], &(sys$whensCall, 2))
			],
			Code)
	),
	iload$iload(Pred, Arity, Code),
	iload$iload(Pred, Arity, []).

iload$unifies(N, N, []).
iload$unifies(N, Arity, emit(ulvx, [N]).Unifies) :-
	N < Arity,
	N1 is N + 1,
	iload$unifies(N1, Arity, Unifies).

%	BUG!  Redundant.
dynamic(Pred, Arity) :-
	once
	(	getprop(Pred, Key, Prop),
		(	Key = $clause(Arity)		% Quicker
		;	Key = $property(Arity),
			Prop = (dynamic)
		)
	).

$hasWhens(Pred, Arity) :-
	once getprop(Pred, $when(Arity), _).
	
pure(Pred, Arity) :-
	once getprop(Pred, $property(Arity), pure).

predicateProperty(Pred, Arity, Property) :-
	currentPredicate(Pred, Arity),
	(	getprop(Pred, $property(Arity), Property)
	;	Property = compiled,
		\+ getprop(Pred, $property(Arity), dynamic)
	;	Property = system,
		$defined(Pred, Arity, File, _),
		once getprop($systemSourceFiles, $file, File)
	;	Property = built_in,			% Same as system.
		$defined(Pred, Arity, File, _),
		once getprop($systemSourceFiles, $file, File)
	;	Property = library,
		libdirectory(LibDir),
		name(LibDir, LibDirName),
		$defined(Pred, Arity, File, _),
		name(File, FileName),
		append(LibDirName, _, FileName)
	;	Property = spypoint,
		$spypoint(Pred, Arity)
	).

systemPredicate(X) :-
	functor(X, Pred, Arity),
	systemPredicate(Pred, Arity).

systemPredicate(Pred, Arity) :-
	predicateProperty(Pred, Arity, system).

%	Auto-load lib predefined.
libraryPredicate(X, Y) :-
	$autoLoadLibrary(predefined),
	libraryPredicate(X, Y).
nonlogicalPredicate(X) :-
	$autoLoadLibrary(predefined),
	nonlogicalPredicate(X).
libraryPredicate(X, Y, Z) :-
	$autoLoadLibrary(predefined),
	libraryPredicate(X, Y, Z).
nonlogicalPredicate(X, Y) :-
	$autoLoadLibrary(predefined),
	nonlogicalPredicate(X, Y).

absoluteFileName(Rel, Abs) :-
	( $findFile(Rel, [".nl", ""], 0, _Ext, Found) ->
		name(Abs, Found)
	;	format(user_error,
			"~NError in ~w -- File not found.~n", absoluteFileName(Rel, Abs)),
		fail
	).

sourceFile(File) :-
	getprop($sourceFiles, $file, File).

sourceFile(Pred, Arity, File) :-
	currentPredicate(Pred, Arity),
	$defined(Pred, Arity, File, _).

prologFlag(Flag, Old, New) :-
	( atom(Flag) ->
		( $flagNumber(Flag, FN) ->
			true
		;	format(user_error,
				"~NError in ~w -- unknown flag.~n",
				[prologFlag(Flag, Old, New)]),
			fail
		)
	;	format(user_error,
			"~NError in ~w -- flag name must be atom.~n",
			[prologFlag(Flag, Old, New)]),
		fail
	),
	$flags(Flags),
	arg(FN, Flags, Old),
	( \+ atomic(New) ->
		format(user_error,
			"~NError in ~w -- flag value must be atomic.~n",
			[prologFlag(Flag, Old, New)]),
		fail
	),
	$replacn(FN, Flags, New).

prologFlag(Flag, Value) :-
	$getFlag(Flag, Value).

$setFlag(Flag, Value) :-
	( \+ atomic(Value) ->
		format(user_error,
			"~NError in ~w -- flag value must be atomic.~n",
			[$setFlag(Flag, Value)]),
		fail
	),
	$flagNumber(Flag, FN),
	$flags(Flags),
	$replacn(FN, Flags, Value).

$getFlag(Flag, Value) :-
	$flagNumber(Flag, FN),
	$flags(Flags),
	arg(FN, Flags, Value).

$flagNumber(characterEscapes, 1).
$flagNumber(fileErrors, 2).
$flagNumber(debugging, 3).
$flagNumber(wizard, 4).		% Controls various protections against stupidity
$flagNumber(libdirectory, 5).
$flagNumber(bindirectory, 6).
$flagNumber(datagrow, 7).
$flagNumber(localDebug, 8).
$flagNumber(leashMode, 9).
$flagNumber(callNumber, 10).
$flagNumber(delayed, 11).
$flagNumber(vars, 12).
$flagNumber(redefinitionWarning, 13).
$flagNumber(printDepth, 14).
$flagNumber(tdatagrow, 15).
$flagNumber(commanddirectory, 16).
$flagNumber(codegrow, 17).
$flagNumber(optimizeQuery, 18).
$flagNumber(maxDepth, 19).
$flagNumber(fload, 20).
$flagNumber(machine, 21).
$flagNumber(version, 22).
$flagNumber(nstreams, 23).
$flagNumber(maxArity, 24).

%	Auto-load lib db.
dbBackup(DB, File) :-
	$autoLoadLibrary(db),
	dbBackup(DB, File).
dbCons(DB) :-
	$autoLoadLibrary(db),
	dbCons(DB).
dbCreate(DB) :-
	$autoLoadLibrary(db),
	dbCreate(DB).
dbDefine(DB, Functor, Arity) :-
	$autoLoadLibrary(db),
	dbDefine(DB, Functor, Arity).
dbDefine(DB, Functor, Arity, Params) :-
	$autoLoadLibrary(db),
	dbDefine(DB, Functor, Arity, Params).
dbParam(DB, Functor, Arity, Param=Value) :-
	$autoLoadLibrary(db),
	dbParam(DB, Functor, Arity, Param=Value).
dbRestore(DB, File) :-
	$autoLoadLibrary(db),
	dbRestore(DB, File).
dbRules(DB, File) :-
	$autoLoadLibrary(db),
	dbRules(DB, File).
dbUndefine(DB, Functor, Arity) :-
	$autoLoadLibrary(db),
	dbUndefine(DB, Functor, Arity).

%	Auto-load lib man.
man(F) :-
	$autoLoadLibrary(man),
	man(F).

%	Auto-load lib dsimc.
$optimize(X, Y) :-
	$autoLoadLibrary(db),
	$autoLoadLibrary(dsimc),
	$optimize(X, Y).

%	Make a list of the vars in a term.
listOfVars(T, Vars) :-
	$listOfVars(T, Vars, []).

$listOfVars(X, D0, D) :-
	var(X),
	( occurs(X, D) ->	% Devious.  Examine $listOfVarsL for explanation.
		D0 = D			% ensure failure if D is already bound
	;	D0 = X.D		% ensure failure if D is [].
	).
$listOfVars(X, D0, D) :-
	const(X),
	D0 = D.				% ensure failure if D is already bound
$listOfVars(X, D0, D) :-
	term(X),
	functor(X, _, N),
	$listOfVarsN(N, X, D0, D).

%$listOfVars(X, D0, D) :-
%	( var(X) ->
%		(occurs(X, D) ->	% Devious.  Examine $listOfVarsL for explanation.
%			D0 = D			% ensure failure if D is already bound
%		;	D0 = X.D		% ensure failure if D is [].
%		)
%	; const(X) ->
%		D0 = D				% ensure failure if D is already bound
%	;	functor(X, _, N),
%		$listOfVarsN(N, X, D0, D)
%	).

$listOfVarsN(N, T, D0, D) :-
	( N > 0 ->
		arg(N, T, X),
		N1 is N - 1,
		$listOfVars(X, D1, D),
		$listOfVarsN(N1, T, D0, D1)
	;	D0 = D
	).

%	Make a list of variables in Term on which things might be waiting.
%
%	Note the "might".  It is a BUG!  Sufficiently strange delaying
%	predicates may wake and complete without unmarking all the variables
%	on which they have waited.
waitedOn(Term, DelayedVars) :-
	listOfVars(Term, Vars),
	$waitedOn(Vars, DelayedVars).

?- $waitedOn(X, _) when X.
$waitedOn([], []).
$waitedOn(X.XT, Y) :-
	$del(X),
	!,
	Y = X.YT,		% ensure failure if Y is already bound
	$waitedOn(XT, YT).
$waitedOn(_.XT, YT) :-
	$waitedOn(XT, YT).
