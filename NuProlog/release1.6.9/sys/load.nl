/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog code loading predicates.

%	Find a file with a given access mode and a given name and the first
%	of a list of extensions.
$findFile(FileName, Extensions, Mode, Ext, FoundFile) :-
	$fileNameToString(FileName, File),
	( append(Base, 0'..FileExt, File),
	  \+ member(0'., FileExt), \+ member(0'/, FileExt),
	  member(0'..FileExt, Extensions)
	->	true
	;	Base = File
	),
	member(Ext, Extensions),
	append(Base, Ext, FoundFile),
	access(FoundFile, Mode),
	!.

$expandFileName(FileName, File) :-
	$fileName(FileName),			% Just fail if not a potential file name
	$fileNameToString(FileName, FS),
	name(File, FS).

%	FN purports to be a file name.
$fileName(Var) :-
	var(Var),
	!,
	fail.
$fileName(Atom) :-
	atom(Atom),
	!.
$fileName(N.NT) :-
	(\+ integer(N) ->
		$fileName(N)
	),
	$fileName(NT).
$fileName(/(N)) :-
	$fileName(N).
$fileName(N/NT) :-
	$fileName(N),
	$fileName(NT).
$fileName(library(Lib)) :-
	$fileName(Lib).
$fileName(append(LibDir, Lib)) :-
	$fileName(LibDir),
	$fileName(Lib).

$fileNameToString(FN, FS) :-
	(ground(FN) ->
		true
	;	format(user_error,
			"~NError in ~w -- file name must be ground.~n",
			[$fileNameToString(FN, FS)]),
		fail
	),
	$fileNameToList(FN, S0, []),
	$fileNameListToComponents(S0, C0),
	( C0 = (0'~.User).C1 ->
		isAlphaL(User),
		$lookupUserHome(User, Home),
		$fileNameListToComponents(Home, HomeComponents),
		append(HomeComponents, C1, C2)
	; C0 = "/"._ ->
		C2 = C0
	;	getwd(WD),
		$fileNameToList(WD, WDL, []),
		$fileNameListToComponents(WDL, WDComponents),
		append(WDComponents, "/".C0, C2)
	),
	$cleanupFileNameComponents(C2, C3),
	$fileNameListToComponents(S1, C3),
	$listToString(S1, FS),
	!.

$fileNameListToComponents([], []) :-
	!.
$fileNameListToComponents(0'/.L, "/".C) :-
	!,
	$fileNameListToComponents(L, C).
$fileNameListToComponents(LL, C0.C) :-
	spanList(\==(0'/), LL, C0, L),
	$fileNameListToComponents(L, C).

$cleanupFileNameComponents(C0, C) :-
	$cleanupFileNameComponents1(C0, C1),
	( C0 = C1 ->
		C1 = C
	;	$cleanupFileNameComponents(C1, C)
	).

$cleanupFileNameComponents1([], []).
$cleanupFileNameComponents1("/"."/".C, D) :-
	!,
	$cleanupFileNameComponents1("/".C, D).
$cleanupFileNameComponents1("."."/".C, D) :-
	!,
	$cleanupFileNameComponents1(C, D).
$cleanupFileNameComponents1(_."/"."..".C, D) :-
	!,
	$cleanupFileNameComponents1(C, D).
$cleanupFileNameComponents1(C0.C, C0.D) :-
	!,
	$cleanupFileNameComponents1(C, D).

$lookupUser(User, Entry) :-
	( cons(User) ->
		open('/etc/passwd', read, S),
		repeat,
		getl(S, E),
		( E == [] ->
			!,
			close(S),
			fail
		; append(_, [NL], E), NL \== 0'\n ->
			!,
			close(S),
			fail
		),
		append(User, 0':._, E),
		!,
		close(S),
		Entry = E
	; User == [] ->
		( getenv('LOGNAME', Self) ->
			$lookupUser(Self, Entry)
		;	format(user_error,
				"~NError in ~w. -- Can't figure out who I am.~n",
				[$lookupUser(User, Entry)])
		)
	;	format(user_error,
			"~NError in ~w. -- String or [] expected.~n",
			[$lookupUser(User, Entry)])
	).

$lookupUserHome(User, Home) :-
	$lookupUser(User, Entry),
	groupList(\==(0':), Entry, _, Fields),
	nth0(5, Fields, Home).

?- $fileNameToList(X, _, _) when X.
$fileNameToList(Atom, S, SE) :-
	atom(Atom),
	( Atom == [] ->
		S = SE
	;	name(Atom, Name),
		append(Name, SE, S)
	).
$fileNameToList(N.NT) -->
	( {integer(N)} ->
		[N]
	;	$fileNameToList(N),
		"."
	),
	$fileNameToList(NT).
$fileNameToList(/(N)) -->
	"/",
	$fileNameToList(N).
$fileNameToList(N/NT) -->
	$fileNameToList(N),
	"/",
	$fileNameToList(NT).
$fileNameToList(library(Lib)) -->
	{libdirectory(LibDir)},
	$fileNameToList(LibDir),
	( {name(LibDir, X), \+ append(_, _."/", X)}
		-> "/"),
	$fileNameToList(Lib).
$fileNameToList(append(LibDir, Lib)) -->
	$fileNameToList(LibDir),
	$fileNameToList(Lib).

?- $baseName(X, _) when ground(X).
$baseName(Name, Base) :-
	$fileNameToList(Name, List, []),
	(	(	append(_, 0'/.Base, List),
			notMember(0'/, Base)) ->
		true
	;	Base = List
	).

%	make a name for a temporary file by replacing the sub-string
%	"XXXXXX" in Pattern with process id and a letter.
?- makeTemp(X, _) when ground(X).
makeTemp(Pattern, File) :-
	$fileNameToList(Pattern, List, []),
	append("XXXXXX", Suffix, Middle),
	append(Prefix, Middle, List),
	getpid(Pid),
	between(0'a, 0'z, Letter),
	sformat("~s~|~'0t~d~c~6+~s", [Prefix, Pid, Letter, Suffix], File),
	\+ stat(File, _),
	!.

%	Load a file of Nepolog object code.
%	The extension ".no" is added if absent.
load(FileName) :-
	( $findFile(FileName, [".no", ".nl", "", ".pl"], 2'100, Ext, LoadFile) ->
		true
	;	format(user_error,
			"~NError in ~w. -- cannot find file to load.~n",
			[load(FileName)]),
		fail
	),
	( Ext == ".no" ->
		$loadFile(LoadFile)
	;	$consult(LoadFile)
	),
	putl(user, "done\n").

$loadFile(File) :-
	$nameToString(File, FileS),
	format(user, "~NLoading ~s.~n", [FileS]),
	( $load(FileS)
	->	true
	;	format(user_error,
			"~NError in ~w. -- cannot load.~n",
			[$loadFile(File)])
	).

X.Y :-
	mapList(load, X.Y).
%	$applyToEach(File, X.Y, load(File)).

%	Ensure that load/1 has been performed on some files.
ensureLoaded(Files) :-
	( cons(Files) ->
		mapList($ensureLoaded, Files)
	;	Files \== [], $ensureLoaded(Files)		% Not sure about [].
	).
%	$applyToEach(File, Files, $ensureLoaded(File)).

$ensureLoaded(FileName) :-
	( $findFile(FileName, [".no", ".nl", "", ".pl"], 2'100, _Ext, LoadFile) ->	
		( LoadFile = 0'/._ ->
			name(Loading, LoadFile)
		;	getwd(CWD),
			( CWD == '/' ->
				name(Loading, 0'/.LoadFile)
			;	name(CWD, CWDString),
				append(CWDString, 0'/.LoadFile, FullFileName),
				name(Loading, FullFileName)
			)
		),
		( \+ sourceFile(Loading) ->
			load(FileName)
		)
	;	load(FileName)
	).

$autoLoadLibrary(Lib) :-
	prologFlag(redefinitionWarning, Old, off),
	lib Lib,
	prologFlag(redefinitionWarning, _, Old).

consult(Files) :-
	( cons(Files) ->
		mapList($consult, Files)
	;	Files \== [], $consult(Files)			% Not sure about [].
	).

$consult(File) :-
	\+ ground(File),
	!,
	format(user_error,
		"~NError in ~w. -- file name must be ground.~n",
		[consult(File)]),
	fail.
$consult(user) :-
	!,
	format(user, "~NConsulting user.~n", []),
	open('/dev/tty', read, Stream),
	$consultStdin(Stream).
$consult(File) :-
	( $findFile(File, [".nl", "", ".pl"], 2'100, _Ext, TrueFileName) ->
		true
	;	format(user_error,
			"~NError in ~w. -- cannot find file.~n",
			[consult(File)]),
		fail
	),
	$nameToAtom(TrueFileName, TrueFile),
	( getprop($sourceFiles, $file, TrueFile) ->
		true
	;	addprop($sourceFiles, $file, TrueFile)
	),
	format(user, "~NConsulting ~a.~n", [TrueFile]),
	open(TrueFile, read, Stream),
	$consultStdin(Stream).

$consultStdin(Stream) :-
	currentInput(Input),
	setInput(Stream),
	$initConsult(First),
	repeat,
	( read1(Clause), \+ isEof(Clause) ->
		$storeClause(Clause),
		fail
	;	streamEof(Stream)
	),
	$cleanupConsult(First),
	!,
	close(Stream),
	setInput(Input).

$initConsult(First) :-
	( $consulting ->
		First = rest
	;	First = first,
		$initConsult
	).

$initConsult :-
	getprop($consulting, $consulting, Pred),
	remprop(Pred, $consulting),
	fail.
$initConsult :-
	remprop($consulting, $consulting),
	remprop($consulting, $consultingNow),
	addprop($consulting, $consultingNow, true).

%	Boy Scout Technique.
$cleanupConsult(rest).
$cleanupConsult(first) :-
	$initConsult,
	remprop($consulting, $consultingNow).

$storeClause(C) :-
	expandTerm(C, D),
	$storeClause2(D).

$storeClause2(?-(Head when Pattern)) :-
	!,
	$loadingPredicate(Head),
	(Head when Pattern).
$storeClause2(:-(Head when Pattern)) :-
	!,
	$loadingPredicate(Head),
	(Head when Pattern).
$storeClause2(?-(G)) :-
	!,
	call(G),
	!.
$storeClause2(:-(G)) :-
	!,
	call(G),
	!.
$storeClause2((Head :- Body)) :-
	!,
	$loadingPredicate(Head),
	assertz((Head :- Body)).
$storeClause2(Head) :-
	$loadingPredicate(Head),
	assertz(Head).

%	The behaviour of things like dynamic/1 is different during consult.
$consulting :-
	once getprop($consulting, $consultingNow, true).

$loadingPredicate(Head) :-
	functor(Head, Pred, Arity),
	$loadingPredicate(Pred, Arity).

$loadingPredicate(Pred, Arity) :-
	( getprop(Pred, $consulting, Arity) ->
		true
	;	$redefiningPredicate(Pred, Arity),
		abolish(Pred, Arity),
		( $consulting ->
			addpropa(Pred, $consulting, Arity),
			addpropa($consulting, $consulting, Pred)
		)
	).

$redefiningPredicate(Head) :-
	functor(Head, Pred, Arity),
	$redefiningPredicate(Pred, Arity).

$redefiningPredicate(Pred, Arity) :-
	( $defined(Pred, Arity), prologFlag(redefinitionWarning, on) ->
		format(user_error, "~NWarning: ~a/~d redefined~n", [Pred, Arity])
	).

libdirectory(Lib) :-
	$defined(libraryDirectory, 1),
	libraryDirectory(Lib).
libdirectory(Lib) :-
	prologFlag(libdirectory, Lib).

lib(FileName) :-
	( $findFile(library(FileName), [".no"], 2'100, _Ext, Lib) ->
		true
	;	format(user_error,
			"~NError in ~w. -- cannot find library.~n",
			[lib(FileName)]),
		fail
	),
	( getprop($sourceFiles, $file, F), name(F, Lib) ->
		( prologFlag(redefinitionWarning, on) ->
			format(user_error,
				"~NError in ~w. -- library all ready loaded.~n",
				[lib(FileName)]),
			fail
		)
	;	$loadFile(Lib)
	).

iload$iload(Pred, Arity, Code) :-
	$toBytecodes(Code, BC, []),
	$iload(Pred, Arity, BC),
	!.

?- $toBytecodes(X, _, _) when X.
$toBytecodes([], CE, CE).
$toBytecodes(X.XT, C, CE) :-	
	$toBytecode(X, C, CE1),
	$toBytecodes(XT, CE1, CE).

$toBytecode(emit(Op, Args), (OpCode.ArgsL).CE, CE) :-
	append(Args, [], ArgsL),	% BUG!  Iload doesn't like strings!
	$bytecode(Op, OpCode, _).
$toBytecode(emit(Op, Args, Ext), (OpCode.ArgsL).C, CE) :-
	append(Args, [], ArgsL),	% BUG!  Iload doesn't like strings!
	$toExtension(Ext, C, CE),
	$bytecode(Op, OpCode, _).
$toBytecode($(Label), :(Label).CE, CE).
$toBytecode({_Comment}, CE, CE).

$toExtension($(Const), $(Const).CE, CE).
%$toExtension(String, #(String).CE, CE) :-
%	string(String),
%	putString(String).
$toExtension(Pred/Arity, $(Pred, Arity).CE, CE).
$toExtension(#(_, _, GroundTerm), #(GroundTerm).CE, CE).
$toExtension(&(Label), &(Label).CE, CE).
$toExtension(&(Pred, Arity), &(Pred, Arity).CE, CE).
$toExtension(cFunction(Name), cFunction(Name).CE, CE).
$toExtension(call(Address), call(Address).CE, CE).
$toExtension(table(Table), table(Table).CE, CE).
$toExtension(sotTable(L1, L2, L3, L4), [&(L1), &(L2), &(L3), &(L4)| CE], CE).

loadForeignFiles(Files, Libraries) :-
	( prologFlag(fload, fail) ->
		format(user_error,
			"~NForeign functions not supported/installed on this machine.~n",
			[]),
		fail
	),
	$catchError(
		fload$loadForeignFiles(Files, Libraries),
		loadForeignFiles(Files, Libraries),
		Error,
		Error
		),
	fail.
loadForeignFiles(_, _).

fload$loadForeignFiles(Files, Libraries0) :-
	( ground(Files), mapList($baseName, Files, FileBaseNames),
	  ground(Libraries0), mapList($nameToString, Libraries0, Libraries1) ->
		true
	;	$throwError(format(
			user_error, "~NGround lists of file names required.~n", []))
	),
	solutions(
		Function-Format,
		some F1.FSL.FS.FormatsX
		(	member(F1, FileBaseNames),
			solutions(
				FSX,
				some F2
				(	foreignFile(F2, FSX),
					$baseName(F2, F1)
				),
				FSL),
			( FSL == [] ->
				$throwError(format(
					user_error,
					"~NFile ~w not defined in foreignFile/2.~n",
					[F1]))
			),
			member(FS, FSL),
			member(Function, FS),
			solutions(FormatX, fload$foreign(Function, _, FormatX), FormatsX),
			( FormatsX = [Format] ->
				true
			; FormatsX == [] ->
				$throwError(format(
					user_error,
					"~NFunction ~w not defined in foreign/[2,3].~n",
					[Function]))
			;	$throwError(format(
					user_error,
					"~NFunction ~w multiply defined in foreign/[2,3] -- ~w.~n",
					[Function, FormatsX]))
			)
		),
		Functions),
	fload$checkFunctionSpecs(Functions),
	fload$functionNames(Functions, FunctionNames, Formats, Links),
	mapList($nameToString, Files, FileNames),
	append(Libraries1, ["-lc", "-lm"], LibraryNames),
%	mapList($nameToString, Libraries, LibraryNames),
	( fload$links(Links, LinkFile) ->
		$fload(LinkFile.FileNames, LibraryNames, FunctionNames, Addresses),
		unlink(LinkFile)
	;	$fload(FileNames, LibraryNames, FunctionNames, Addresses)
	),
	mapList(fload$install, Formats, Addresses).
%	fload$installL(Formats, Addresses).

fload$foreign(Function, c, Format) :-
	$defined(foreign, 2),
	foreign(Function, Format).
fload$foreign(Function, Language, Format) :-
	Language = c,						% BUG!  What about others?
	$defined(foreign, 3),
	foreign(Function, Language, Format).

?- fload$functionNames(X, Y, _, _) when X or Y.
fload$functionNames([], [], [], []).
fload$functionNames(
		(Function - Format).Functions,
		FunctionName.Names,
		Format.Formats,
		Links) :-
	Format = _/_,
	!,
	name(Function, FN),
	$listToString(0'_.FN, FunctionName),
	fload$functionNames(Functions, Names, Formats, Links).
fload$functionNames(
		(Function - Format).Functions,
		FunctionName.Names,
		Format.Formats,
		link(FN1, Format, FN2).Links) :-
	name(Function, FN1),
	append("NU_", FN1, FN2),
	( prologFlag(machine, sgi) ->
		$listToString(FN2, FunctionName)
	;	$listToString(0'_.FN2, FunctionName)
	),
	fload$functionNames(Functions, Names, Formats, Links).

fload$checkFunctionSpecs(Functions) :-
	( ground(Functions) ->
		true
	;	$throwError(format(
			user_error,
			"~NFunction specifications (~w) must be ground.~n",
			[Functions]))
	),
	( member(Function - Format, Functions),
	  \+ fload$checkFunctionSpec(Function, Format) ->
		$throwError(format(
			user_error,
			"~NFunction(~w, ~w) not a valid function specification.~n",
			[Function, Format]))
	).

fload$checkFunctionSpec(Function, Format) :-
	atom(Function),
	Format =.. Pred.Args,
	atom(Pred),
	mapList(fload$checkArg, Args),
	( member([_], Args, _.Rest) ->
		\+ member([_], Rest)
	).
%	(all Rest._1 delete([_1], Args, Rest) => (all _2 not member([_2], Rest))).
fload$checkFunctionSpec(Function, Pred/Arity) :-
	atom(Function),
	atom(Pred),
	integer(Arity),
	Arity >= 0.

fload$checkArg(+T) :-
	fload$type(T).
fload$checkArg(-T) :-
	fload$type(T).
fload$checkArg([-T]) :-
	fload$type(T).

fload$links(Links, LinkFileName) :-
	cons(Links),
	makeTemp("/tmp/lcXXXXXX.c", CFile),
	open(CFile, write, Stream),
	libdirectory(Lib),
	format(Stream, "#include \"~apublic.h\"~n", Lib),
	mapList(fload$link(Stream), Links),
	close(Stream),
	append(BaseFile, ".c", CFile),
	append(BaseFile, ".o", LinkFile),
	$listToString(LinkFile, LinkFileName),
	( access('/bin/cc', 1) ->
		CComp = '/bin/cc'
	; access('/usr/bin/cc', 1) ->
		CComp = '/usr/bin/cc'
	;	$throwError(
			format(user_error, "~NCan't find C compiler.~n", []))
	),
	( prologFlag(machine, encore) ->
		%	BUG!  Yeuch!
		sformat("cd /tmp; ~a -c ~s", [CComp, CFile], CC)
	;	sformat("~a -c ~s -o ~s", [CComp, CFile, LinkFile], CC)
	),
	system(CC, Status),
	unlink(CFile),
	( Status \== 0 ->
		$throwError(
			format(user_error, "~NSystem command \"~s\" failed.~n", [CC]))
	).

fload$link(Stream, link(Function, Format, Link)) :-
	format(Stream, "int ~s(X)~nregister Object *X;~n{~n", [Link]),
	fload$decl(Stream, Format),
	fload$call(Stream, Function, Format),
	fload$args(Stream, Format),
	putl(Stream, ");\n"),
	fload$returnArgs(Stream, Format),
	fload$return(Stream, Format),
	putl(Stream, "return(1);\n}\n").

fload$decl(Stream, Format) :-
	functor(Format, _, Arity),
	between(1, Arity, N),
	arg(N, Format, -Arg),
	fload$type(Arg, Type),
	format(Stream, "~a X~d;~n", [Type, N - 1]),
	fail.
fload$decl(_, _).

fload$call(Stream, Function, Format) :-
	( functor(Format, _, Arity), between(1, Arity, N), arg(N, Format, [-Arg]) ->
		fload$type(Arg, Type),
		format(Stream, "~a R;~n", [Type]),
		format(Stream, "~a ~s();~n", [Type, Function]),
		putl(Stream, "R = ")
	),
	format(Stream, "~s(", [Function]).

fload$args(Stream, Format) :-
	functor(Format, _, Arity),
	( arg(Arity, Format, [_]) ->
		LastN is Arity - 1
	;	LastN = Arity
	),
	between(1, Arity, N),
	arg(N, Format, RArg),
	fload$arg(Stream, RArg, N),
	( N < LastN ->
		putl(Stream, ", ")
	),
	fail.
fload$args(_, _).

fload$arg(Stream, +Arg, N) :-
	fload$extract(Arg, EFN),
	format(Stream, "~a(X[~d])", [EFN, N - 1]).
fload$arg(Stream, -_Arg, N) :-
	format(Stream, "&X~d", [N - 1]).

fload$returnArgs(Stream, Format) :-
	functor(Format, _, Arity),
	between(1, Arity, N),
	arg(N, Format, -Arg),
	fload$make(Arg, MFN),
	format(Stream, "X[~d] = ~a(X~d);~n", [N - 1, MFN, N - 1]),
	fail.
fload$returnArgs(_, _).

fload$return(Stream, Format) :-
	functor(Format, _, Arity),
	between(1, Arity, N),
	arg(N, Format, [-Arg]),
	fload$make(Arg, MFN),
	format(Stream, "X[~d] = ~a(R);~n", [N - 1, MFN]),
	fail.
fload$return(_, _).

fload$type(integer).
fload$type(float).
fload$type(single).
fload$type(double).
fload$type(atom).
fload$type(string).
fload$type(pointer).
fload$type(term).

fload$type(integer, int).
fload$type(float, double).
fload$type(single, float).
fload$type(double, double).
fload$type(atom, int).
fload$type(string, 'char *').
fload$type(pointer, 'char *').
fload$type(term, 'Object').

fload$extract(integer, 'NUeInt').
fload$extract(float, 'NUeNumber').
fload$extract(single, 'NUeNumber').
fload$extract(double, 'NUeNumber').
fload$extract(atom, 'NUeRef').
fload$extract(string, 'NUePrintName').
fload$extract(pointer, 'NUeRef').
fload$extract(term, '').				% BUG!  Too clever.

fload$make(integer, 'NUMakeInt').
fload$make(float, 'NUMakeFloat').
fload$make(single, 'NUMakeFloat').
fload$make(double, 'NUMakeFloat').
fload$make(atom, 'NUMakeAtom').
fload$make(string, 'NUStringToAtom').
fload$make(pointer, 'NUMakePointer').
fload$make(term, '').					% BUG!  Too clever.

fload$install(Pred/Arity, Address) :-
	!,
	iload$iload(Pred, Arity,
		[emit(frun, [Arity], call(Address)), emit(pro, [])]),
	iload$iload(Pred, Arity, []).
fload$install(Skel, Address) :-
	functor(Skel, Pred, Arity),
	\+
	(	between(1, Arity, N0),
		arg(N0, Skel, +Arg0),
		fload$ctype(Arg0, CType),
		N01 is N0 - 1,
		iload$iload(Pred, Arity, [emit(ctypx, [N01, CType])]),
		fail
	),
	\+
	(	between(1, Arity, N1),
		arg(N1, Skel, Arg1),
		(	Arg1 = -_
		;	Arg1 = [-_]
		),
		ArityN1 is Arity + N1 - 1,
		N11 is N1 - 1,
		iload$iload(Pred, Arity, [emit(gvarx, [ArityN1, N11])]),
		fail
	),
	iload$iload(Pred, Arity, [emit(frun, [Arity], call(Address))]),
	\+
	(	between(1, Arity, N2),
		arg(N2, Skel, Arg2),
		(	Arg2 = -_
		;	Arg2 = [-_]
		),
		ArityN2 is Arity + N2 - 1,
		N21 is N2 - 1,
		iload$iload(Pred, Arity, [emit(gvalx, [ArityN2, N21])]),
		fail
	),
	iload$iload(Pred, Arity, [emit(pro, [])]),
	iload$iload(Pred, Arity, []).

%	Note that there is no entry for 'term'.
fload$ctype(float, C) :- !, $ctype(number, C).
fload$ctype(single, C) :- !, $ctype(number, C).
fload$ctype(double, C) :- !, $ctype(number, C).
fload$ctype(string, C) :- !, $ctype(atom, C).
fload$ctype(pointer, C) :- !, $ctype($block, C).
fload$ctype(T, C) :- $ctype(T, C).
