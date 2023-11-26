revise :-
	runRevise("").
revise(X.nl) :-
	reviseFile(X).
revise(X.ns) :-
	reviseFile(X).
revise(X.no) :-
	reviseFile(X).
revise(Pred/Arity) :-
	$defined(Pred, Arity, File, _),
	append(F, ".no", File),
	reviseFile(F).
revise(Tag) :-
	name(Tag, TagName),
	append("-t ", TagName, Flag),
	runRevise(Flag).
revise(Pred, Arity) :-
	$defined(Pred, Arity, File, _),
	append(F, ".no", File),
	reviseFile(F).

reviseFile(F) :-
	name(F, FileName),
	append("-f ", FileName, Flag),
	runRevise(Flag).

runRevise(Options) :-
	loadedFiles(List),
	append("/mip/jas/bin/revise ", Options, C1),
	append(C1, List, RevCmd),
writeln(RevCmd),
	shell(RevCmd),
	load('@.no').
	
loadedFiles(List) :-
	properties(sourceFile, name, Files),
writeln(Files),
	makeFileList(Files, [], List).

makeFileList([], L1, L2) :-
	L2 = L1.
makeFileList(F.Rest, L1, L2) :-
	append("/mip/jas/lib/nuprolog/lib", _, F),
	makeFileList(Rest, L1, L2).
makeFileList(F.Rest, L1, L2) :-
	F = "@",
	makeFileList(Rest, L1, L2).
makeFileList(F.Rest, L1, L2) :-
	append(L1, " ", L1a),
	append(L1a, F, L1b),
	makeFileList(Rest, L1b, L2).
