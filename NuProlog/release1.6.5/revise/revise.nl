/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

% Nepolog "incremental compilation" predicates.
% Author: John Shepherd, modified by Lee Naish

	% revise/0 revises last .no file loaded
	% (or repeats last revise)
revise :-
	getprop($sourceFiles, $file, LastFile),
	name(LastFile, LFS),
	append(LFS1, ".no", LFS),
	!,
		% delete '@' if there is one
	( delete(0'@, LFS1, LFS3) ->
		LFS4 = LFS3
	;
		LFS4 = LFS1
	),
	$reviseFile(LFS4).

	% revise/1 revises a file or predicate or tag (no tags now!)
	% depending on what the argument looks like
revise(X.nl) :-
	!,
	name(X, XS),
	$reviseFile(XS).
revise(Pred/Arity) :-
	!,
	revise$currentPredicate(Pred, Arity),
	revise$revisePred(Pred, Arity).
revise(Pred) :-
	atom(Pred),
	revise$currentPredicate(Pred, Arity),	% any Arity ok
	!,
	revise$revisePred(Pred, Arity).
	% need way to find file when using tags
	% - grep tag file?
%revise(Tag) :-
%	name(Tag, TagName),
%	append("-t ", TagName, TOpt),
%	$runRevise(TOpt, []).

revise$revisePred(Pred, Arity) :-
	$defined(Pred, Arity, SourceFile, _),
	name(SourceFile, SourceFileName),
	append(SourceFileBaseName, ".no", SourceFileName),
		% delete '@' if there is one
	( delete(0'@, SourceFileBaseName, SFBN1) ->
		SFBN2 = SFBN1
	;
		SFBN2 = SourceFileBaseName
	),
	name(Pred, PredName),
	append("-p ", PredName, Flag),
	!,
	$runRevise(Flag, SFBN2).

revise$currentPredicate(Pred, Arity) :-
	currentPredicate(Pred, Arity),
	\+ systemPredicate(Pred, Arity).

$reviseFile(FileName) :-
	$runRevise("-f", FileName).

$runRevise(Options, File) :-
	revise$comdir(ComDir),
	append(Options, 0' .File, C0),
	append("revise ", C0, C1),
	append(ComDir, C1, RevCmd),
	system(RevCmd),
	append(File, "@.no", LFS),
	name(LF, LFS),
	load(LFS).

revise$comdir(ComDir) :-
	$getFlag(commanddirectory, C),
	name(C, ComDir).
