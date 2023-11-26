/*
 * rule.nl  -  external database interface for RULE
 *
 * Copyright (C) 1986, The University of Melbourne
 */


%
% $rule_setup
%
$rule_setup(Db, X) :-
	X =.. FX._,
	atomToString(FX, XS),
	atomToString(Db, DbS),
	append(DbS, "/", F1),
	append(F1, XS, F2),
	append(F2, "/", F3),
	append(F3, "r.no", File),
	load(File).

%
% $rule_assertz	
%	access an external database rule
%
$rule_assertz(Db, X) :-
	(	X = (H :- B),
		H =.. FX._
	;	X ~= (H :- B),
		X =.. FX._
	),
	atomToString(FX, XS),
	atomToString(Db, DbS),
	append(DbS, "/", F1),
	append(F1, XS, F2),
	append(F2, "/", F3),
	append(F3, "r.nl", File),
	open(File, append, Stream),
	writev(Stream, [quote,prefix], X),
	put(Stream, 0'.),
	nl(Stream),
	close(Stream),
	$compile(File).


%
% $rule_asserta:
%	add a new record to the database
%
$rule_asserta(Db, X) :-
	(	X = (H :- B),
		H =.. FX._
	;	X ~= (H :- B),
		X =.. FX._
	),
	atomToString(FX, XS),
	atomToString(Db, DbS),
	append(DbS, "/", F1),
	append(F1, XS, F2),
	append(F2, "/", F3),
	append(F3, "r.nl", File),
	append(File, ".tmp", TmpFile),
	append("mv ", File, C1),
	append(C1, " ", C2),
	append(C2, TmpFile, MvCmd),
	system(MvCmd, 0),
	open(File, write, Stream),
	writev(Stream, [quote,prefix], X),
	put(Stream, 0'.),
	nl(Stream),
	close(Stream),
	append("cat ", TmpFile, C3),
	append(C3, " >> ", C4),
	append(C4, File, CatCmd),
	system(CatCmd, 0),
	$compile(File).


%
% rule_retract:
%	delete matching records one by one from database
%
$rule_retract(Db, X) :-
	(	X = (H :- B),
		H =.. FX._
	;	X ~= (H :- B),
		X =.. FX._
	),
	atomToString(FX, XS),
	atomToString(Db, DbS),
	append(DbS, "/", F1),
	append(F1, XS, F2),
	append(F2, "/", F3),
	append(F3, "r.nl", File),
	append(File, ".tmp", TmpFile),
	append("mv ", File, C1),
	append(C1, " ", C2),
	append(C2, TmpFile, MvCmd),
	repeat,
	system(MvCmd, 0),
	open(TmpFile, read, TmpStream),
	open(File, write, Stream),
	($rule_copy(TmpStream, Stream, X) ->
		$rule_copy(TmpStream, Stream, end_of_file),
		close(Stream),
		close(TmpStream),
		$compile(File)
	;	!,
		close(Stream),
		close(TmpStream),
		$compile(File),			% this is not necessary, and
						% should not affect anything
		fail
	).

$rule_copy(Stream1, Stream2, UntilTerm) :-
	repeat,
	read(Stream1, Term),
	(	Term = UntilTerm,
		!,
		true
	;	nonvar(Term),
		eof(Term),
		!,
		fail
	;	writev(Stream2, [quote,prefix], Term),
		put(Stream2, 0'.),
		nl(Stream2),
		fail
	).

%
% rule_rall:
%	delete all matching records from database
%	(should be rewritten properly - ie only on pass and compile)
%

$rule_rall(D, P) :-
	$rule_retract(D, P), fail.

$rule_rall(_, _).

%
% $compile(File)
%
% compile and load nuprolog src File
%

$compile(File) :-
	append("nc -c ", File, Cmd),
	system(Cmd, Status),
	append(BaseFile, ".nl", File),
	append(BaseFile, ".no", LoadFile),
%dbug	writeln(LoadFile),
	load(LoadFile).

%
% $rule_define:
%	create a clean, new relation
%
$rule_define(Db, Rname, Arity) :-
	atomToString(Rname, RnameS),
	atomToString(Db, DbS),
	append(DbS, "/", F1),
	append(F1, RnameS, Dir),
	append("mkdir ", Dir, Cmd1),
	system(Cmd1, 0),
	append(Dir, "/", F3),
	append(F3, "r.nl", File),
	append("touch ", File, Cmd2),
	system(Cmd2, 0),
	$compile(File).

%
% $rule_undefine:
%	remove a relation
%
%
$rule_undefine(Db, R) :-
	atomToString(Db, DbS),
	append("rm -rf ", DbS, S1),
	name(R, RS),
	append(S1, 0'/.RS, Cmd),
%dbug	writeln(Cmd),
	system(Cmd, 0).

