/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/


?-hide(sprog(2).getpath(3).[]).

$main([]) :-		% for initial boot
	exit(0).
$main(_.Prog.[]) :-
	getpath("./", Prog, Path),
	assert(pp(Prog, Path)),
	fail.
$main(_.Prog.Dir.[]) :-
	name(Dir, SDir),
	$app(SDir, "/", SD),
	getpath(SD, Prog, Path),
	assert(pp(Prog, Path)),
	fail.
$main(Argv) :-
	retract(pp(Prog, Path)),
	sprog(Prog, Path).
$main(Argv) :-
	writeln(2, 'Usage: plsave f.pl {dir}'),
	exit(1).

getpath(SDir, Prog, Path) :-
	name(Prog, SProg),
	$app(SP, ".pl", SProg),
	$app(SDir, SP, SPath),
	name(Path, SPath).

sprog(Prog, Save) :-
	retractall(getpath(_, _, _)),
	retractall($main(_)),
	reconsult(Prog),
	fail.
sprog(Prog, Save) :-
	retractall(sprog(_, _)),
	save(Save),
	argv(X),
	X = S.Argv,
	name(S, SS),
	(	$app(_, "plsave", SS)
	;	 
		main(S.Argv)
	;
		exit(999)
	),
	exit(0).
