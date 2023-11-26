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

?-($addop(1200,fx, (?-))).
?-$addop(1200,fx, (:-)).
?-$addop(1200,xfx, (:-)).
?-$addop(1200,xfx, (-->)).
?-$addop(1180, fx, use_if).
?-$addop(1170,fx,(if)).
?-$addop(1160,xfx, (else)).
?-$addop(1150,xfx, (then)).
?-$addop(1100,xfy,(;)).
?-$addop(1050,xfy,(->)).
?-$addop(1000,xfy,',').
?-$addop(900,fy, ls).
?-$addop(900,fy, listing).
?-$addop(900,fy, wait).
?-$addop(900,xfx, when).	% for NU-Prolog
?-$addop(900,fy, ~).
?-$addop(900,fy, not).
?-$addop(900,fy, once).
?-$addop(900,fy, \+).
?-$addop(900,fy, nospy).
?-$addop(900,fy, spy).
?-$addop(900,fy, lib).
?-$addop(700,xfx, =).
?-$addop(700,xfx, ~=).
?-$addop(700,xfx, \=).
?-$addop(700,xfx, is).
?-$addop(700,xfx, =..).
?-$addop(700,xfx, ==).
?-$addop(700,xfx, \==).
?-$addop(700,xfx, =:=).
?-$addop(700,xfx, =\=).
?-$addop(680,xfy, or).
?-$addop(660,xfy, and).
?-$addop(630,xfx, <).
?-$addop(630,xfx, >).
?-$addop(630,xfx, =<).
?-$addop(630,xfx, >=).
?-$addop(600,xfy,'.').
?-$addop(500,yfx,+).
?-$addop(500,yfx,-).
?-$addop(500,yfx,/\).
?-$addop(500,yfx,\/).
?-$addop(500,fx,(+)).
?-$addop(500,fx,(-)).
?-$addop(500,fx,\).
?-$addop(400,yfx,*).
?-$addop(400,yfx,/).
?-$addop(400,yfx,<<).
?-$addop(400,yfx,>>).
?-$addop(300,xfx,mod).
?-$addop(200,xfy,'^').
?-$hide($trace), $hide(seeng(1)), $hide(tellng(1)),
	$hide(done(1)), $hide(ifile(1, 1)), $hide(ofile(1, 1)).

protect([]).
protect(A.B) :-
	$expand(A, C),
	$protect(C),
	protect(B).
protect(A) :-
	A ~= [],
	A ~= _._,
	$expand(A, C),
	$protect(C).

hide([]).
hide(A.B) :-
	$expand(A, C),
	$hide(C),
	hide(B).
hide(A) :-
	A ~= [],
	A ~= _._,
	$expand(A, C),
	$hide(C).

clindex(A) :-
	clindex(1, A).

clindex(_, []).
clindex(N, A.B) :-
	$expand(A, C),
	$clindex(N, C),
	clindex(N, B).
clindex(N, A) :-
	A ~= [],
	A ~= _._,
	$expand(A, C),
	$clindex(N, C).

$expand(A, A) :-
	atom(A).
$expand(A, B) :-
	A =.. F.N.[],
	length(L, N),
	B =.. F.L.

-X.Y :-
	!,
	$debug(0),
	write(1, 'reconsulting ', 0),
	writeln(1, X),
	reconsult(X),
	writeln(1, done),
	(	Y = [],
		!
	;
		Y
	).
$(X).Y :-
	!,
	$debug(0),
	write(1, 'reconsulting ', 0),
	writeln(1, X),
	reconsult(X),
	writeln(1, done),
	(	Y = [],
		!
	;
		Y
	).
X.Y :-
	$debug(0),
	write(1, 'consulting ', 0),
	writeln(1, X),
	consult(X),
	writeln(1, done),
	(	Y = [],
		!
	;
		Y
	).

get0(A) :-
	getc(3, B),
	(	B >= 0,
		A = B
	;
		B < 0,
		A = 26
	).

get(A) :-
	repeat,
	get0(X),
	(	X > 32
	;
		X = 26
	),
	!,
	A = X.

skip(N, X) :-
	(	var(X)
	;
		atom(X)
	),
	write(2, 'incorrect use of skip', 0),
	nl(2),
	!,
	fail.
skip(N, X) :-
	repeat,
	getc(N, C),
	(	C = X
	;	 
		$memb(C, X)
	;
		C < 0,
		!,
		fail
	),
	!.

skip(X) :-
	skip(3, X).

put(X) :-
	integer(X),
	putc(4, X).
put([]).
put(A.B) :-
	putc(4, A),
	put(B).

write(A) :-
	write(4, A).

writef(A, F) :-
	write(4, A, F).

print(A) :-
	portray(A),
	!.
print(A) :-
	write(4, A).

portray(0).

?-$ret((portray(0) :- true)).

portraygoals(A) :-
	write(4, A, 2'110110).

display(A) :-
	write(4, A, 11).

putatom(A) :-
	write(4, A).

printf(A, B) :-
	fprintf(4, A, B).

nl :-
	putc(4, 10).

nl(F) :-
	putc(F, 10).

read(X) :-
	read(3, X).

assertz(X) :-			% faster when database system finished
	assert(X).

deny(X, Y) :-			% faster when database system finished
	$ret((X :- Y)).

writeln(N, X) :-
	write(N, X),
	putc(N, 10).

writeln(X) :-
	write(4, X),
	putc(4, 10).

call(A) :-
	A.

A \= A :-
	!,
	fail.
_ \= _.

A == B :-
	not A ~= B.

A \== B :-
	not not A ~= B.

functor(T, F, N) :-
	T =.. F.A,
	length(A, N).

maxint(536870911).

eof((?-end)).

shell(A) :-
	name(B, A),
	system(B, _).

more(A) :-
	name(A, B),
	$app("more ", B, C),
	shell(C).

edit(A) :-
	name(A, B),
	$app("vi ", B, C),
	shell(C),
	-A.[].

sh :-
	system(sh, _).

csh :-
	system(csh, _).

trimcore.

notrace :-
	trace(0).

trace :-
	trace(15).

backtrace :-
	backtrace(10).

restart :-
	abort.

op(X, Y, []) :-
	!.
op(X, Y, A.B) :-
	!,
	$addop(X, Y, A),
	op(X, Y, B).
op(X, Y, Z) :-
	$addop(X, Y, Z).

arg(N, T, A) :-
	T =.. F.A1,
	$membn(N, A1, A).

tab(0).
tab(N) :-
	N > 0,
	N1 is N - 1,
	putc(4, 32),
	tab(N1).

?-wait $membn(1, 0, 1).
?-wait $membn(0, 1, 1).
$membn(1, A.B, A).
$membn(N, A.B, C) :-
	N > 1,
	plus(M, 1, N),
	$membn(M, B, C).

$memb(A, A.B).
$memb(A, B.C) :-
	$memb(A, C).

$app([], A, A).
$app(A.B, C, A.D) :-
	$app(B, C, D).

?-wait length(1, 0).
?-wait length(0, 1).
length([], 0).
length(A.B, N) :-
	N > 0,
	plus(N1, 1, N),
	length(B, N1).

if C then A else B :-
	$nground(C).
if C then A else B :-
	$ndelay(N),
	C,
	$ndelay(M),
	(	N =\= M,
		!,
		error(enotdelay, (if C then A else B))
	;
		!,
		A
	).
if C then A else B :-
	!,
	B.
if C then A :-
	$nground(C).
if C then A :-
	$ndelay(N),
	C,
	$ndelay(M),
	(	N =\= M,
		!,
		error(enotdelay, (if C then A))
	;
		!,
		A
	).
if C then A.

~A :-
	$nground(A).
~A :-
	$ndelay(N),
	A,
	$ndelay(M),
	(	N =\= M,
		!,
		error(enotdelay, ~(A))
	;
		!,
		fail
	).
~A.

asserta((X :- Y)) :-		% faster when database system finished (maybe)
	!,
	$nassert((X :- Y), 1).
asserta(X) :-
	$nassert((X :- true), 1).

$proc(T, F, N) :-
	$proc(A, B),
	F = A,
	N = B,
	functor(T, F, N).

(ls) :-
	$debug(0),
	(listing).

ls(X) :-
	$debug(0),
	listing(X).

retract((A :- B)) :-			% faster when database system finished
	!,
	$ret((A :- B)).
retract(A) :-
	$ret((A :- true)).

retractall(A) :-
	$ret((A :- B)),
	fail.
retractall(A).

clause(A, B) :-
	$nclause((A :- B), C, N),
	(A :- B) = C.

not A :-
	A,
	!,
	fail.
not A.

\+A :-
	A,
	!,
	fail.
\+A.

once(A) :-
	A,
	!.

	% this is actually a clause of ;
	% ; should really be defined here (is it needed earlier?)
?- $nassert((A -> B ; C :- $lcut, $cond(A, B, C)), 1).

A -> B :-
	A,
	!,
	B.
A -> B.

$cond(A, B, C) :-
	A,
	!,
	B.
$cond(A, B, C) :-
	C.

consult(F) :-
	seeng(I),
	see(F),
	repeat,
	read(T),
	$con1(T),
	seen,
	see(I),
	!.

$con1((A :- B)) :-
	!,
	$nassert((A :- B), _),
	fail.
$con1((?-end)) :-
	!.
$con1((?-Q)) :-
	!,
	Q,
	!,
	fail.
$con1((:-Q)) :-
	!,
	Q,
	!,
	fail.
$con1((A --> B)) :-
	!,
	$gexpand(A, B, A1, B1),
	!,
	$nassert((A1 :- B1), _),
	fail.
$con1(X) :-
	assert(X),
	fail.

reconsult(A) :-
	clause(done(_), fail),
	write(2, 'Warning: nested reconsult', 0),
	nl,
	fail.
reconsult(A) :-
	$nassert((done(_) :- fail), _),
	seeng(I),
	see(A),
	repeat,
	read(T),
	$try(T),
	seen,
	see(I),
	retractall(done(_)),
	!.

$try((H :- T)) :-
	!,
	$recdone(H),
	$nassert((H :- T), _),
	fail.
$try((?-end)) :-
	!.
$try((?-wait P)) :-
	!,
	$recdone(P),
	wait P,
	fail.
$try((?-G)) :-
	!,
	G,
	!,
	fail.
$try((:-G)) :-
	!,
	G,
	!,
	fail.
$try((A --> B)) :-
	!,
	$gexpand(A, B, A1, B1),
	!,
	$recdone(A1),
	$nassert((A1 :- B1), _),
	fail.
$try(C) :-
	$recdone(C),
	assert(C),
	fail.

$recdone(H) :-
	done(H),
	!.
$recdone(H) :-
	functor(H, F, N),
	functor(P, F, N),
	$nassert((done(P):-true), 1),
	retractall(P),
	$rmspy(P),
	$retwait(P),
	!.

$rmspy(_).			% defined in spy.pl, if loaded

lib F :-
	$debug(0),
	name(F, S),
	libdirectory(LC),
	name(LC, LS),
	$app(LS, 47.S, S1),
	name(N, S1),
	-N.[].

use_if(X) :-
	not(X),
	repeat,
	read(Y),
	nonvar(Y),
	(	Y = (?- use_else)
	;
		Y = (?- use_end)
	;
		eof(Y),	
		writeln('Unmatched "use"')
	;
		Y = (?- use_if _),
		use_else,
		fail
	),
	!.
use_if(_).

use_else :-
	repeat,
	read(Y),
	nonvar(Y),
	(	Y = (?- use_end)
	;
		eof(Y),	
		writeln('Unmatched "use"')
	;
		Y = (?- use_if _),
		use_else,
		fail
	),
	!.

use_end.

muprolog.
nuprolog :- fail.

pipe(A) :-
	(	ifile(A, _)
	;
		ofile(A, _)
	),
	write(2, 'pipe name exists', 0),
	nl,
	!,
	fail.
pipe(A) :-
	pipe(I, O),
	assert(ifile(A, I)),
	assert(ofile(A, O)).

seeing(A) :-
	seeng(A).

seeng(user).

ifile(user, 0).

see(F) :-
	ifile(F, N),
	!,
	$fcopy(N, 3),
	retract(seeng(_)),
	assert(seeng(F)),
	!.
see(F) :-
	open(F, N, r),
	assert(ifile(F, N)),
	$fcopy(N, 3),
	retract(seeng(_)),
	$nassert((seeng(F) :- true), 1),
	!.

seen :-
	seeng(F),
	(	F ~= user,
		ifile(F, N),
		close(N),
		retract(ifile(F, N)),
		see(user)
	;
		F = user
	).

telling(A) :-
	tellng(A).

tellng(user).

ofile(user, 1).

tell(F) :-
	ofile(F, N),
	!,
	$fcopy(N, 4),
	retract(tellng(_)),
	assert(tellng(F)),
	!.
tell(F) :-
	open(F, N, w),
	assert(ofile(F, N)),
	$fcopy(N, 4),
	retract(tellng(_)),
	assert(tellng(F)),
	!.

told :-
	tellng(F),
	(	F ~= user,
		ofile(F, N),
		close(N),
		retract(ofile(F, N)),
		tell(user)
	;
		F = user
	),
	!.

next(I, O) :-
	(	I = user,
		!
	;	 
		(	ifile(I, IN),
			!
		;
			open(I, IN, r)
		),
		$fmove(IN, 0)
	),
	(	O = user,
		!
	;	 
		(	ofile(O, ON),
			!
		;
			open(O, ON, w)
		),
		$fmove(ON, 1)
	).

$trace :-
	$debug(0).

trace(N) :-
	$debug(0),
	retract(($trace :- _)),
	assert(($trace :- $debug(N))),
	!.

$err(X) :-
	$errnum(N),
	write(2, 'ERROR ', 0),
	write(2, N),
	write(2, ', call: ', 0),
	write(2, X),
	nl(2),
	exit(1).

% TODO
% * Should this be handled by "lib db."?
% * Or done via consulting "dbpreds.pl"?
% ?-database, 'dbpreds.pl'.[].	/* mods for database version */

?-
$protect(put(1)),
$protect((1 \== 1)),
$protect(tell(1)),
$protect(assertz(1)),
$protect(nl),
$protect(told),
$protect(nl(1)),
$protect((1, 1)),
$protect($do(1)),
$protect(1.1),
$protect(skip(1)),
$protect(skip(1, 1)),
$protect(end),
$protect(maxint(1)),
$protect(trace(1)),
$protect(lib(1)),
$protect(assert(1)),
$protect(sh),
$protect(op(1, 1, 1)),
$protect((ls)),
$protect(print(1)),
$protect(putatom(1)),
$protect(ls(1)),
$protect(true),
$protect(see(1)),
$protect(next(1, 1)),
$protect(protect(1)),
$protect(portraygoals(1)),
$protect((1 ; 1)),
$protect(get(1)),
$protect(asserta(1)),
$protect((1 = 1)),
$protect(hide(1)),
$protect(clindex(1)),
$protect(clindex(1, 1)),
$protect(retract(1)),
$protect(fail),
$protect(get0(1)),
$protect(call(1)),
$protect(read(1)),
$protect((1 \= 1)).
?-
$protect(retractall(1)),
$protect(clause(1, 1)),
$protect(pipe(1)),
$protect(not(1)),
$protect(if(1)),
$protect(functor(1, 1, 1)),
$protect(write(1)),
$protect(libdirectory(1)),
$protect(seen),
$protect(writeln(1)),
$protect(length(1, 1)),
$protect((1 == 1)),
$protect(writeln(1, 1)),
$protect(reconsult(1)),
$protect($start(1)),
$protect(consult(1)),
$protect(~(1)),
$protect(seeing(1)),
$protect(telling(1)),
$protect($con(1))
.
?-
$protect(shell(1)),
$protect(csh),
$protect(arg(1,1,1)),
$protect(tab(1)),
$protect(deny(1,1)),
$protect(writef(1,1)),
$protect(display(1)),
$protect(eof(1)),
$protect(printf(1,1)),
$protect(\+(1)),
$protect((1 -> 1)),
$protect($cond(1,1,1)),
$protect(notrace),
$protect(trimcore),
$protect(restart),
$protect(more(1)),
$protect(edit(1)),
$protect(backtrace),
$protect(trace)
.
?-
$protect($app(1,1,1)),
$protect($memb(1,1)),
$protect($ret(1)),
$protect($con1(1)),
$protect($try(1)),
$protect($recdone(1)),
$protect($expand(1,1)),
$protect($proc(1,1,1)),
$protect($membn(1,1,1)),
$protect(use_if(1)),
$protect(use_else),
$protect(use_end),
$protect(muprolog),
$protect(nuprolog),
$protect(once(1)).
