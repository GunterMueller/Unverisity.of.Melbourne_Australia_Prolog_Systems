/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog unix predicates.

access(P, M) :-
	$nameToString(P, PN),
	$syscall(access, PN, M, 0, 0).

chdir(D) :-
	$nameToString(D, DN),
	$syscall(chdir, DN, 0, 0, 0).

cd(D) :- chdir(D).

chmod(P, M) :-
	$nameToString(P, PN),
	$syscall(chmod, PN, M, 0, 0).

environ(Env) :- $syscall(environment, 0, 0, 0, Env).

getenv(Name, Value) :-
	environ(Env),
	( atom(Name) ->
		memberchk(Name = Value, Env)
	;	member(Name = Value, Env)
	).

getegid(Id) :- $syscall(getegid, 0, 0, 0, Id).

getgid(Id) :- $syscall(getgid, 0, 0, 0, Id).

getgroups(Groups) :- $syscall(getgroups, 0, 0, 0, Groups).

getlogin(Login) :- $syscall(getlogin, 0, 0, 0, Login).

getpid(Pid) :- $syscall(getpid, 0, 0, 0, Pid).

getppid(Pid) :- $syscall(getppid, 0, 0, 0, Pid).

getpw(N, PWEnt) :-
	$nameToString(N, NN),
	$syscall(getpwnam, NN, 0, 0, PWEnt).
getpw(Uid, PWEnt) :-
	integer(Uid),
	$syscall(getpwuid, Uid, 0, 0, PWEnt).

getuid(Uid) :- $syscall(getuid, 0, 0, 0, Uid).

geteuid(Uid) :- $syscall(geteuid, 0, 0, 0, Uid).

getwd(CWD) :- $syscall(getwd, 0, 0, 0, CWD).

hostname(Hostname) :- $syscall(hostname, 0, 0, 0, Hostname).

kill(Pid, SigName) :-
	((ground(SigName), integer(Pid))
	->	true
	;	format(user_error,
			"~NError in ~w.~nPid and Signal expected.~n",
			[kill(Pid, SigName)]),
		fail
	),
	(integer(SigName)
	->	SigNum = SigName
	;	$signumber(SigName, SigNum)
	),
	$syscall(kill, Pid, SigNum, 0, 0).

link(F1, F2) :-
	$nameToString(F1, FN1),
	$nameToString(F2, FN2),
	$syscall(link, FN1, FN2, 0, 0).

mkdir(D) :-
	$nameToString(D, DN),
	$syscall(mkdir, DN, 8'777, 0, 0).

random(R) :- $syscall(random, 0, 0, 0, R).

rename(F1, F2) :-
	$nameToString(F1, FN1),
	$nameToString(F2, FN2),
	$syscall(rename, FN1, FN2, 0, 0).

rmdir(D) :-
	$nameToString(D, DN),
	$syscall(rmdir, DN, 0, 0, 0).

sleep(S) :- $syscall(sleep, S, 0, 0, 0).

stat(P, Status) :-
	$nameToString(P, PN),
	$syscall(stat, PN, 0, 0, Status),
	Status ~= [].

time(TimeList) :-
	$syscall(time, 0, 0, 0, TimeList).

time(TimeStamp, TimeList) :-
	integer(TimeStamp),
	$syscall(time, TimeStamp, 1, 0, TimeList).

truncate(F, Length) :-
	$nameToString(F, FN),
	$syscall(truncate, FN, Length, 0, 0).

umask(U) :- $syscall(umask, U, 0, 0, _).

unlink(F) :-
	$nameToString(F, FN),
	$syscall(unlink, FN, 0, 0, 0).

wait(Pid, Status) :-
	$syscall(wait, 0, 0, 0, Pid.Status),
	Pid >= 0.

directory(P, Files) :-
	$nameToString(P, PN),
	$syscall(readdir, PN, 0, 0, Files),
	cons(Files).

sh :-
	system("/bin/sh").
csh :-
	system("/bin/csh").

shell(X) :-
	system(X, _).

system(X) :-
	system(X, _).

system(C, Status) :-
	$nameToString(C, CN),
	$syscall(system, CN, 0, 0, Status).

system(C, Args, Status) :-
	(fork(Pid)
	->	once
		(	repeat,
			wait(Pid, Status0)
		),
		Status0 = Status
	;	(	exec(C, Args)
		;	exit(-1)
		)
	).

fork :-
	fork(_).

fork(Pid) :-
	$syscall(fork, 0, 0, 0, Pid),
	Pid > 0.

exec(C, Args) :-
	$nameToString(C, CN),
	mapList($nameToString, Args, ArgsN),
	$syscall(exec, CN, ArgsN, 0, 0).

statistics(S) :-
	$syscall(statistics, 0, 0, 0, S).

statistics :-
	statistics(S),
	member(memory = [Memory, _], S),
	member(program = [Program, _], S),
	member(global = [GlobalUsed, GlobalFree], S),
	member(local = [LocalUsed, LocalFree], S),
	member(trail = [TrailUsed, TrailFree], S),
	member(utime = [UTime, _], S),
	member(stime = [STime, _], S),
	!,
	format("total memory    ~d bytes~n", [Memory]),
	format("  program space ~d bytes~n", [Program]),
	format("  global stack  ~d bytes = ~d in use + ~d free~n",
		[GlobalUsed + GlobalFree, GlobalUsed, GlobalFree]),
	format("  local stack   ~d bytes = ~d in use + ~d free~n",
		[LocalUsed + LocalFree, LocalUsed, LocalFree]),
	format("  trail stack   ~d bytes = ~d in use + ~d free~n",
		[TrailUsed + TrailFree, TrailUsed, TrailFree]),
	format("~3d sec. runtime = ~3d sec. user + ~3d sec. system~n",
		[UTime + STime, UTime, STime]).

$fseek(Stream, Offset, From) :-
	ground(Stream),
	ground(Offset),
	ground(From),
	once $anyCurrentStream(FD, Stream),		% No need to leave a cp
	$syscall(fseek, FD, Offset, From, 0).

$ftell(Stream, Spot) :-
	ground(Stream),
	$anyCurrentStream(FD, Stream),
	$syscall(ftell, FD, 0, 0, Spot).

$fesq(P, Q) :-
	$syscall(fesq, 0, 0, 0, P.Q).

$signal(Sig, Action) :-
	integer(Sig),
	atom(Action),
	$syscall(signal, Sig, Action, 0, 0).
