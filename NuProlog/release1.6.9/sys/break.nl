/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog interrupt predicates.

break :-
	getprop($mainloop, $breaklevel, Break),
	NewBreak is Break + 1,
	$break(NewBreak).

$break(Break) :-
	$mainloop([], Break).

%	Process an interrupt -- usually a <DEL>.
%	Bit grotty at the moment -- especially if ^D given as Action.
$break(_Break, _Sig, _N) :-
	nl(user),
	repeat,
	putl(user, "Prolog interrupted (h for help)? "),
	flushOutput(user),					% BUG? in System V line-buffering
	get(user, Action),
	(Action == -1 ->
		clearIOError(user_input),
		$close(user_input),				% Re-open user_input on terminal
		abort
	;	skip(user, 10),
		$breakAction(Action)
	).

$breakAction(0'?) :-
	$breakAction(0'h).
$breakAction(0'a) :-
	abort.
$breakAction(0'b) :-
	break.
$breakAction(0'c) :-
	true.
$breakAction(0'd) :-
	debug,
	$setFlag(localDebug, debug).
$breakAction(0'e) :-
	exit(0).
$breakAction(0'h) :-
	putl(user,
"Prolog interrupt options:
    a    abort
    b    break
    c    continue
    d    debug
    e    exit
    h/?  help
    t    trace
"),
	fail.
$breakAction(0't) :-
	trace,
	$setFlag(localDebug, trace).

%	Two convenient ternary predicates for the receipt of signals.
$exit(_Break, _Sig, _N) :-
	exit(1).
$ignore(_Break, _Sig, _N).

signal(SigName, Action) :-
	( ground(SigName), atom(Action) ->
		true
	;	format(user_error,
			"~NError in ~w -- signal name and atomic action expected.~n",
			[signal(SigName, Action)]),
		fail
	),
	( integer(SigName) ->
		Sig = SigName
	;	$signumber(SigName, Sig)
	),
	$signal(Sig, Action).

$signumber(sighup, 1).		/* hangup */
$signumber(sigint, 2).		/* interrupt (rubout) */
$signumber(sigquit, 3).		/* quit (ASCII FS) */
$signumber(sigill, 4).		/* illegal instruction (not reset when caught)*/
$signumber(sigtrap, 5).		/* trace trap (not reset when caught) */
$signumber(sigiot, 6).		/* IOT instruction */
$signumber(sigemt, 7).		/* EMT instruction */
$signumber(sigfpe, 8).		/* floating point exception */
$signumber(sigkill, 9).		/* kill (cannot be caught or ignored) */
$signumber(sigbus, 10).		/* bus error */
$signumber(sigsegv, 11).	/* segmentation violation */
$signumber(sigsys, 12).		/* bad argument to system call */
$signumber(sigpipe, 13).	/* write on a pipe with no one to read it */
$signumber(sigalrm, 14).	/* alarm clock */
$signumber(sigterm, 15).	/* software termination signal from kill */
$signumber(sigusr1, 16).	/* user defined signal 1 */
$signumber(sigusr2, 17).	/* user defined signal 2 */
$signumber(sigcld, 18).		/* death of a child */
$signumber(sigpwr, 19).		/* power-fail restart */
