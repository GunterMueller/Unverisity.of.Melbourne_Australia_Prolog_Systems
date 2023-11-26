/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog error predicates.

sys$error(warning, Message) :-
	format(user_error, "~NWarning: ~w.~n", [Message]).
sys$error(fatal, Message) :-
	format(user_error, "~NFatal Error: ~w.~n", [Message]),
	abort.

$catchError(Goal, Error, Action) :-
	$catchError(Goal, Goal, Error, Action).

$catchError(Goal, ApparentGoal, Error, Action) :-
	catch(Goal, sys$error(Error)),
	(nonvar(Error) ->
		!,
		format(user_error, "~NError in ~w.~n", [ApparentGoal]),
		call(Action),
		!,
		fail
	).

$throwError(Error) :-
	throw(sys$error(Error)).
