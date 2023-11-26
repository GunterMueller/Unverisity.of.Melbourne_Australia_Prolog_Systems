/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart
 */

% Nepolog aggregation predicates.

% Aggregate functions: $min/4, $max/4 $count/4, $sum/5,
%			$insert/4, delete/4, $update/5
% 
% All predicates delay until Globals are ground.
% 
% $min(Globals, Term, Goal, Result):
% 	Result is the minimum (via compare/3) Term produced by Goal.
%	Terms may contain local variables.
% $max(Globals, Term, Goal, Result):
% 	Result is the maximum (via compare/3) Term produced by Goal.
%	Terms may contain local variables.
% $count(Globals, Term, Goal, Result):
% 	Result is the number of distinct Terms produced by Goal.
%	Terms must be ground (otherwise result may be infinite).
% $sum(Globals, Summand, Term, Goal, Result):
% 	Result is the sum of Summands, for each distinct Term produced by Goal.
% 	Summand may be any arithmetically evaluable term.
%	Terms must be ground (otherwise result may be infinite).
% $insert(.....Philip can fill this in

?- putprop($aggregates, level, 0).

%$min(V, Term, Goal, Min) :-		% transformed straight to $limit
%	$limit(V, (<), Term, Goal, Min).

%$max(V, Term, Goal, Max) :-		% transformed straight to $limit
%	$limit(V, (>), Term, Goal, Max).

?- $limit(V, _, _, _, _) when ground(V).
$limit(_V, O, Term, Goal, MinMax) :-
	once getprop($aggregates, level, N),
	N1 is N + 1,
	putprop($aggregates, level, N1),
	(	call(Goal),		% BUG!  Need to check for floundering!
						% Note: May be local variables in terms
		( getprop($aggregates, N1, MinMax1) ->
			( compare(O, Term, MinMax1) ->
				putprop($aggregates, N1, Term)
			)
		;	putprop($aggregates, N1, Term)
		),
		fail
	;	true
	),
	putprop($aggregates, level, N),
	once getprop($aggregates, N1, Temp),
	remprop($aggregates, N1, Temp),
	MinMax = Temp.				% There is a reason for this!

$count(V, Term, Goal, Count) :-
	$sum(V, 1, Term, Goal, Count).

?- $sum(V, _, _, _, _) when ground(V).
$sum(_V, S, Term, Goal, Sum) :-
	once getprop($aggregates, level, N),
	N1 is N + 1,
	putprop($aggregates, level, N1),
	putprop($aggregates, N1 - sum, 0),
	$call_sum(N, N1, S, Term, Goal),
	putprop($aggregates, level, N),
	remprop($aggregates, N1),
	once getprop($aggregates, N1 - sum, Temp),
	remprop($aggregates, N1 - sum, Temp),
	Sum = Temp.				% There is a reason for this!

$call_sum(N, N1, S, Term, Goal) :-
	call(Goal),				% BUG!  Need to check for floundering!
	( ground(S.Term) ->		% Var(s) in term means (possibly) infinite sum
		\+ getprop($aggregates, N1, Term),
		addprop($aggregates, N1, Term),
		once getprop($aggregates, N1 - sum, Subtotal),
		Sum is Subtotal + S,	% S may be evaluable term
		putprop($aggregates, N1 - sum, Sum),
		fail
	;	putl("Error: Goal produced unground term in sum aggregate.\n")
	),
	putprop($aggregates, level, N),
	remprop($aggregates, N1),
	remprop($aggregates, N1 - sum),
	!,
	fail.
$call_sum(_N, _N1, _S, _Term, _G).

$insert(V, B, I, G) :-
 	$update(V, B, I, [], G).
 
$delete(V, B, D, G) :-
 	$update(V, B, [], D, G).

% $update/5 is a database update predicate that takes lists of insertions
% and list of deletions and applies them to all solutions to a goal.

?- putprop($findall, $level, 0).

?- $update(V, _, _, _, _) when ground(V).
$update(_V, B, I, D, G) :-
	once getprop($findall, $level, N),
	N1 is N + 1,
	putprop($findall, $level, N1),
	once
	(	call(G),
		( ground(B) ->
			addprop($findall, N, B),
			fail
		;	format("~NError: Unground update -- ~w.~n", [(B, I, D)])
		),
		remprop($findall, N),
		putprop($findall, $level, N),
		!,
		fail
	;	getprop($findall, N, B),
		once mapList(retract, D),
		once mapList(assert, I),
		fail
	;	true
	),
	remprop($findall, N),
	putprop($findall, $level, N).
