/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Lee Naish
 */

	% Implementation of negation constructs for call.
	% Constructs are transformed into the low level primitives
	% below once, then simply called.

	% Lee Naish July 1986

?- useIf nuprolog.
	% V is the global variables of Goal, not including "gSome" vars.
	% (likewise for the other preds, except $call_is_eq)
	% ($some is not actually needed at the moment)
%$some(V, Goal) :-
%	( ground(V) ->
%		once(call(Goal))
%	;
%		call(Goal)
%	).

?- $if(V, _, _, _) when ground(V).
$if(_V, C, A, B) :-
	( call(C) ->
		call(A)
	;
		call(B)
	).

?- $if_soft(V, _, _, _) when ground(V).
$if_soft(_V, C, A, B) :-
	true,
	(	$label(L),
		call(C),
		$softCut(L),
		call(A)
	;		% <- $softCut kills just this choice point
		call(B)
	).

	% V contains "all" and "gAll" vars - I think this implementation
	% is correct.
$call_is_eq(V, T1, T2, R) :-
	true,
	(	'$copyVariablesToTopOfHeap:-)'(V),
		$is_eq(T1, T2, R)
	;
		R = true
	).

?- useEnd.

?- useIf muprolog.
	% V contains "all" and "gAll" vars - I think this
	% is not implemented properly
$call_is_eq(V, T1, T2, R) :-
	$is_eq(V, T1, T2, R).

?- useEnd.

% $solutions(V, T, G, S) is defined elsewhere

	% Now everything that needs to be transformed...

?- useIf nuprolog.
?- all(V, G) when G.	% should be made a bit stronger
?-useEnd.
all(V, G) :-
	$call_trf(all(V,G), G1),
	call(G1).

?- useIf nuprolog.
?- gAll(V, G) when G.	% should be made a bit stronger
?-useEnd.
gAll(V, G) :-
	$call_trf(gAll(V,G), G1),
	call(G1).

?- useIf nuprolog.
?- some(V, G) when G.	% should be made a bit stronger
?-useEnd.
some(V, G) :-
	$call_trf(some(V,G), G1),
	call(G1).

?- useIf nuprolog.
?- gSome(V, G) when G.	% should be made a bit stronger
?-useEnd.
gSome(V, G) :-
	$call_trf(gSome(V,G), G1),
	call(G1).

=>(F, G) :-
	$call_trf(=>(F,G), G1),
	call(G1).

<=(F, G) :-
	$call_trf(<=(F,G), G1),
	call(G1).

<=>(F, G) :-
	$call_trf(<=>(F,G), G1),
	call(G1).

solutions(T, G, S) :-
	$call_trf(solutions(T, G, S), G1),
	call(G1).

min(T, G, S) :-
	$call_trf(min(T, G, S), G1),
	call(G1).

max(T, G, S) :-
	$call_trf(max(T, G, S), G1),
	call(G1).

count(T, G, S) :-
	$call_trf(count(T, G, S), G1),
	call(G1).

sum(E, T, G, S) :-
	$call_trf(sum(E, T, G, S), G1),
	call(G1).

?- useIf nuprolog.
?- not(G) when G.			% Give $call_trf something to work on
not(G) :-
	$call_trf(not(G), G1),
	call(G1).

?- if(A then B) when A and B.			% BUG!  Too weak.  Consider if some ...
if(G) :-
	$call_trf(if(G), G1),
	call(G1).

?- else(if(A then B), C) when A and B and C.	% BUG!  Too weak.
%?- else(A, B) when A and B.
else(A, B) :-
	$call_trf(else(A, B), G1),
	call(G1).
?-useEnd.

?- useIf muprolog.	% changing not and if tends to break things
not1(G) :-
	$call_trf(not(G), G1),
	call(G1).

if1(G) :-
	$call_trf(if(G), G1),
	call(G1).

?-useEnd.
