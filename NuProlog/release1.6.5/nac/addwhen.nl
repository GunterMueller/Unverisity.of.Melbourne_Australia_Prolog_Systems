/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

%	LOGIC PREPROCESSOR
%
%	code for adding wait declarations
%		now hacked (almost to death) for when declarations
%	and (some) code for checking for tests
%		and reordering clause bodies
%
%	Written by Lee Naish


% add whens and calculate cost for a proc
addwhen(P) :-
	bagof((P:-B), clause(P, B), Clauses),
	genw(Clauses, W),
	wadd(W),
	check_det(P, Clauses, W).

% generate whens for a proc
genw(Clauses, Whens) :-
	possloops(Clauses, Plp),
	%compress(Plp, Loops),		% why was this deleted???
					% should use sort anyway
	Loops = Plp,
	wForCalls(Loops, Whens1),
	extraW(Clauses, Whens1, Whens).

% find possible looping heads and calls from list of clauses
possloops([], []).
possloops((H:-B).L, Lps) :-
	duplicate(H, H1),
	cloops(H1, B, Lps1),
	possloops(L, Lps2),
	append(Lps1, Lps2, Lps).

% find possible loops in a single clause
cloops(H,C,[]) :-
	var(C),
	!,
	functor(H,F,N),
	format("% Meta-variable ignored in procedure ~a/~d.~n", [F, N]).
cloops(H, (C1, C2), R) :-
	!,
	cloops(H, C1, R1),
	cloops(H, C2, R2),
	append(R1, R2, R).
cloops(H, C, R) :-
	meta_call(C, C1),
	!,
	cloops(H, C1, R).
cloops(H, C, [H-C]) :-
	recursive(H, C),
	!.
cloops(H, C, []).

	% get list of calls from meta call/construct
?- meta_call(A, _) when A.
%meta_call((A,B), [A,B]).	% not needed here
meta_call((A;B), [A,B]).
%meta_call((A->B), [A,B]).	% shouldn't be in pure code!
meta_call((A=>B), [A,B]).
meta_call((A<=B), [A,B]).
meta_call((A<=>B), [A,B]).
meta_call(some(A,B), [B]).
meta_call(all(A,B), [B]).
%meta_call(gSome(A,B), [B]).
%meta_call(gAll(A,B), [B]).
meta_call(solutions(A,B,C), [B]).
meta_call(min(A,B,C), [B]).
meta_call(max(A,B,C), [B]).
meta_call(count(A,B,C), [B]).
meta_call(sum(A,B,C,D), [C]).
meta_call(not(A), [A]).
%meta_call(\+(A), [A]).
%meta_call(once(A), [A]).
meta_call(if(A), []) :-
	var(A),
	!.
meta_call((if A else B), []) :-
	var(A),
	!.
meta_call((if A then B else C), [A,B,C]) :-
	!.
meta_call((if A then B), [A,B]).

% is C a recursive call? - will handle mutual recursion some time
recursive(H, C) :-
	\+ H \= C.

% add list of when declarations
wadd([]).
wadd(A.B) :-
	asserta(whens(A)),
	wadd(B).

% calculate cost of a proc from its whens and clauses
check_det(P, Clauses, Waits) :-
	functor(P,F,N),
	( no_cons(Waits) ->
		format("% procedure ~a/~d doesn't construct some arg(s).~n", [F, N]),
		asserta(nocons(P))
	),
	(is_local_det(Waits, Clauses) ->	% meta vars
		format("% procedure ~a/~d is locally deterministic.~n", [F, N])
	;	\+ non_det(P),	% not marked as non-det already
		ass_non_det(P)
	).

% given a (probably) non-deterministic proc, assert that it and its callers
% are non-deterministic
ass_non_det(P) :-
	assertz(non_det(P)),
	calls(Q, P),
	\+ non_det(Q),
	ass_non_det(Q),
	fail.
ass_non_det(_).

% finds current cost of proc
hascost(P, C) :-
	cost(P, C),
	!.

% costs are 'asserta'ed before this
?- dynamic cost/2.
cost(P, 50) :-
		system_pred(P).
cost(_, 1000).

% (probably) nondeterministic procs are asserted here
?- dynamic non_det/1.
non_det(repeat).	% hardly likely in pure code!
non_det(member(_, _)).
non_det(delete(_, _, _)).
non_det(perm(_, _)).
non_det(append(_, _, _)).	% most uses are actually deterministic
non_det(currentPredicate(_, _)).
non_det(systemPredicate(_, _)).
	% should be more here - check manual

% procs with possible loops which cant be stopped by whens asserted here
?- dynamic poss_loop/1.
poss_loop(repeat).

% procs which dont construct some arg (set) are asserted here
% perhaps we should have all system tests here
?- dynamic nocons/1.
nocons(_ < _).

	% used for copying clauses when reordering
?- dynamic tmp/1.

	% generate list of whens for list of heads and recursive calls
wForCalls(LL, SimpleWL) :-
	wSepListForCalls(LL, WSL),
	combineWList(WSL, WL),
	simplifyWList(WL, SimpleWL).

	% generate list of separate whens for a list of head-calls
wSepListForCalls([], []).
wSepListForCalls((H-C).LL, W.WL) :-
	wFor1Call(H, C, W),
	wSepListForCalls(LL, WL).

	% generate when declaration for a single recursive call
wFor1Call(Head, Call, (WHead when WBody)) :-
	duplicate(Call, Call1),
	Call1 =.. F.CA,
	Head =.. F.HA,
	wFor1Call1(HA, CA, WHA, WBody),
	WHead =..F.WHA,
	( WBody == false ->
		(	write('% possible infinite loop: '),
			numberVars(Head.Call, 0, _),
			writev([quote, cons], Head),
			write(' :- ... '),
			writev([quote, cons], Call),
			nl,
			fail
		;	true
		),
		functor(Head, F, N),
		functor(P, F, N),
		asserta(poss_loop(P))
	).

	% generate when declaration for a single recursive call
wFor1Call1([], [], [], false).
wFor1Call1(H.HL, C.CL, _.WHL, B) :-
	atomic(C),
	!,
	wFor1Call1(HL, CL, WHL, B).
wFor1Call1(H.HL, C.CL, _.WHL, B) :-
	var(C),
	var(H),
	!,
	wFor1Call1(HL, CL, WHL, B).
wFor1Call1(H.HL, C1.CL, C.WHL, B) :-
	var(C1),
	% nonvar(H),
	!,
	wFor1Call1(HL, CL, WHL, B1),
	( B1 == false ->
		% B = nonvar(C)
		B = C
	;
		% B = (nonvar(C) or B1)
		B = (C or B1)
	).
wFor1Call1(H.HL, C.CL, WH.WHL, B) :-
	C =.. F.CA,
	H =.. F.HA,
	wFor1Call1(HA, CA, WHA, B1),
	wFor1Call1(HL, CL, WHL, B2),
	( B1 == false ->
		B = B2
	;
		WH =.. F.WHA,
		( B2 == false ->
			B = B1
		;
			B = (B1 or B2)
		)
	).

	% combine a list of separate when declarations
	% rather more messy than it should be because when
	% declarations are not fully implemented
	%	- heads must be mutually nonunifiable
combineWList(WSL, WL) :-
	combineWList1(WSL, [], WL1),
	( WL1 = [_ when F], F == false ->
		WL = []
	;
		WL = WL1
	).

combineWList1([], SoFar, SoFar).
combineWList1(W.WSL, SoFar, WL) :-
	addWToList(W, SoFar, SoFar1),
	combineWList1(WSL, SoFar1, WL).

	% add single when to when list
addWToList(W, [], [W]).
addWToList((H when B), (H1 when B1).WL, (H1 when B1).WL1) :-
	H \= H1,		% meta vars -> nonlogical ~=
	!,
	addWToList((H when B), WL, WL1).
addWToList((H when B), (H1 when B1).WL, (H1 when B2).WL) :-
	variant(H, H1),
	!,
	H = H1,
	( (B == ever ; B == B1) ->
		B2 = B1
	;
		B2 = (B1 and B)
	).
	% add more clauses here dealing with cases where one head subsumes
	% the other ???
addWToList((H when B), (H1 when B1).WL, (H1 when B1).WL) :-
	!,
	(	write('% Warning: when declarations for '),
		numberVars(H, 0, _),
		writev([quote, cons], H),
		writeln(' may be too strict'),
		fail
	;
		true
	).

	% add extra whens so all clauses match at least one when
	% if there are no whens already, dont bother adding any
extraW(_, [], []).
extraW(CL, W.WL, WL1) :-
	extraW1(CL, W.WL, WL1).

	% add extra whens so all clauses match at least one when
extraW1([], WL, WL).
extraW1((H:-B).CL, WL, WL1) :-
	( \+ member((H when _), WL) ->	% mustnt bind vars in WL
		addWToList((H when ever), WL, WL2)
	;
		WL2 = WL
	),
	extraW1(CL, WL2, WL1).

	% simplify bodies of when declarations
	% ### improve this - especiall distribute or over and
	% also reorder (have code for this)
	% alternatively, put some of the code in the compiler
simplifyWList(WL, WL).

	% two terms are variants of each other
variant(A, B) :-
	\+ \+ (numberVars(A, 0, N), numberVars(B, 0, N), A=B).
