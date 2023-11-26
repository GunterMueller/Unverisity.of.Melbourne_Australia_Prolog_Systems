/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Lee Naish
 */

% Transformation of high level negation "primitives" to low level ones.
% Print warning messages for nonlocal quantified vars.
%
% In the following, L,L1 denote the terms of local (quantified) vars.
% If there are no quantifiers, L is [].  G denotes the global vars
% (all vars except locals).
% This is now out of date - everything is transformed into low level
% code like -> for the compiler and code similar to that below for
% the interpreter (call/1).  It gives the general idea though.

%	High Level	transformed to	Low Level
%
% all L T1 ~= T2		=====>	$is_eq(L, T1, T2, fail)
% all L not C			=====>	$if(G, C, fail, true)
% not some L C			=====>	$if(G, C, fail, true)
% if some L C then T else E	=====>	$if(G, C, T, E)
% 					or $if_soft(G, C, T, E) (L occurs in T)
% if some L C then T		=====>	$if(G, C, T, true)
% 					or $if_soft(G, C, T, true) (L in T)
% some L C			=====>	$some(G, C)
% solutions(L1, some L C, S)	=====>	$solutions(G, L1, C, S)

% The same transformations are done for gSome and gAll.  These
% allow global variables to be quantified (all and some give errors).

% some, all, etc, should be prefix binary operators but the code
% does not assume that, so it can be run more easily.

%	transform clause and return auxillary pred defns as well
%	should compile with impneg_co.nl
$neg_trf(T1, T2, Defns) :-
	$save_proc_name(T1),
	$trf(T1, T2, Defns).

%	do similar transformations for interpreter/call
%	default is to have no implicit quantification
%	should compile with impneg_in.nl
$call_trf(C1, C2) :-
	$call_trf(C1, [], C2).

%	At the main loop of the user interface, unique vars in user
%	queries are considered local - these are put in 'U'
$call_trf(C1, U, C2) :-
	$op_trf(C1, C, []), % operational extended syntax trf
	$trf(C, C2, [], U, Defns),
	( Defns \== [] ->
		format(user_error,
			"~NError in call/1 of ~w -- negation broken.~n",
			[C1])
	).

%	transform term and return auxillary pred defns
$trf(T1, T2, Defns) :-
	$xs_trf(T1, T),
	$unique_vars(T1, U),	% unique vars in source code are local
	$trf(T, T2, [], U, Defns).

%	transform high level term to low level
%	V is a term containing all global vars
%	U is the list of (unique) vars which should be considered local
$trf(T0, T, _V, _U, Defns) :-		% BUG?  should be call(T)?
	var(T0),
	!,
	T0 = T,
	Defns = [].
$trf(T, T1, V, U, Defns) :-
	$meta_call(T, Args, Args1, Argsx, T1),
	!,
	$$trf_list(Args, Args1, Argsx.V, U, Defns).
$trf(T, T1, V, U, Defns) :-
	$trf1(T, T1, V, U, Defns).

$trf1(all(L, C), C2, V, U, Defns) :-
	!,
	$str_all(all(L, C), LV, GV, C1),
	$trf_all(C1, C2, V, LV, GV, U, Defns).
$trf1(gAll(L, C), C2, V, U, Defns) :-
	!,
	$str_all(gAll(L, C), LV, GV, C1),
	$trf_all(C1, C2, V, LV, GV, U, Defns).
$trf1(not(Var), not(Var), _V, _U, []) :-
	var(Var),
	% writeln('% Error - malformed "not"'),
	!.
$trf1(not(C), CNew, V, U, Defns) :-
	!,
	$str_some(C, LV, GV, C1),
	$trf(C1, C2, V, U, CDefns),
	$extra_vars(C1, f(GV, LV, U), G),
	append(GV, G, NA),
	$imp_not(G, NA, C2, CNew, Defn),
	append(Defn, CDefns, Defns).
$trf1((T1 ~= T2), C, _V, U, Defns) :-
	!,
	$v_intersect(U, T1 ~= T2, G),		% not needed for compiler
	$imp_is_eq(G, [], T1, T2, fail, C, Defns).
$trf1(freeze(Var, C), T, V, U, Defns) :-
	!,
	$trf(C, C1, V, U, CDefns),
	$extra_vars(C1, f(Var, U), NA),
	$imp_freeze(Var, Var.NA, C1, T, Defn),
	append(Defn, CDefns, Defns).
$trf1((if I), T, V, U, Defns) :-
	!,
	$trf_if((if I), T, V, U, Defns).
$trf1(else(I, J), T, V, U, Defns) :-
	!,
	$trf_if(else(I, J), T, V, U, Defns).
%$trf1(solutions(T, C, S), $solutions(G, T, C2, S), V, U, Defns) :-
%	!,
%	$str_some(C, LV, GV, C1),
%	$extra_vars(C, f(T, LV, GV, U), G),	% should do other code like this
%	$trf(C1, C2, V, U, Defns).
$trf1(solutions(T, C, S), CNew, V, U, Defns) :-
	!,
	$str_some(C, LV, GV, C1),
	$extra_vars(C, f(T, LV, GV, U), AG),% BUG!  This should probably be C1.
	%	Note that G contains only variables that are in V.
	$v_intersect(AG, V, G),
	$extra_vars(T, G.GV, RV),
	append(G, RV, GRV),
	append(GV, GRV, NA),				% NA are the variables global to C2.
	$trf(C1, C2, RV.V, U, CDefns),
	$imp_solutions(AG, NA, T, C2, S, CNew, Defn),
	append(Defn, CDefns, Defns).
%$trf1(max(T, C1, S), $limit(G, (>), T, C2, S), V, U, Defns) :-
%	!,
%	$extra_vars(C1, U.T, G),
%	$trf(C1, C2, V, U, Defns).
%$trf1(min(T, C1, S), $limit(G, (<), T, C2, S), V, U, Defns) :-
%	!,
%	$extra_vars(C1, U.T, G),
%	$trf(C1, C2, V, U, Defns).
%$trf1(sum(E, T, C1, S), $sum(G, E, T-E, C2, S), V, U, Defns) :-
%	!,
%	$extra_vars(C1, U.E.T, G),	% should vars in T,E be local? y
%	$trf(C1, C2, V, U, Defns).
%$trf1(count(T, C1, S), $sum(G, 1, T, C2, S), V, U, Defns) :-
%	!,
%	$extra_vars(C1, U.T, G),		% should vars in T be local? y
%	$trf(C1, C2, V, U, Defns).
$trf1(max(T, C, S), CNew, V, U, Defns) :-
	!,
	$extra_vars(C, U.T, AG),
	%	Note that G contains only variables that are in V.
	$v_intersect(AG, V, G),
	$extra_vars(T, G, RV),
	append(G, RV, NA),
	$trf(C, C1, RV.V, U, CDefns),
	$imp_limit(AG, NA, (>), T, C1, S, CNew, Defn),
	append(Defn, CDefns, Defns).
$trf1(min(T, C, S), CNew, V, U, Defns) :-
	!,
	$extra_vars(C, U.T, AG),
	%	Note that G contains only variables that are in V.
	$v_intersect(AG, V, G),
	$extra_vars(T, G, RV),
	append(G, RV, NA),
	$trf(C, C1, RV.V, U, CDefns),
	$imp_limit(AG, NA, (<), T, C1, S, CNew, Defn),
	append(Defn, CDefns, Defns).
$trf1(sum(E, T, C, S), CNew, V, U, Defns) :-
	!,
	$extra_vars(C, f(U, E, T), AG),	% should vars in T,E be local? y
	%	Note that G contains only variables that are in V.
	$v_intersect(AG, V, G),
	$extra_vars(T.E, G, RV),
	append(G, RV, NA),
	$trf(C, C1, RV.V, U, CDefns),
	$imp_sum(AG, NA, E, T-E, C1, S, CNew, Defn),
	append(Defn, CDefns, Defns).
$trf1(count(T, C, S), CNew, V, U, Defns) :-
	!,
	$extra_vars(C, U.T, AG),		% should vars in T be local? y
	%	Note that G contains only variables that are in V.
	$v_intersect(AG, V, G),
	$extra_vars(T, G, RV),
	append(G, RV, NA),
	$trf(C, C1, RV.V, U, CDefns),
	$imp_sum(AG, NA, 1, T, C1, S, CNew, Defn),
	append(Defn, CDefns, Defns).
$trf1(some(L, C), C3, V, U, Defns) :-
	!,
	$str_some(some(L, C), LV, GV, C1),
	$extra_vars(C, f(GV, LV, U), G),
	$trf(C1, C2, V, U, Defns),
	(G == [] ->
		C3 = once(C2)
	;	$squash_list(G, SG),
		C3 = (ground(SG) -> once(C2) ; C2)
	).
			% mustn't have cut in C2
$trf1(gSome(L, C), C3, V, U, Defns) :-
	!,
	$str_some(gSome(L, C), LV, GV, C1),
	$extra_vars(C, f(GV, LV, U), G),
	$trf(C1, C2, V, U, Defns),
	( G == [] ->
		C3 = once(C2)
	;	$squash_list(G, SG),
		C3 = (ground(SG) -> once(C2) ; C2)
	).
$trf1(T, T, _V, _U, []).

%	$trf list of args
%	V contains the initial global vars plus all the
%	preceeding arguments.  $trf is called with this and
%	L, all the succeeding arguments.
?- $$trf_list(A, _, _, _, _) when A.
$$trf_list([], [], _, _U, []).
$$trf_list(A0.L, A.L1, V, U, Defns) :-
	$trf(A0, A, L.V, U, Defns1),
	$$trf_list(L, L1, A0.V, U, Defns2),
	append(Defns1, Defns2, Defns).

%	$trf - special case for all(L, C) and gAll(L, C)
%	should clean up dubious constructs
$trf_all(Var, C, _V, _LV, _GV, _U, Defns) :-
	var(Var),
	!,
	putl(user_error, "% Error - malformed 'all'\n"),
	C = all(_, Var),
	Defns = [].
$trf_all((T1 ~= T2), C, _V, LV, GV, U, Defns) :-
	!,
	Defns = [],
	$v_intersect(U, T1 ~= T2, UV),		% not needed in compiler
	append(UV, LV, A),
	$imp_is_eq(A, GV, T1, T2, fail, C, _Defns).
$trf_all((not C), CNew, V, LV, GV, U, Defns) :-
	!,
	$trf(C, C1, V, U, CDefns),
	$extra_vars(C, f(GV, LV, U), G),
	append(GV, G, NA),
	$imp_not(G, NA, C1, CNew, Defn),
	append(Defn, CDefns, Defns).

%	$trf - special case for if statements
%	should clean up dubious constructs
$trf_if((if Var), C, _V, _U, Defns) :-
	var(Var),
	!,
	putl(user_error, "% Error - malformed 'if'\n"),
	C = (if Var),
	Defns = [].
$trf_if((Var else E), C, _V, _U, Defns) :-
	var(Var),
	!,
	putl(user_error, "% Error - malformed 'if'\n"),
	C = (Var else E),
	Defns = [].
$trf_if((if Var else E), C, _V, _U, Defns) :-
	var(Var),
	!,
	putl(user_error, "% Error - malformed 'if'\n"),
	C = (if Var else E),
	Defns = [].
$trf_if((if Var then T), (if Var then T), _V, _U, []) :-
	$str_some(Var, _, _, VV),
	var(VV),
	!,
	putl(user_error, "% Error - malformed 'if'\n").
$trf_if((if Var then T else E), (if Var then T else E), _V, _U, []) :-
	$str_some(Var, _, _, VV),
	var(VV),
	!,
	putl(user_error, "% Error - malformed 'if'\n").
$trf_if((if TL = TR then T else E), CNew, V, U, Defns) :-
	( var(TL), const(TR) ->
		$trf(freeze(TL, (TL == TR -> T ; E)), CNew, V, U, Defns)
	;	var(TR),
		const(TL),
		$trf(freeze(TR, (TL == TR -> T ; E)), CNew, V, U, Defns)
	),
	!.
$trf_if((if C then T else E), CNew, V, U, Defns) :-
	!,
	$str_some(C, LV, GV, C1),
	( C1 = (TL=TR) ->
		$v_intersect(U, C1, UV),	% not needed in compiler
		append(UV, LV, A),
		$imp_is_eq(A, GV, TL, TR, R, ISEQ, _),
		CNew = (ISEQ, IF),
		% = is added to then in case there are shared quantified vars
		$trf_if((if R==true then (TL=TR, T) else E), IF, V, U, Defns)
	;	$trf(C1, C2, V.T.E, U, CDefns),	% make into new proc
		$trf(T, T1, V.LV.GV.C1.E, U, TDefns),
		$trf(E, E1, V.C.T, U, EDefns),
		append(CDefns, TDefns, CTDefns),
		append(CTDefns, EDefns, CTEDefns),
		$extra_vars(C, f(GV, LV, U), G),
		$shared_vars((LV, GV, C1, T, E), V, SV),
		( $is_local(LV, T) ->
			$imp_if(G, SV, C2, T1, E1, CNew, Defn)
		; 	$imp_if_soft(G, SV, C2, T1, E1, CNew, Defn)
		),
		append(Defn, CTEDefns, Defns)
	).
$trf_if((if C then T), CNew, V, U, Defns) :-
	!,
	$trf_if((if C then T else true), CNew, V, U, Defns).
$trf_if(I, _C, _V, _U, _Defns) :-
	format(user_error, "~NError - ~w is malformed.~n", [I]),
	fail.

%	list of preds with preds as args (which need to be transformed)
%	Should be a user hook to add to this
%	Need to be careful with multiple levels of functor nesting -
%	it may construct a clause with meta vars.
%?- $meta_call(A, _, _, _, _) when A.				% Really index
$meta_call((A, B), [A, B], [A1, B1], [], (A1, B1)).
$meta_call((A :- B), [B], [B1], A, (A :- B1)).
$meta_call((A ; B), [A, B], [A1, B1], [], (A1; B1)).
$meta_call(:-(A), [A], [A1], [], :-(A1)).
$meta_call(?-(A), [A], [A1], [], ?-(A1)).
% $meta_call(whenGround(A, B), [B], [B1], [A], whenGround(A, B1)).
% $meta_call(freeze(A, B), [B], [B1], [A], freeze(A, B1)).
$meta_call(call(A), [A], [A1], [], call(A1)).
		% standard negation - leave it out to encourage NU-Neg?
$meta_call(\+(A), [A], [A1], [], \+(A1)).
$meta_call(once(A), [A], [A1], [], once(A1)).
$meta_call((A -> B), [A, B], [A1, B1], [], (A1 -> B1)).
%$meta_call((A -> B ; C),... handled by -> and ; separately

%	strip some and gSome from goal, returning inner goal,
%	"some" vars and "gSome" vars
%	(used to be written nicely with 'meta' primitives)
$str_some(M, L, GL, M1) :-
	var(M),
	!,
	L = [],
	GL = [],
	M = M1.
$str_some(some(V, G), L1, GL, G1) :-
	!,
	$str_some(G, L, GL, G1),
	$extra_vars(V, [], VL),
	append(VL, L, L1).
$str_some(gSome(V, G), L, GL1, G1) :-
	!,
	$str_some(G, L, GL, G1),
	$extra_vars(V, [], VL),
	append(VL, GL, GL1).
$str_some(M, [], [], M).

%	strip all and gAll from goal, returning inner goal,
%	"all" vars and "gAll" vars
%	(as above)
$str_all(M, L, GL, M1) :-
	var(M),
	!,
	L = [],
	GL = [],
	M = M1.
$str_all(all(V, G), L1, GL, G1) :-
	!,
	$str_all(G, L, GL, G1),
	$extra_vars(V, [], VL),
	append(VL, L, L1).
$str_all(gAll(V, G), L, GL1, G1) :-
	!,
	$str_all(G, L, GL, G1),
	$extra_vars(V, [], VL),
	append(VL, GL, GL1).
$str_all(M, [], [], M).

%	L is a list of vars, G is the list of vars in T but not
%	in L.
%	"local" vars in T should not be counted. These are vars in
%	the first arg of all, some and solutions (+gAll, gSome, $is_eq?)
%
%	BUG!  This treats X in q(some(X, p)) as local.
$extra_vars(T, L, G) :-
	$extra_vars1a(T, L, [], G).

$extra_vars1a(V, L, D0, D) :-
	var(V),
	( occurs(V, L.D0) ->
		D = D0
	;	D = V.D0
	).
$extra_vars1a(V, L, D0, D) :-
	const(V),
	D = D0.
$extra_vars1a(V, L, D0, D) :-
	term(V),
	$extra_vars2(V, L, D0, D).

$extra_vars2(all(V, C), L, D, D1) :-
	!,
	$extra_vars1a(C, V.L, D, D1).
$extra_vars2(some(V, C), L, D, D1) :-
	!,
	$extra_vars1a(C, V.L, D, D1).
$extra_vars2(solutions(T, C, S), L, D, D1) :-
	!,
	$ev1a_2(C, S, T.L, D, D1).
$extra_vars2(min(T, C, S), L, D, D1) :-
	!,
	$ev1a_2(C, S, T.L, D, D1).
$extra_vars2(max(T, C, S), L, D, D1) :-
	!,
	$ev1a_2(C, S, T.L, D, D1).
$extra_vars2(count(T, C, S), L, D, D1) :-
	!,
	$ev1a_2(C, S, T.L, D, D1).
$extra_vars2(sum(E, T, C, S), L, D, D1) :-
	!,
	$ev1a_2(C, S, E.T.L, D, D1).
$extra_vars2(C, L, D, D1) :-
	term(C),
	C =.. _F.A,		% complex term - (mutually) recurse
	$ev1a_list(A, L, D, D1).

$ev1a_2(A1, A2, L, D0, D) :-
	$extra_vars1a(A2, L, D0, D1),
	$extra_vars1a(A1, L, D1, D).

?- $ev1a_list(A, _, _, _) when A.
$ev1a_list([], _L, D, D).
$ev1a_list(H.T, L, D0, D) :-
	$ev1a_list(T, L, D0, D1),
	$extra_vars1a(H, L, D1, D).

%	list of vars are not in global term
?- $is_local(A, _) when A.
$is_local([], _).
$is_local(V.T, G) :-
	\+ occurs(V, G),
	$is_local(T, G).

%	return list of non-local vars shared by two terms
$shared_vars(A, B, V) :-
	$extra_vars(A, [], AV),
	$extra_vars(B, [], BV),
	$v_intersect(AV, BV, V).

%	set intersection for vars (first arg must be a list
%	second arg may be any term)
?- $v_intersect(A, _, _) when A.
$v_intersect([], _BV, []).
$v_intersect(A.AV, BV, V0) :-
	( occurs(A, BV) ->
		V0 = A.V
	;	V0 = V
	),
	$v_intersect(AV, BV, V).
%$v_intersect(_A.AV, BV, V) :-
%	$v_intersect(AV, BV, V).

%	L is the list of unique vars in T
$unique_vars(T, L) :-
	$unique_vars1(T, [], [], L, _).

%	T is the term, R is the list of repeated vars so far
%	L is the list of unique vars so far, L1 and R1 are returned
$unique_vars1(T, R, L, L1, R1) :-
	( const(T) ->
		L = L1,
		R = R1
	; var(T) ->
		( occurs(T, L) ->
			R1 = T.R,
			$v_delete(T, L, L1)
		; occurs(T, R) ->
			L1 = L,
			R1 = R
		;	L1 = T.L,
			R1 = R
		)
	;	functor(T, _F, N),
		$unique_vars_a(N, T, R, L, L1, R1)
	).

$unique_vars_a(N, T, R, L, L1, R1) :-
	( N > 0 ->
		arg(N, T, A),
		N1 is N - 1,
		$unique_vars1(A, R, L, L2, R2),
		$unique_vars_a(N1, T, R2, L2, L1, R1)
	;	L = L1,
		R = R1
	).

%	delete for lists of vars (uses == instead of =)
%	only delete first occurrence then fails on backtracking
$v_delete(A, A1.B, C0) :-
	( A == A1 ->
		B = C0
	;	C0 = A1.C,
		$v_delete(A, B, C)
	).
%$v_delete(A, A1.B, A1.C) :-
%	$v_delete(A, B, C).

%	construct header code for gAll vars in ~=
?- $is_eq_header(A, _, _) when A.
$is_eq_header([], T, T).
$is_eq_header(V.VL, T, ('$copyVariablesToTopOfHeap:-)'(V), R)) :-
	$is_eq_header(VL, T, R).

%	convert a non-empty list of variables into a term
%	with the same variables but fewer functors
$squash_list([A], A) :-
	!.
$squash_list(A, F) :-
	F =.. f.A.

%	if we are reading a new procedure, save its name and
%	initialise the string for generating new proc names
%	eg, if the current procedure is proc/3, the next names
%	generated will be '.aproc3', '.bproc3' etc.
$save_proc_name((H:-_B)) :-
	!,
	$save_proc_name1(H).
$save_proc_name(H) :-
	$save_proc_name1(H).

$save_proc_name1(H) :-
	$curr_proc_name(H),
	!.
$save_proc_name1(HH) :-
	( integer(HH) ->
		H = xyzzy(_)
	;	H = HH
	),
	retract($curr_proc_name(_)),
	functor(H, F, N),
	functor(H1, F, N),
	assert($curr_proc_name(H1)),
	name(F, FS),
	intToString(N, NS),
	append(FS, NS, S),
	retract($next_name(_)),
	assert($next_name(0'..0'a.S)),
	!.

?- useIf nuprolog.
?- initializing, $dynamic($curr_proc_name/1).
?- initializing, assert($curr_proc_name(xyzzy(_))).
?- useElse.
?- assert($curr_proc_name(xyzzy(_))).
?- useEnd.

%	create a new pred with arguments A and which waits until G is ground
$new_freeze_pred(Var, A, Body, Head, defn([Var], Head, Body)) :-
	$new_name(N),
	$extra_vars(A, Var, A1),
	Head =.. N.Var.A1.

%	create a new pred with arguments A and which waits until G is ground
$new_pred(G, A, Body, Head, defn(Whens, Head, Body)) :-
	$new_name(N),
	$groundWhenVars(G, Whens),
	( G == [] ->					% for $solutions, $limit, $sum
		Head =.. N.A
	;	$extra_vars(A, G, A1),
		append(G, A1, Args),
		Head =.. N.Args
	).

?- $groundWhenVars(G, Whens) when G or Whens.
$groundWhenVars([], []).
$groundWhenVars(G.GT, ground(G).WT) :-
	$groundWhenVars(GT, WT).

%	generate a new pred name
$new_name(Name) :-
	retract($next_name(N)),
	name(Name, N),
	N = Dot.Char.Rest,
	( Char < 0'z ->
		Char1 is Char + 1,
		N1 = Dot.Char1.Rest
	;	N1 = Dot.0'a.Char.Rest
	),
	assert($next_name(N1)),
	!.

?- useIf nuprolog.
?- initializing, $dynamic($next_name/1).
?- initializing, assert($next_name(".x")).		% next name to generate
?- useElse.
?- assert($next_name(".x")).
?- useEnd.
