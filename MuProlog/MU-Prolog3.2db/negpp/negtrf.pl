%	$Header: negtrf.pl,v 1.1 85/11/25 19:37:17 lee Exp $

% Transformation of high level negation "primitives" to low level ones.
% Print warning messages for nonlocal quantified vars.
% This should be changed, so only g_all and g_some can have global vars.
% Also, the defn of a local var should be fixed up - if a var is local
% to a term, it is local to all outer terms.
%
% In the following, L,L1 denote the terms of local (quantified) vars.
% If there are no quantifiers, L is [].  G denotes the global vars
% (all vars except locals).

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

% The same transformations are done for g_some and g_all.  These
% allow global variables to be quantified (all and some give errors).

% some, all, etc, should be prefix binary operators but the code
% does not assume that, so it can be run more easily.

	% transform clause
trf(T1, T2) :-
	trf(T1, T2, []).

	% transform high level term to low level
	% V is a term containing all global vars
trf(T, T, _V) :-
	var(T),
	!.
trf(T, T, _V) :-
	atomic(T),
	!.
trf(all(L, Var), all(L, Var), V) :-
	var(Var),
	!.
trf(all(L, T1 ~= T2), $is_eq(L2, T1, T2, fail), V) :-
	!,
	extra_vars(L, [], L1),
	check_local(L1, V, all(L, T1~=T2)),
	get__(T1 ~= T2, L_),
	append(L_, L1, L2).
trf(g_all(L, Var), g_all(L, Var), V) :-
	var(Var),
	!.
trf(g_all(L, T1 ~= T2), $is_eq(L2, T1, T2, fail), V) :-
	!,
	extra_vars(L, [], L1),
	get__(T1 ~= T2, L_),
	append(L_, L1, L2).
trf((T1 ~= T2), $is_eq(L2, T1, T2, fail), V) :-
	!,
	get__(T1 ~= T2, L2).
trf(all(L, not C), $if(G, C1, fail, true), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, (all(L, not C))).
trf(g_all(L, not C), $if(G, C1, fail, true), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G).
trf(not(Var), not(Var), V) :-
	var(Var),
	!.
trf((not some(L, C)), $if(G, C1, fail, true), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, (not some(L, C))).
trf((not g_some(L, C)), $if(G, C1, fail, true), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G).
trf((not C), $if(G, C1, fail, true), V) :-
	!,
	trf(C, C1, V),
	extra_vars(C, [], G).
trf((if I), T, V) :-
	!,
	trf_if((if I), T, V).
trf(solutions(T, C, S), $solutions(G, T, C1, S), V) :-
	(	var(C)
	;
		C \= some(_, _),	% change to NU-Negation
		C \= g_some(_, _)	% change to NU-Negation
	),
	!,
	trf(C, C1, V),
	extra_vars(T, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, solutions(T, C, S)).
trf(solutions(T, some(L, C), S), $solutions(G, T, C1, S), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L.T, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, solutions(T, some(L, C), S)).
trf(solutions(T, g_some(L, C), S), $solutions(G, T, C1, S), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L.T, [], L1),
	extra_vars(C, L1, G).
trf(some(L, C), $some(G, C1), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, (some(L, C))).
trf(g_some(L, C), $some(G, C1), V) :-
	!,
	trf(C, C1, V),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G).
trf(T, T1, V) :-
	meta_call(T, Args, Args1, T1),
	!,
	trf_list(Args, Args1, V).
trf(T, T, V).

	% trf list of args
	% V contains the initial global vars plus all the
	% preceeding arguments.  trf is called with this and
	% L, all the succeeding arguments.
trf_list([], [], _).
trf_list(A0.L, A.L1, V) :-
	trf(A0, A, L.V),
	trf_list(L, L1, A0.V).

	% trf - special case for if statements
	% should clean this all up
trf_if((if Var), (if Var), V) :-
	var(Var),
	!.
trf_if((if Var then T), (if Var then T), V) :-
	var(Var),
	!.
trf_if((if Var then T else E), (if Var then T else E), V) :-
	var(Var),
	!.
trf_if((if some(L, C) then T else E), R, V) :-
	!,
	trf(C, C1, V.T.E),
	trf(T, T1, V.C.E),
	trf(E, E1, V.C.T),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V.E, (if some(L, C) then T else E)),
	( is_local(L1, T) ->
		R = $if(G, C1, T1, E1)
	;
		R = $if_soft(G, C1, T1, E1)
	).
trf_if((if g_some(L, C) then T else E), R, V) :-
	!,
	trf(C, C1, V.T.E),
	trf(T, T1, V.C.E),
	trf(E, E1, V.C.T),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	( is_local(L1, T) ->
		R = $if(G, C1, T1, E1)
	;
		R = $if_soft(G, C1, T1, E1)
	).
trf_if((if C then T else E), $if(G, C1, T1, E1), V) :-
	!,
	trf(C, C1, V.T.E),
	trf(T, T1, V.C.E),
	trf(E, E1, V.C.T),
	extra_vars(C, [], G).
trf_if((if some(L, C) then T), R, V) :-
	!,
	trf(C, C1, V.T),
	trf(T, T1, V.C),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	check_local(L1, V, (if some(L, C) then T)),
	( is_local(L1, T) ->
		R = $if(G, C1, T1, true)
	;
		R = $if_soft(G, C1, T1, true)
	).
trf_if((if g_some(L, C) then T), R, V) :-
	!,
	trf(C, C1, V.T),
	trf(T, T1, V.C),
	extra_vars(L, [], L1),
	extra_vars(C, L1, G),
	( is_local(L1, T) ->
		R = $if(G, C1, T1, true)
	;
		R = $if_soft(G, C1, T1, true)
	).
trf_if((if C then T), $if(G, C1, T1, true), V) :-
	!,
	trf(C, C1, V.T),
	trf(T, T1, V.C),
	extra_vars(C, [], G).

	% check list of vars are not in global term
	% if they are, print message, using third arg
check_local(V, G, C) :-
	( is_local(V, G) ->
		true
	;
		write(2, '% Error: nonlocal quantified var in '),
		functor(C, F, _),
		write(2, F),
		%write(' ... '),
		%arg(1, C, A),
		%functor(A, F1, _),
		%write(2, F1),
		writeln(2, ' ...')
	).
