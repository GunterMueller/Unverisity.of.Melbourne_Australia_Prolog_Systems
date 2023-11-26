%	$Header: unegtrf.pl,v 1.1 85/11/25 19:37:56 lee Exp $

% Transformation of high level negation constructs to low level ones.
% UNSOUND VERSION (doesnt do any checking either)
%

%	High Level	transformed to	Low Level
%
% all L T1 ~= T2		=====>	T1 \= T2
% all L not C			=====>	\+(C)
% not some L C			=====>	\+(C)
% if some L C then T else E	=====>	(C -> T ; E)
% 					or (\+C -> E ; C, T) (L occurs in T)
% if some L C then T		=====>	(C -> T)
% 					or (\+C -> true ; C, T) (L in T)
% some L C			=====>	once(C)
% solutions(L1, some L C, S)	=====>	setof(L1, L^C, S)

	% transform high level term to low level
trf(T, T) :-
	var(T),
	!.
trf(T, T) :-
	atomic(T),
	!.
trf(all(L, Var), all(L, Var)) :-	% what does this mean?
	var(Var),
	!.
trf(all(L, T1 ~= T2), T1 \= T2) :-
	!.
trf(g_all(L, Var), g_all(L, Var)) :-	% what does this mean?
	var(Var),
	!.
trf(g_all(L, T1 ~= T2), T1 \= T2) :-
	!.
trf((T1 ~= T2), T1 \= T2) :-
	!.
trf(all(L, not C), \+(C1)) :-
	!,
	trf(C, C1).
trf(g_all(L, not C), \+(C1)) :-
	!,
	trf(C, C1).
trf((not Var), \+(Var)) :-
	var(Var),
	!.
trf((not some(L, C)), \+(C1)) :-
	!,
	trf(C, C1).
trf((not g_some(L, C)), \+(C1)) :-
	!,
	trf(C, C1).
trf((not C), \+(C1)) :-
	!,
	trf(C, C1).
trf((if I), T) :-
	!,
	trf_if((if I), T).
trf(solutions(T, C, S), setof(T, C1, S)) :-
	(	var(C)
	;
		C \= some(_, _),	% change to NU-Negation
		C \= g_some(_, _)	% change to NU-Negation
	),
	!,
	trf(C, C1).
trf(solutions(T, some(L, C), S), setof(T, L^C1, S)) :-
	!,
	trf(C, C1).
trf(solutions(T, g_some(L, C), S), setof(T, L^C1, S)) :-
	!,
	trf(C, C1).
trf(some(L, C), once(C1)) :-
	!,
	trf(C, C1).
trf(g_some(L, C), once(C1)) :-
	!,
	trf(C, C1).
trf(T, T1) :-
	meta_call(T, Args, Args1, T1),
	!,
	trf_list(Args, Args1).
trf(T, T).

	% trf list of args
trf_list([], []).
trf_list(A0.L, A.L1) :-
	trf(A0, A),
	trf_list(L, L1).

	% trf - special case for if statements
	% should clean this up a bit?
trf_if((if Var), (if Var)) :-	% what does this mean?
	var(Var),
	!.
trf_if((if Var then T), (Var -> T1)) :-
	var(Var),
	!,
	trf(T, T1).
trf_if((if Var else E), (Var ; E1)) :-	% very dubious!
	var(Var),
	!,
	trf(E, E1).
trf_if((if Var then T else E), (Var -> T1 ; E1)) :-
	var(Var),
	!,
	trf(T, T1),
	trf(E, E1).
trf_if((if some(L, C) then T else E), R) :-
	!,
	trf(C, C1),
	trf(T, T1),
	trf(E, E1),
	extra_vars(L, [], L1),
	( is_local(L1, T) ->
		R = (C1 -> T1 ; E1)
	;
		R = (\+C1 -> E1 ; C1, T1)
	).
trf_if((if g_some(L, C) then T else E), R) :-
	!,
	trf(C, C1),
	trf(T, T1),
	trf(E, E1),
	extra_vars(L, [], L1),
	( is_local(L1, T) ->
		R = (C1 -> T1 ; E1)
	;
		R = (\+C1 -> E1 ; C1, T1)
	).
trf_if((if C then T else E), (C1 -> T1 ; E1)) :-
	!,
	trf(C, C1),
	trf(T, T1),
	trf(E, E1).
trf_if((if some(L, C) then T), R) :-
	!,
	trf(C, C1),
	trf(T, T1),
	extra_vars(L, [], L1),
	( is_local(L1, T) ->
		R = (C1 -> T1)
	;
		R = (\+C1 -> true ; C1, T1)
	).
trf_if((if g_some(L, C) then T), R) :-
	!,
	trf(C, C1),
	trf(T, T1),
	extra_vars(L, [], L1),
	( is_local(L1, T) ->
		R = (C1 -> T1)
	;
		R = (\+C1 -> true ; C1, T1)
	).
trf_if((if C then T), (C1 -> T1)) :-
	!,
	trf(C, C1),
	trf(T, T1).
