%	$Header: npp.pl,v 1.1 85/11/25 19:36:33 lee Exp $

%	NU-Negation transformation - main loop and utilities.

?- op(1170,fx,(if)).
?- op(1160,xfx, (else)).
?- op(1150,xfx, (then)).
?- op(900,fy, not).
?- op(700,xfx, ~=).
?- op(600,xfy,'.').

	% loop to read program, transform it and spit it out
main(_) :-
	% wflags(4),
	repeat,
	read(X),
	%read2(X, _dict),	% prefix binary op version
	% (	X == end_of_file,
	 (	nonvar(X),
		eof(X),
		!
	;
		X = ?-(op(P, T, F)),
		op(P, T, F),
		fail
	;
		trf(X, X1),
		writecl(X1),
		fail
	).

	% list of preds with preds as args (which need to be transformed)
	% Should be a user hook to add to this
	% Need to be careful with multiple levels of functor nesting -
	% it may construct a clause with meta vars.
meta_call((A, B), [A, B], [A1, B1], (A1, B1)).
meta_call((A :- B), [B], [B1], (A :- B1)).
meta_call((A; B), [A, B], [A1, B1], (A1; B1)).
meta_call(?-(A), [A], [A1], ?-(A1)).
meta_call(when_ground(A, B), [B], [B1], when_ground(A, B1)).
meta_call(freeze(A, B), [B], [B1], freeze(A, B1)).
meta_call(call(A), [A], [A1], call(A1)).
		% standard negation - leave it out to engourage NU-Neg?
meta_call(\+(A), [A], [A1], \+(A1)).
meta_call(once(A), [A], [A1], once(A1)).
meta_call((A -> B), [A, B], [A1, B1], (A1 -> B1)).
%meta_call((A -> B ; C),... handled by -> and ; separately

	% L is a list of vars, G is the list of vars in T but not
	% in L.
	% "local" vars in T should not be counted. These are vars in
	% the first arg of all, some and solutions (+g_all, g_some, $is_eq?)
	% (new) and also underscores
extra_vars(T, L, G) :-
	extra_vars1(T, L, [], G).

extra_vars1(V, L, D, D) :-
	$is_(V),		% V is an underscore (MU-Prolog specific)
	!.
extra_vars1(V, L, D, D) :-
	var(V),			% V is in the list already
	occurs(V, L.D),
	!.
extra_vars1(V, L, D, V.D) :-
	var(V),			% new var - add to list
	!.
extra_vars1(A, L, D, D) :-
	atomic(A),		% atom/integer - ignore
	!.
extra_vars1(all(V, C), L, D, D1) :-
	!,
	extra_vars1(C, V.L, D, D1).
extra_vars1(some(V, C), L, D, D1) :-
	!,
	extra_vars1(C, V.L, D, D1).
extra_vars1(solutions(T, C, S), L, D, D1) :-
	!,
	ev1_list(C.S.[], T.L, D, D1).
extra_vars1(C, L, D, D1) :-
	C =.. F.A,		% complex term - (mutually) recurse
	ev1_list(A, L, D, D1).

ev1_list([], L, D, D).
ev1_list(H.T, L, D, D2) :-
	ev1_list(T, L, D, D1),
	extra_vars1(H, L, D1, D2).

	% get list of underscore vars from a term
get__(T, G) :-
	get__1(T, [], G).

get__1(V, D, V.D) :-
	$is_(V),		% V is an underscore (MU-Prolog specific)
	V = A,			% not any more! (HACK!!)
	!.
get__1(V, D, D) :-
	var(V),			% other var - ignore
	!.
get__1(A, D, D) :-
	atomic(A),		% atom/integer - ignore
	!.
get__1(C, D, D1) :-
	C =.. F.A,		% complex term - (mutually) recurse
	get__list(A, D, D1).

get__list([], D, D).
get__list(H.T, D, D2) :-
	get__list(T, D, D1),
	get__1(H, D1, D2).

	% list of vars are not in global term
is_local([], _).
is_local(V.T, G) :-
	\+ occurs(V, G),
	is_local(T, G).

	% write a clause nicely
writecl(C) :-
	assert(tmp(C)),		% fixes var names in MU-prolog
	retract(tmp(C)),
	!,
	portraycl(C),
	writeln(.).

% ?- [r].		% reader

?- lib append.
?- argv(A), A = _._._, lib('../sys/listing.pl').
