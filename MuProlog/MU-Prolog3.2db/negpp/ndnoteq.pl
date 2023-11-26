%	$Header: ndnoteq.pl,v 1.1 85/11/25 19:35:58 lee Exp $
%
%	Sound inequality for systems without delaying.
%	Maybe should implement $is_eq/4 properly.

	% equality testing pred - returns result R.  Loc is local vars.
$is_eq(Loc, T1, T2, R) :-
	( R == fail ->
		$noteq(Loc, T1, T2)
	;
		write('Error: $is_eq only implements ~= : '),
		write($is_eq(Loc, T1, T2, R)),
		nl,
		fail
	).

	% $noteq(Loc, T1, T2) <-> all Loc T1 ~= T2
$noteq(Loc, T1, T2) :-
	T1 \= T2,		% dont unify -> true
	!.
$noteq(Loc, T1, T2) :-
	extra_vars(T1.T2, Loc, V),
	T1 = T2,		% unifies without
	extra_vars(V, [], V1),	% constructing globals -> false
	V == V1,
	!,
	fail.
$noteq(Loc, T1, T2) :-
	write('Error: should delay '),
	write($noteq(Loc, T1, T2)),
	nl.
	% abort.

	% L is a list of vars, G is the list of vars in T but not
	% in L.  It is important that if T is a list of vars and
	% L is [] then G is identical to T (the order of vars is
	% preserved).  Could be written better.
extra_vars(T, L, G) :-
	extra_vars1(T, L, [], G).

extra_vars1(V, L, D, D) :-
	var(V),			% V is in one of the lists already
	\+ \+((make_ground(L.D),	% (clean this up!)
		nonvar(V))),
	!.
extra_vars1(V, L, D, V.D) :-
	var(V),			% new var - add to list
	!.
extra_vars1(A, L, D, D) :-
	atomic(A),		% atom/integer - ignore
	!.
extra_vars1(C, L, D, D1) :-
	C =.. F.A,		% complex term - (mutually) recurse
	ev1_list(A, L, D, D1).

ev1_list([], L, D, D).
ev1_list(H.T, L, D, D2) :-
	ev1_list(T, L, D, D1),
	extra_vars1(H, L, D1, D2).

