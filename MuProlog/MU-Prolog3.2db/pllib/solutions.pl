%	$Header: solutions.pl,v 1.2 85/10/12 22:33:40 lee Exp $
%
%	Logical all solutions pred solutions/3 is transformed (trf.pl)
%	into $solutions/4, the first arg being the global vars.
%
%	This is still rather hacky and needs to be cleaned up.  It may even
%	have a bug or two.  Still relies on MU-Prolog stuff.
%
%	Error messages are printed if there are local variables in answers or
%	delayed calls in the goals.
%	Noteq and term_compare(?) should be implemented in C, not Prolog -
%	this version is far slower than it could be
%
%						Lee Naish

%?- hide([tsort(2), tinsert(3), prove(1), $bag]).

$solutions(G, X, P, Set) :-
	$bagof(X, P, G, Bag),
	tsort(Bag, Set).

$bagof(X, P, Key, Bag) :-
	asserta(bagtmp($bag)),
	prove(P),
	asserta(bagtmp(Key - X)),
	make_ground(Key),
	not ground(X),
	writeln(2, 'Error: unbound local variables in solutions'),
	fail.
$bagof(X, P, Key, Bag) :-
	$reap([], Bags),
	$pick(Bags, Key, Bag).

$nonempty(_._).

$reap(L0, L) :-
	retract(bagtmp(X)),
	!,
	$reap1(X, L0, L).

$reap1(X, L0, L) :-
	X \== $bag,
	!,
	$reap(X.L0, L).
$reap1(_, L, L).

$pick([], _, []).
$pick(Key-T1.Tn, Key, T1.S) :-
	$pick(Tn, Key, S).
$pick(K-T1.Tn, Key, S) :-
	$is_eq(K, Key, K, fail),	% all K Key ~= K,
	$pick(Tn, Key, S).

prove(P) :-		% instead of call, which is effected by cut
	$ndelay(N),
	P,
	$ndelay(M),
	(if N =\= M then
		writeln('Error: delayed call(s) in solutions')
	).

	% should write a more efficient sort (not O(N*N))
tsort([], []).
tsort(A.B, C.D) :-
	tsort(B, E),
	tinsert(A, E, C.D).

tinsert(A, [], [A]).
tinsert(A, B.C, D) :-
	term_compare(R, A, B),
	(	R = (<),
		D = A.B.C
	;
		R = (=),
		tinsert(A, C, D)
	;
		R = (>),
		D = B.E,
		tinsert(A, C, E)
	).

%?- protect([$setof(3), $bagof(3), $bagof(4), $nonempty(1), $reap(2), $reap1(3),
	%$pick(3), $excess_vars(4), $rem_excess_vars(5), $introduce(3)]).

%?- ['subsumes.pl', 'compare.pl', 'ground.pl'].
?- lib subsumes, lib compare, lib ground.
