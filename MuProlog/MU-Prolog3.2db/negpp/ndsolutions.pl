%==================================================================
%	$solutions/4
%	This is still rather hacky and needs to be cleaned up.

$solutions(G, X, P, Set) :-
	$bagof(X, P, G, Bag),
	tsort(Bag, Set).

$bagof(X, P, Key, Bag) :-
	asserta(bagtmp($bag)),
	prove(P),
	asserta(bagtmp(Key - X)),
	make_ground(Key),
	\+ ground(X),
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
	$pick(Tn, Key, S),
	$noteq(K, Key, K).		% all K Key ~= K.

prove(P) :-		% instead of call, which is effected by cut
	P.

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

	% subsumes/2 arg1 subsumes arg2
subsumes(A, B) :-
	\+ \+ subsumes1(A, B).

subsumes1(A, B) :-
	bindvars(0, _, B),
	A = B.

bindvars(N, M, b(N)) :-
	M is N + 1,
	!.
bindvars(N, N, M) :-
	atomic(M),
	!.
bindvars(N, M, T) :-
	T =.. F.A,
	bindlv(N, M, A).

bindlv(N, N, []).
bindlv(N, M, H.T) :-
	bindvars(N, N1, H),
	bindlv(N1, M, T).

%	term comparison
%	logical version is term_compare/3 which should delay with vars
%	This version prints an error instead.

term_compare((=), X, X).
term_compare(C, X, Y) :-
	$noteq([], X, Y),
	compare1(X, Y, C).

compare1(X, Y, C) :-
	var(X),
	var(Y),
	write('Error: should delay '),
	write(compare1(X, Y, C)),
	nl.
	% abort.
compare1(X, Y, (<)) :-
	int(X),
	int(Y),
	X < Y.
compare1(X, Y, (>)) :-
	int(X),
	int(Y),
	X > Y.
compare1(X, Y, (<)) :-
	int(X),
	non_int(Y).
compare1(X, Y, (>)) :-
	int(Y),
	non_int(X).
compare1(X, Y, C) :-
	X =.. XF.XA,
	Y =.. YF.YA,
	length(XA, XN),
	length(YA, YN),
	(	XN < YN,
		C = (<)
	;	 
		XN > YN,
		C = (>)
	;
		XN = YN,
		ncompare(XF, XA, YF, YA, C)
	).

ncompare(XF, XA, XF, YA, C) :-
	lcompare(XA, YA, C).
ncompare(XF, XA, YF, YA, C) :-
	XF ~= YF,
	name(XF, XN),
	name(YF, YN),
	scompare(XN, YN, C).

lcompare(X, Y, C) :-
	var(X),
	var(Y),
	write('Error: should delay '),
	write(lcompare(X, Y, C)),
	nl.
	% abort.
lcompare(A.XA, B.YA, C) :-
	term_compare(C1, A, B),
	lcompare1(XA, YA, C, C1).

lcompare1(X, Y, C, C1) :-
	var(X),
	var(Y),
	write('Error: should delay '),
	write(lcompare1(X, Y, C, C1)),
	nl.
	% abort.
lcompare1(XA, YA, C, C1) :-
	(	C1 = (<),
		C = (<)
	;	 
		C1 = (>),
		C = (>)
	;
		C1 = (=),
		lcompare(XA, YA, C)
	).

scompare(X, Y, C) :-
	var(X),
	var(Y),
	write('Error: should delay '),
	write(scompare(X, Y, C)),
	nl.
	% abort.
scompare([], [], (=)).
scompare([], _._, (<)).
scompare(_._, [], (>)).
scompare(A.X, A.Y, C) :-
	scompare(X, Y, C).
scompare(A.X, B.Y, (<)) :-
	A < B.
scompare(A.X, B.Y, (>)) :-
	A > B.

non_int(X) :-
	X =.. _.

