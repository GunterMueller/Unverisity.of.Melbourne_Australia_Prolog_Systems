/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart, 1988
 *		Based on a theme by Lawrence Byrd and Lee Naish.
 *	Fixed again by Lee Naish 9/88
 */

% May bear some vague resemblance to some code written by Lawrence Byrd
% at Edinburgh a long time ago.

$writeClause(Head, true) :-
	!,
	$qwrite(1200, Head).
$writeClause(Head, Body) :-
	$opPrec((:-), 1, Prec),
	$qwrite(Prec, Head),
	putl(" :-"),
	$writeBody(Body, 1, ',').

$writeDCGClause(Head, Body) :-
	$opPrec((-->), 1, Prec),
	$qwrite(Prec, Head),
	putl(" -->"),
	$writeBody(Body, 1, ',').

$writeBody((P, Q), IO, T) :-
	!,
	$writeBody(P, IO, T),
	put(0',),
	$aftercomma(T, IO, I),
	$writeBody(Q, I, ',').
$writeBody((if C then A else B), I, T) :-
	!,
	(T == (then) -> I1 is I + 1 ; I1 = I),
	(T == (else) -> putl(" if ") ; nl, $tabs(I1), putl("(if ")),
	$opPrec((then), 1, Prec),
	$qwrite(Prec, C),
	putl(" then"),
	$writeBody(A, I1, (then)),
	nl,
	$tabs(I1),
	putl("else"),
	$writeBody(B, I1, (else)),
	( T \== (else) -> nl, $tabs(I1), put(0'))).
$writeBody((if C then A), I, T) :-
	!,
	(T == (then) -> I1 is I + 1 ; I1 = I),
	(T == (else) -> putl(" if ") ; nl, $tabs(I1), putl("(if ")),
	$opPrec((then), 1, Prec),
	$qwrite(Prec, C),
	putl(" then"),
	$writeBody(A, I1, (then)),
	( T \== (else) -> nl, $tabs(I1), put(0'))).
$writeBody((C -> A ; B), I, T) :-
	!,
	(T == (then) ->
		I1 is I + 1
	;	I1 = I),
	(T == ('->;') ->
		putl(" ")
	;	nl, $tabs(I1), putl("( ")),
	$opPrec((->), 1, Prec),
	$qwrite(Prec, C),
	putl(" ->"),
	$writeBody(A, I1, (then)),
	nl,
	$tabs(I1),
	put(0';),
	$writeBody(B, I1, ('->;')),
	( T \== ('->;') -> nl, $tabs(I1), put(0'))).
$writeBody((C -> A), I, T) :-
	!,
	(T == (then) -> I1 is I + 1 ; I1 = I),
	(T == ('->;') -> putl(" ") ; nl, $tabs(I1), putl("( ")),
	$opPrec((->), 1, Prec),
	$qwrite(Prec, C),
	putl(" ->"),
	$writeBody(A, I1, (then)),
	( T \== ('->;') -> nl, $tabs(I1), put(0'))).
$writeBody((P ; Q), I, T) :-
	!,
	(	T = (;),
%		put(0'\t),
%		put(0'\s),
		$writeBody(P, I, (;))
	;	nl,
		$tabs(I),
		put(0'(),
		$writeBody(P, I, '(')
	),
	nl,
	$tabs(I),
	put(0';),
	$writeBody(Q, I, (;)),
	(	T = (;)
	;	nl,
		$tabs(I),
		put(0'))
	),
	!.
$writeBody(X, I, T) :-
	$beforelit(T, I),
	$opPrec((,), 1, Prec),	% Pos 1 of (,) has lowest prec of constructs
	$qwrite(Prec, X).

$aftercomma(',', I, I) :-
	!.
$aftercomma(_, IO, I) :-
	I is IO + 1.

$beforelit('(', _) :-
	put(0'\t).
$beforelit((;), I) :-
%	nl,
%	$tabs(I + 1).	% should really evaluate I + 1 here
	put(0'\t).
$beforelit((then), I) :-
	nl,
	$tabs(I + 1).
$beforelit(('->;'), I) :-
%	nl,
%	$tabs(I + 1).
	put(0'\t).
$beforelit((else), I) :-
	nl,
	$tabs(I + 1).
$beforelit(',', I) :-
	nl,
	$tabs(I).

$tabs(N) :-
	(N > 0 ->
		put(0'\t),
		N1 is N - 1,
		$tabs(N1)
	;	true
	).

$qwrite(Prec, X) :-
	writev([prec = Prec, quote, list], X).

portraycl(X) :-
	numberVars(X, 0, _),
	$portraycl1(X),
	fail.
portraycl(_).

portraycl(S, C) :-
	currentOutput(O),
	setOutput(S),
	portraycl(C),
	setOutput(O).

$portraycl1((A :- B)) :-
	!,
	$writeClause(A, B).
$portraycl1((A --> B)) :-
	!,
	$writeDCGClause(A, B).
$portraycl1(A) :-
	$writeClause(A, true).

portraygoals(G) :-
	numberVars(G, 0, _),
	$qwrite(1200, G),
	fail.
portraygoals(_).

portraygoals(S, C) :-
	currentOutput(O),
	setOutput(S),
	portraygoals(C),
	setOutput(O).

(listing) :-
	\+ (
		currentPredicate(Pred, Arity),
		\+ predicateProperty(Pred, Arity, system),
		$listing(Pred, Arity),
		fail
	).

listing(X) :-
	\+ ground(X),
	!,
	format(user_error,
		"~NError: ~w -- argument not ground.~n", [listing(X)]),
	fail.
listing(Atom) :-
	atom(Atom),
	!,
	\+ (currentPredicate(Atom, Arity), $listing(Atom, Arity), fail).
listing(Pred/Arity) :-
	atom(Pred), integer(Arity),
	!,
	$listing(Pred, Arity).
listing(List) :-
	$list(List),
	!,
	\+ (member(X, List), listing(X), fail).
listing(X) :-
	format(user_error,
	  "~NError: ~w -- argument not (list of) predicate description(s).~n",
	  [listing(X)]),
	fail.

$listing(Pred, Arity) :-
	nl,
	(if some Type predicateProperty(Pred, Arity, Type) then
		( Type == compiled ->
			format("%\t~a/~d is compiled.\n", [Pred, Arity])
		; Type == database ->
			functor(X, Pred, Arity),
			$onDisc(DB, X, Access),
			format("%\t~a/~d is a ~a relation in database ~a.\n",
				[Pred, Arity, Access, DB]),
			putl("%\tClauses of database relations are not listed.\n")
		; Type == (pure) ->
			format(":- pure ~a/~d.\n", [Pred, Arity])
		; Type == (dynamic) ->
			\+ predicateProperty(Pred, Arity, database),
			format(":- dynamic ~a/~d.\n", [Pred, Arity]),
			\+ (getwhen(Pred, Arity, When),
				portraygoals((:- When)), putl(".\n"), fail),
			\+ (getclause(Pred, Arity, Clause),
					portraycl(Clause), putl(".\n"), fail)
		;	fail
		)
	else
		format("%\t~a/~d is not defined.\n", [Pred, Arity])
	).

$opPrec(Op, Pos, Prec) :-
	once (
		currentOp(Prec1, Type, Op),
		$opAdj(Pos, Type, Adj),
		Prec is Prec1 - Adj
	).

:- $opAdj(Pos, Type, _) when Pos and Type.
$opAdj(1, xfx, 1).
$opAdj(1, xfy, 1).
$opAdj(1, fxy, 1).
$opAdj(1, fxx, 1).
$opAdj(1, yfx, 0).
$opAdj(1, yfy, 0).
$opAdj(1, fyx, 0).
$opAdj(1, fyy, 0).
$opAdj(2, xfx, 1).
$opAdj(2, xfy, 0).
$opAdj(2, fxy, 0).
$opAdj(2, fxx, 1).
$opAdj(2, yfx, 1).
$opAdj(2, yfy, 0).
$opAdj(2, fyx, 1).
$opAdj(2, fyy, 0).
$opAdj(1,  xf, 1).
$opAdj(1,  fx, 1).
$opAdj(1,  yf, 0).
$opAdj(1,  fy, 0).
