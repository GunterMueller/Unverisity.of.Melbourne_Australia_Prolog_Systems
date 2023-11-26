/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

% Nepolog conversion predicates.

%
% atomToString(Atom, String) : convert between atoms and strings.
% intToAtom(Int, Atom) : convert between integers and atoms.
% intToString(Int, String) : convert between integers and strings.
%

atomToString(Atom, Ch.String) :-
	name(Atom, Ch.String).

?- intToAtom(Int, Atom) when Int or Atom.
intToAtom(Int, Atom) :-
	intToString(Int, String),
	name(Atom, String).

?- intToString(Int, String) when Int or ground(String).
intToString(Int, String) :-
	(integer(Int) ->
		(Int > 0 ->
			$convertIToS(Int, [], String)
		;	(Int =:= 0 ->
				String = "0"
			;	MagInt is - Int,
				$convertIToS(MagInt, [], String1),
				String = 0'-.String1
			)
		)
	;	var(Int),					% Prevents intToString("abc", X) bug.
		(String = 0'-.String1 ->
			$convertSToI(String1, 0, MagInt),
			Int is - MagInt
		;	$convertSToI(String, 0, Int)
		)
	).

?- $convertSToI(String, _, _) when String.
$convertSToI([], Int, Int).
$convertSToI(D.String, N, Int) :-
	0'0 =< D and D =< 0'9,
	N1 is N * 10 + D - 0'0,
	$convertSToI(String, N1, Int).

$convertIToS(Int, String, String1) :-
	(Int =:= 0 ->
		String = String1
	;	D is Int mod 10 + 0'0,
		Int1 is Int // 10,
		$convertIToS(Int1, D.String, String1)
	).

intToAtom(Radix, Int, Atom) :-
	intToAtom(Radix, 0'a, Int, Atom).

intToString(Radix, Int, String) :-
	intToString(Radix, 0'a, Int, String).

?- intToAtom(Radix, A, Int, Atom) when Radix and A and (Int or Atom).
intToAtom(Radix, A, Int, Atom) :-
	intToString(Radix, A, Int, String),
	name(Atom, String).

?- intToString(Radix, A, Int, String) when
		Radix and A and (Int or ground(String)).
intToString(Radix, A, Int, String) :-
	(integer(Int) ->
		(Int > 0 ->
			$convertIToS(Radix, A, Int, [], String)
		;	(Int =:= 0 ->
				String = "0"
			;	MagInt is - Int,
				$convertIToS(Radix, A, MagInt, [], String1),
				String = 0'-.String1
			)
		)
	;	var(Int),					% Prevents intToString("abc", X) bug.
		(String = 0'-.String1 ->
			$convertSToI(Radix, A, String1, 0, MagInt),
			Int is - MagInt
		;	$convertSToI(Radix, A, String, 0, Int)
		)
	).

?- $convertSToI(_, _, String, _, _) when String.
$convertSToI(_, _, [], Int, Int).
$convertSToI(Radix, A, D.String, N, Int) :-
	$radixDigit(Radix, A, D),
	$charToDigit(D, A, DV),
	N1 is N * Radix + DV,
	$convertSToI(Radix, A, String, N1, Int).

$convertIToS(Radix, A, Int, String, String1) :-
	( Int =:= 0 ->
		String = String1
	;	D is Int mod Radix,
		$digitToChar(D, A, C),
		Int1 is Int // Radix,
		$convertIToS(Radix, A, Int1, C.String, String1)
	).

$radixDigit(Radix, A, D) :-
		Radix =< 10 and 0'0 =< D and D < 0'0 + Radix
	or	Radix > 10 and (0'0 =< D and D =< 0'9 or A =< D and D < A - 10 + Radix).

$charToDigit(C, A, D) :-
	( 0'0 =< C and C =< 0'9 ->
		D is C - 0'0
	; A =< C and C < A + 26 ->
		D is C - A + 10
	;	fail
	).

$digitToChar(D, A, C) :-
	( D < 10 ->
		C is D + 0'0
	;	C is D - 10 + A
	).
