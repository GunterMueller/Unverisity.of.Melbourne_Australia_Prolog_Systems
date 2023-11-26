/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Philip Dart
 */

% numberVars(Term, M, N)
% 	unifies each variable in Term with a special term $VAR(I).
% 	M and N are integer arguments. M must be instantiated.
%
% varNumbers(Term, VTerm)
% 	reverse of numberVars: changes $VAR(N) to a variable, where N is atomic.
%

?- numberVars(_, N, _) when N.
numberVars(Term, N0, N) :-
	( var(Term) ->
		Term = $VAR(N0),
		N is N0 + 1
	; const(Term) ->
		N0 = N
	; Term = $VAR(M), integer(M) ->
		% Special case to ignore $VAR(N) (Optional)
		N0 = N
	;	functor(Term, _, A),
		$numberVars(A, Term, N0, N)
	).

?- $numberVars(A, _, _, _) when A.
$numberVars(A, Term, N0, N) :-
	(A > 0 ->
		A1 is A - 1,
		arg(A, Term, SubTerm),
		$numberVars(A1, Term, N0, N1),
		numberVars(SubTerm, N1, N)
	;	N0 = N
	).

varNumbers(Exp, VarExp) :-
	$varNumbers(Exp, [], VarExp, _).

$varNumbers(Exp0, In, Exp, Out) :-
	( var(Exp0) ->
		In = Out,
		Exp0 = Exp
	; const(Exp0) ->
		In = Out,
		Exp0 = Exp
	; Exp0 = $VAR(N), integer(N) ->
		( member(N - Exp, In)
		->	Out = In
		;	Out = (N - Exp).In
		)
	;	functor(Exp0, F, A),
		functor(Exp, F, A),
		$varNumbers(A, Exp0, In, Exp, Out)
	).

?- $varNumbers(A, _, _, _, _) when A.
$varNumbers(A, Exp0, In, Exp, Out) :-
	(A > 0 ->
		arg(A, Exp0, Exp1),
		arg(A, Exp, Exp2),
		$varNumbers(Exp1, In, Exp2, Out1),
		A1 is A - 1,
		$varNumbers(A1, Exp0, Out1, Exp, Out)
	;	In = Out
	).
