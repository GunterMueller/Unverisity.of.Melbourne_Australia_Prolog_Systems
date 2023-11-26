/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog builtin predicates.

true.
fail :- fail.
$cutd(L) :- $cutd(L).
$softCut(L) :- $softCut(L).
$label(L) :-
	format(user_error,
		"~NError in ~w -- not permitted in interpreted code at the moment.~n",
		[$label(L)]),
	abort.
$funcall(_) :- $funcallBuiltin(0).		% Used as $exect/1.
$funcall(_, _) :- $funcallBuiltin(1).
$funcall(_, _, _) :- $funcallBuiltin(2).
$funcall(_, _, _, _) :- $funcallBuiltin(3).
$funcall(_, _, _, _, _) :- $funcallBuiltin(4).
$funcall(_, _, _, _, _, _) :- $funcallBuiltin(5).
$funcall(_, _, _, _, _, _, _) :- $funcallBuiltin(6).
$apply(_, _) :- $applyBuiltin(1).
$apply(_, _, _) :- $applyBuiltin(2).
$apply(_, _, _, _) :- $applyBuiltin(3).
$apply(_, _, _, _, _) :- $applyBuiltin(4).
$apply(_, _, _, _, _, _) :- $applyBuiltin(5).
$apply(_, _, _, _, _, _, _) :- $applyBuiltin(6).
$execs(G) :- $execs(G).
throw(R) :- throw(R).
var(T) :- var(T).
$ref(T) :- $ref(T).
$del(T) :- $del(T).
nonvar(T) :- nonvar(T).
$icn(T) :- $icn(T).
$ucn(T) :- $ucn(T).
compound(T) :- compound(T).
const(T) :- const(T).
cons(T) :- cons(T).
$string(T) :- $string(T).
term(T) :- term(T).
$struct(T) :- $struct(T).
$bmv(T) :- $bmv(T).
atom(T) :- atom(T).
integer(T) :- integer(T).
atomic(T) :- atomic(T).
$block(T) :- $block(T).
float(T) :- float(T).
number(T) :- number(T).
X = Y :- X = Y.
X < Y :- X < Y.
X =< Y :- X =< Y.
X > Y :- Y < X.
X >= Y :- Y =< X.
X =:= Y :- X =:= Y.
X =\= Y :- X =\= Y.
X is Y :- X is Y.
X == Y :- X == Y.
X \== Y :- X \== Y.
$eRef(X, Y) :- $eRef(X, Y).
$mkObj(W, X, Y, Z) :- $mkObj(W, X, Y, Z).
X @< Y :- X @< Y.
X @=< Y :- X @=< Y.
X @> Y :- X @> Y.
X @>= Y :- X @>= Y.
compare(X, Y, Z) :- compare(X, Y, Z).
$sort(X, Y, Z) :- $sort(X, Y, Z).
X and Y :- X and Y.
X or Y :- X or Y.
X ~= Y :- X ~= Y.
'$copyVariablesToTopOfHeap:-)'(X) :- '$copyVariablesToTopOfHeap:-)'(X).
$fvar(X, Y) :- $fvar(X, Y).
ground(X) :- ground(X).
$load(X) :- $load(X).
$iload(X, Y, Z) :- $iload(X, Y, Z).
$fload(W, X, Y, Z) :- $fload(W, X, Y, Z).
fork(W, X, Y) :- fork(W, X, Y).
fork(W, X, Y, Z) :- fork(W, X, Y, Z).
$spy(X, Y) :- $spy(X, Y).
$nospy(X, Y) :- $nospy(X, Y).
$replacn(X, Y, Z) :- $replacn(X, Y, Z).
setarg(X, Y, Z) :- setarg(X, Y, Z).
$oncut(X, Y) :- $oncut(X, Y).
put(X) :- put(X).
put(X, Y) :- put(X, Y).
putl(X) :- putl(X).
putl(S, X) :- putl(S, X).
$display(X) :- $display(X).
$display(S, X) :-
	currentOutput(O),
	setOutput(S),
	$display(X),
	setOutput(O).
$printf(S, F, X) :- $printf(S, F, X).
$printNumber(F, P, X, S) :- $printNumber(F, P, X, S).
$sprt(F, X, S) :- $sprt(F, X, S).
X =.. Y :- X =.. Y.
occurs(X, Y) :- occurs(X, Y).
name(X, Y) :- name(X, Y).
$listToString(X, Y) :- $listToString(X, Y).
$flags(X) :- $flags(X).
$defined(X, Y) :- $defined(X, Y).
$defined(X, Y, Z, A) :- $defined(X, Y, Z, A).
$predicateArities(X, Y) :- $predicateArities(X, Y).
%$file(X, Y, Z) :- $file(X, Y, Z).
plus(X, Y, Z) :- plus(X, Y, Z).
times(X, Y, Z) :-		% X*Y=Z
	Z is X * Y,
	Y is Z // X,
	X is Z // Y.
divides(Num, Den, Div, Mod) :- 	% Num/Den=Div, Num mod Den=Mod
	Div is Num // Den,
	Mod is Num mod Den,
	Num is Den * Div + Mod,
	(if Div = 0 then true else Den is (Num - Mod) // Div).
functor(X, Y, Z) :- functor(X, Y, Z).
arg(X, Y, Z) :- arg(X, Y, Z).

$aref(X, Y, Z) :- $aref(X, Y, Z, 0).
$arefl(X, Y, Z) :- $aref(X, Y, Z, 1).
$aset(W, X, Y, Z) :- $aset(W, X, Y, Z, 0).

$error(X, Y) :-
	format(user_error,
		"~NError in ~w -- not permitted in interpreted code at the moment.~n",
		[$error(X, Y)]),
	fail.

repeat.
repeat :- repeat.

%	The following are handled directly by call/2 and should never
%	be called directly.
! :-
	format(user_error,
		"~NError in ~w.~nShould not be possible to call this directly.~n",
		[!]),
	fail.
$softCut :-
	format(user_error,
		"~NError in ~w.~nShould not be possible to call this directly.~n",
		[$softCut]),
	fail.
(A, B) :-
	format(user_error,
		"~NError in ~w.~nShould not be possible to call this directly.~n",
		[(A, B)]),
	fail.
(A; B) :-
	format(user_error,
		"~NError in ~w.~nShould not be possible to call this directly.~n",
		[(A; B)]),
	fail.

/* In callnot.nl
?- (if X then _) when ground(X).
?- (if X then _ else _) when ground(X).
(if X then Y) :-
	X -> Y.
(if X then Y else Z) :-
	X -> Y; Z.
*/

\+ G :- \+ G.
once(G) :- once(G).
A -> B :- A -> B.
A \= B :- A \= B.
abort :- abort.
exit(X) :- exit(X).
$symbol(X, Y) :- $symbol(X, Y).
$copy(0, X, Y, Z) :-
	$copy(0, X, Y, Z).
$copy(1, X, Y, Z) :-
	$copy(1, X, Y, Z).
$uncopy(X, Y, Z) :- $uncopy(X, Y, Z).
erase(Ref) :- erase(Ref).
$abolishCode(Pred, Arity) :- $abolishCode(Pred, Arity).
$makeBMT(Key, Term, BMT, P) :- $makeBMT(Key, Term, BMT, P).
$linkBMT(0, BMT, Pred) :- $linkBMT(0, BMT, Pred).
$linkBMT(1, BMT, Pred) :- $linkBMT(1, BMT, Pred).
instance(BMT, Value) :- instance(BMT, _Key, Value).
instance(BMT, Key, Value) :- instance(BMT, Key, Value).
$proplist(Atom, PropList) :- $proplist(Atom, PropList).
$proplist(Atom, Key, PropList) :- $proplist(Atom, Key, PropList).
get0(X) :- get0(X).
get(X) :- get(X).
get0(S, X) :- get0(S, X).
get(S, X) :- get(S, X).
getToken(S, X, Y) :- getToken(S, X, Y).
$tokenize(S0, X, Y, S1) :- $tokenize(S0, X, Y, S1).
nl :- nl.
nl(S) :- nl(S).
$open(X, Y, Z) :- $open(X, Y, Z).
$close(X) :- $close(X).
$currentStream(X, Y, Z, A) :- $currentStream(X, Y, Z, A).
$getStream(X, Y) :- $getStream(X, Y).
flushOutput(X) :- flushOutput(X).
clearIOError(X) :- clearIOError(X).
setInput(X) :- setInput(X).
setOutput(X) :- setOutput(X).
currentInput(X) :- currentInput(X).
currentOutput(X) :- currentOutput(X).
ttyget0(X) :- ttyget0(X).
ttyget(X) :- ttyget(X).
ttyskip(X) :- ttyskip(X).
ttyput(X) :- ttyput(X).
ttynl :- ttynl.
ttytab(N) :- ttytab(N).
ttyflush :- ttyflush.

/*
 * Things that might one day be builtins, but aren't yet.
 */

?- isAtom(X) when X.
isAtom(X) :-
	atom(X).

?- isAtomic(X) when X.
isAtomic(X) :-
	atomic(X).

?- isConst(X) when X.
isConst(X) :-
	const(X).

?- isCons(X) when X.
isCons(X) :-
	cons(X).

?- isInt(X) when X.
isInt(X) :-
	integer(X).

?- isFloat(X) when X.
isFloat(X) :-
	float(X).

?- isNumber(X) when X.
isNumber(X) :-
	number(X).

?- isTerm(X) when X.
isTerm(X) :-
	term(X).

?- isExpression(X) when X.
isExpression(X) :- number(X).
isExpression(pi).
isExpression(maxint).
isExpression(minint).
isExpression(not(X)) :- isExpression(X).
isExpression(+X) :- isExpression(X).
isExpression(-X) :- isExpression(X).
isExpression(\X) :- isExpression(X).
isExpression(sin(X)) :- isExpression(X).
isExpression(cos(X)) :- isExpression(X).
isExpression(tan(X)) :- isExpression(X).
isExpression(asin(X)) :- isExpression(X).
isExpression(acos(X)) :- isExpression(X).
isExpression(atan(X)) :- isExpression(X).
isExpression(exp(X)) :- isExpression(X).
isExpression(log(X)) :- isExpression(X).
isExpression(log10(X)) :- isExpression(X).
isExpression(sqrt(X)) :- isExpression(X).
isExpression(integer(X)) :- isExpression(X).
isExpression(float(X)) :- isExpression(X).
isExpression(round(X)) :- isExpression(X).
isExpression(+(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(-(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(*(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(/(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(//(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(mod(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(**(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(atan2(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(/\(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(\/(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(^(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(<<(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(>>(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(=:=(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(=\=(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(<(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(=<(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(>(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(>=(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(and(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(or(X, Y)) :- isExpression(X), isExpression(Y).
isExpression(integer8At(X)) :- isExpression(X).
isExpression(integer_8_at(X)) :- isExpression(X).
isExpression(unsigned8At(X)) :- isExpression(X).
isExpression(unsigned_8_at(X)) :- isExpression(X).
isExpression(integer16At(X)) :- isExpression(X).
isExpression(integer_16_at(X)) :- isExpression(X).
isExpression(unsigned16At(X)) :- isExpression(X).
isExpression(unsigned_16_at(X)) :- isExpression(X).
isExpression(integerAt(X)) :- isExpression(X).
isExpression(integer_at(X)) :- isExpression(X).
isExpression(addressAt(X)) :- isExpression(X).
isExpression(address_at(X)) :- isExpression(X).
isExpression(singleAt(X)) :- isExpression(X).
isExpression(single_at(X)) :- isExpression(X).
isExpression(doubleAt(X)) :- isExpression(X).
isExpression(double_at(X)) :- isExpression(X).
