/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% A nepolog compiler -- system utilities.

%	Declare a predicate dynamic.
dynamic(D) :-
	$applyToEachPredicate(X, D, co$dynamic(X)).

co$dynamic(Pred/Arity) :-
	( dynamic(Pred, Arity) ->
		true
	;	$dynamic(Pred/Arity)
	).

makeLabel(L) :-
	once
	(	$symbol(label, Def),
		arg(4, Def, PropList),
		$propertyMember(BMT, PropList),
		BMT = $bmt(_, _, L, $label, _, _),
		L1 is L + 1,
		$replacn(3, BMT, L1)
	).

?- initializing, $dynamic(groundTerm/3).
