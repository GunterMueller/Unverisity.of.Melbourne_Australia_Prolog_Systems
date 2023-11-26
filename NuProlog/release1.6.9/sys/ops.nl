/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

% Nepolog predicates to do with operators.

op(Prec, Type, Ops) :-
	(	integer(Prec), 0 =< Prec and Prec =< 1200,
		atom(Type),
		(	atom(Ops)
		;	mapList(atom, Ops)
		)
	->	fail
	;	format(user_error,
			"~NError in ~w -- arguments not appropriate.~n",
			[op(Prec, Type, Ops)]),
		!,
		fail
	).
op(_, _, []) :-
	!.
op(Prec, Type, Ops) :-
	cons(Ops),
	!,
	mapList($addop(Prec, Type), Ops).
op(X, Y, Op) :-
	atom(Op),
	$addop(X, Y, Op).

$addop(Prec, Type, Op) :-
	atom(Op),
	integer(Prec),
	0 < Prec and Prec =< 1200,
	!,
	( $opField(Op, Def), Def \== [] ->
		( $sysOp(Op, _, _), \+ $sysOp(Op, Prec, Type) ->
			format(user_error,
				"~NWarning in ~w -- changing a system-defined operator.~n",
				[op(Prec, Type, Op)])
		),
		( parser$convert1(Type, Prec, Def, NewDef) ->
			$setOpField(Op, NewDef)
		;	format(user_error,
				"~NError in ~w -- invalid operator redefinition.~n",
				[op(Prec, Type, Op)]),
			fail
		)
	;	( parser$convert1(Type, Prec, op(Prec,n,n,n,n,n,n,n,n), NewDef) ->
			$setOpField(Op, NewDef),
			( $sysOp(Op, Prec, Type) ->
				true
			;	addpropa($operators, $currentOp, Op)
			)
		;	format(user_error,
				"~NError in ~w -- invalid operator redefinition.~n",
				[op(Prec, Type, Op)]),
			fail
		)
	).
$addop(0, Type, Op) :-
	atom(Op),
	!,
	( $sysOp(Op, _, _) ->
		format(user_error,
			"~NWarning in ~w -- undefining a system-defined operator.~n",
			[op(0, Type, Op)])
	),
	$setOpField(Op, []),
	remprop($operators, $currentOp, Op),
	!.

$defop(X, Y, Z) :- $addop(X, Y, Z).

$opField(Atom, Def) :-
	$symbol(Atom, Symbol),
	arg(5, Symbol, Def).

$setOpField(Atom, Value) :-
	$symbol(Atom, Symbol),
	% BUG!  Need to change the representation of precedences to an icn.
	( $copy(0, Value, 0, Skel) ->
		$replacn(5, Symbol, Skel)
	;	format(user_error,
			"~NError in ~w -- ground value expected.~n",
			[$setOpField(Atom, Value)])
	).

currentOp(Prec, Type, Op) :-
	( var(Op) ->
		(	$sysOp(Op, _, _)
		;	getprop($operators, $currentOp, Op)
		)
	),
	$opField(Op, Def),
	Def \== [],
	parser$convert2(Type, Prec, Def).
