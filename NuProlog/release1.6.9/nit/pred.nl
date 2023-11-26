%	Copyright (C) 1986, The University of Melbourne
%
%	predcheck.nl
%	Find all undefined calls, and find predicates which aren't used.
%
%	Author: J. Zobel, November 1986
%	Revised December 1987: Justin Zobel, Philip Dart, Giles Lean.


%-- arity_check -- Find predicates with same functor and different arity.

arity_check(F, _) :-
	( \+ get_error(arity - (F/_))
	->	findall(A, dPred(F, A), Arities),
		( Arities = []
		->	format(user_error,
		"Warning: No predicates for arity check on functor ~a.\n", [F])
		;	( Arities = _._._
			->	sort(Arities, Arities1),
				add_error(arity - (F/Arities1))
			)
		)
	).

%-- used_check --

used_check(F, A) :-
	getprop($nit, $called, F/A).
used_check(F, A) :-
	dDCall(F1, A1, F, A), F1/A1 \= F/A.
used_check(F, A) :-
	add_error(unused - [F, A]).

%-- call_check -- Find all undefined calls.  Record an error for any non-logical
%		  calls inside a pure predicate.

call_check(F, A) :- 
	forall(dClause(F, A, cl((H :- B), _, _)),
		once call_check(F/A, (H :- B), B)
	).

?- call_check(X, Y, _) when X and Y.
call_check(H, C, G) :-
	% BUG!  This has an endearing touch of directness to it.
	var(G),
	!.
call_check(H, C, G) :-
	$meta_call(G, SGs, _, _),
	functor(G, F, A),
	check_construct(H, C, F/A),
	call_check_list(H, C, SGs).
call_check(Func/Arity, (H :- B), Pred) :-
	functor(Pred, PF, PA),
	((	option_set(pure_check),
		once nonlogicalPredicate(PF, PA),
		dDecs(Func, Arity, pure, _._)
	) ->	add_error(nonlog_pure - (H :- B) - [PF, PA])
	),
	(( 	PA = 1, arg(1, Pred, Arg),
		occurs(PF, (assert, asserta, assertz, retract, retractall)),
		nonvar(Arg),
		(Arg = (Head :- _)
		->	nonvar(Head), functor(Head, F, A)
		;	functor(Arg, F, A)
		)
	) ->	addprop($nit, $modified, F/A)
	),
	( (Pred = retractall(F2, A2), atom(F2), isInt(A2))
	->	add_error(modified - F2/A2)
	),
	(	dPred(PF, PA)
	;	getprop($nit, $dbClause, (PF/PA))
	;	systemPredicate(PF, PA)
	;	getprop($nit_lib, $lib, Lib), libraryPredicate(Lib, PF, PA)
	;	add_error(undefined - (H :- B) - [PF, PA])
	).

?- call_check_list(H, C, L) when H and C and L.
call_check_list(_, _, []).
call_check_list(H, C, SG.SGs) :-
	call_check(H, C, SG),
	call_check_list(H, C, SGs).

check_construct(F/A, (H :- B), CF/CA) :-
	((	member(CF/CA, [(->)/2, (\+)/1, setof/3, bagof/3, findall/3]),
		option_set(pure_check),
		dDecs(F, A, pure, _._)
	) ->	add_error(nonlog_pure - (H :- B) - [CF, CA])
	).
