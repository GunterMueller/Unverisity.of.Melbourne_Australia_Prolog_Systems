/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Justin Zobel and Lee Naish
 */

%
% xpp:		filter prolog source file, transforming all extended syntax
%			into standard (NU-) Prolog statements.
%
% Author:	J. Zobel, January 1986.
% Revised:	May 1986
% Again:	by Lee Naish, May 1986

%	Rename variables, remove extended syntax, locate errors.
$xs_trf(C, C1) :-
	$get_head(C, H),
	functor(H, F, N),
	( pure(F, N) ->
		$pure_trf(C, C1, [])
	;	$op_trf(C, C1, [])
	).

%	get head of clause
$get_head((H1:-_B), H2) :-
	!,
	H1 = H2.
$get_head(H, H).

%	Operational transformations.
%	_1 and _2 are the input and output expressions respectively.
%	_3 is an assocation list of variables which is passed down.
$op_trf(B, B1, Rename) :-
	var(B),
	!,
	B1 = call(B2),
	$change_vars(B, B2, Rename).
$op_trf(B, B1, Rename) :-
	$op_trf1(B, B1, Rename).

$op_trf1((V, W), C, Rename) :-
	!,
	C = (V1, W1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((V ; W), C, Rename) :-
	!,
	C = (V1 ; W1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((V -> W), C, Rename) :-
	!,
	C = (V1 -> W1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
%$op_trf(call(V), call(V1), Rename) :-	% should this be in?
%	!,
%	$op_trf(V, V1, Rename).
$op_trf1(not W, C, Rename) :-
	!,
	C = (not W1),
	$op_trf(W, W1, Rename).
$op_trf1(\+ W, C, Rename) :-
	!,
	C = (\+ W1),
	$op_trf(W, W1, Rename).
$op_trf1(once(W), C, Rename) :-
	!,
	C = once(W1),
	$op_trf(W, W1, Rename).
$op_trf1(?-(W), C, Rename) :-
	!,
	C = ?-(W1),
	$op_trf(W, W1, Rename).
$op_trf1(:-(W), C, Rename) :-
	!,
	C = :-(W1),
	$op_trf(W, W1, Rename).
$op_trf1((H :- W), C, Rename) :-
	!,
	C = (H :- W1),
	$op_trf(W, W1, Rename).
$op_trf1(gSome(V, W), C, Rename) :-
	!,
	C = gSome(V1, W1),
	$change_vars(V, V1, Rename),
	$op_trf(W, W1, Rename).
		% (BUG) (LEE)
		% we should really strip off all "all"s and "gAll"s
$op_trf1(all(U, (V <= W)), C, Rename) :-
	!,
	C = (not some(U1, (W1, not V1))),
	$add_vars(U, Rename, NewRename), 
	$change_vars(U, U1, NewRename),
	$op_trf(V, V1, NewRename),
	$op_trf(W, W1, NewRename).
$op_trf1(all(U, (V => W)), C, Rename) :-
	!,
	C = (not some(U1, (V1, not W1))),
	$add_vars(U, Rename, NewRename), 
	$change_vars(U, U1, NewRename),
	$op_trf(V, V1, NewRename),
	$op_trf(W, W1, NewRename).
$op_trf1(all(U, (V <=> W)), C, Rename) :-
	!,
	C = (not some(U1, (V1, not W1)), not some(U2, (W2, not V2))),
	$add_vars(U, Rename, NewRename), 
	$change_vars(U, U1, NewRename),
	$op_trf(V, V1, NewRename),
	$op_trf(W, W1, NewRename),
	$add_vars(U, Rename, NewRename2), 
	$change_vars(U, U2, NewRename2),
	$change_vars(V, V2, NewRename2),
	$change_vars(W, W2, NewRename2).
$op_trf1(gAll(U, (V <= W)), C, Rename) :-
	!,
	C = (not gSome(U, (W1, not V1))),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1(gAll(U, (V => W)), C, Rename) :-
	!,
	C = (not gSome(U, (V1, not W1))),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1(gAll(U, (V <=> W)), C, Rename) :-
	!,
	C = (not gSome(U, (V1, not W1)), not gSome(U, (W1, not V1))),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((V <= W), C, Rename) :-
	!,
	C = (V1 ; not W1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((V => W), C, Rename) :-
	!,
	C = (W1 ; not V1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((V <=> W), C, Rename) :-
	!,
	C = ((W1 ; not V1), (V1 ; not W1)),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1(some(V, W), C, Rename) :-
	!,
	C = some(V1, W1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$op_trf(W, W1, NewRename).
$op_trf1(solutions(V, W, S), C, Rename) :-
	!,
	C = solutions(V1, W1, S1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$op_trf(W, W1, NewRename).
$op_trf1(min(V, W, S), C, Rename) :-
	!,
	C = min(V1, W1, S1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$op_trf(W, W1, NewRename).
$op_trf1(max(V, W, S), C, Rename) :-
	!,
	C = max(V1, W1, S1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$op_trf(W, W1, NewRename).
$op_trf1(count(V, W, S), C, Rename) :-
	!,
	C = count(V1, W1, S1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$op_trf(W, W1, NewRename).
$op_trf1(sum(E, V, W, S), C, Rename) :-
	!,
	C = sum(E1, V1, W1, S1),
	$add_vars(E-V, Rename, NewRename), 
	$change_vars(E-V, E1-V1, NewRename),
	$change_vars(S, S1, Rename),
	$op_trf(W, W1, NewRename).
$op_trf1(all(V, W), C, Rename) :-
	var(W),
	!,
	C = all(V1, call(W1)),
	$add_vars(V, Rename, NewRename),
	$change_vars(V.W, V1.W1, NewRename).
$op_trf1(all(V, not W), C, Rename) :-
	!,
	C = all(V1, not W1),
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$op_trf(W, W1, NewRename).
$op_trf1(all(V, X ~= Y), C, Rename) :-
	!,
	C = all(V1, InEq),
	$add_vars(V, Rename, NewRename), 
	$change_vars((V, X ~= Y), (V1, InEq), NewRename).
$op_trf1(all(V, W), C, Rename) :-
	!,
	C = (not some(V1, not W1)),
	$change_vars(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1(gAll(V, W), C, Rename) :-
	!,
	C = gAll(V1, W1),
	$change_vars(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((if V then W), C, Rename) :-
	(	var(V)
	;	V \= some(_,_),
		V \= gSome(_,_)
	),
	!,
	C = (if V1 then W1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename).
$op_trf1((if Cond then W), C, Rename) :-
	!,
	C = (if some(LV1, gSome(GV1, Cond1)) then W1),
	$str_some(Cond, LV, GV, Cond2),
	$add_vars(LV, Rename, NewRename),
	$change_vars((GV.LV), (GV1.LV1), NewRename),
	$op_trf(Cond2, Cond1, NewRename),
	$op_trf(W, W1, NewRename).
$op_trf1((if V then W else X), C, Rename) :-
	(	var(V)
	;	V \= some(_,_),
		V \= gSome(_,_)
	),
	!,
	C = (if V1 then W1 else X1),
	$op_trf(V, V1, Rename),
	$op_trf(W, W1, Rename),
	$op_trf(X, X1, Rename).
$op_trf1((if Cond then W else X), C, Rename) :-
	!,
	C = (if some(LV1, gSome(GV1, Cond1)) then W1 else X1),
	$str_some(Cond, LV, GV, Cond2),
	$add_vars(LV, Rename, NewRename),
	$change_vars((GV.LV), (GV1.LV1), NewRename),
	$op_trf(Cond2, Cond1, NewRename),
	$op_trf(W, W1, NewRename),
	$op_trf(X, X1, Rename).
$op_trf1(W, W1, Rename) :-
	$change_vars(W, W1, Rename).

%	Pure transformations.
%	_1 and _2 are the input and output expressions respectively.
%	_3 is an assocation list of variables which is passed down.
$pure_trf(B, B1, Rename) :-
	var(B),
	!,
	B1 = call(B2),
	$change_vars(B, B2, Rename).
$pure_trf(B, B1, Rename) :-
	$pure_trf1(B, B1, Rename).

$pure_trf1((V, W), (V1, W1), Rename) :-
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(W, W1, Rename).
$pure_trf1((V ; W), G, Rename) :-
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(W, W1, Rename),
	$trf_disjunct((V1 ; W1), G).
%$pure_trf(call(V), call(V1), Rename) :-	% should this be in?
%	!,
%	$pure_trf(V, V1, Rename).
%$pure_trf(?-(W), ?-(W1), Rename) :-
%	!,
%	$pure_trf(W, W1, Rename).
%$pure_trf(:-(W), :-(W1), Rename) :-
%	!,
%	$pure_trf(W, W1, Rename).
$pure_trf1((H:-W), (H:-W1), Rename) :-
	!,
	$pure_trf(W, W1, Rename).
$pure_trf1(not B, not call(B1), Rename) :-
	var(B),
	!,
	$change_vars(B, B1, Rename).
%$pure_trf1(not (V, W), G, Rename) :-	% eeek! wrong wrong wrong!
%	!,
%	$pure_trf(not V, V1, Rename),
%	$pure_trf(not W, W1, Rename),
%	$trf_disjunct((V1 ; W1), G).
$pure_trf1(not (V ; W), (V1, W1), Rename) :-
	!,
	$pure_trf(not V, V1, Rename),
	$pure_trf(not W, W1, Rename).
$pure_trf1(not (V <= W), (V1, W1), Rename) :-
	!,
	$pure_trf(not V, V1, Rename),
	$pure_trf(W, W1, Rename).
$pure_trf1(not (V => W), (W1, V1), Rename) :-
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(not W, W1, Rename).
$pure_trf1(not (V <=> W), G, Rename) :-
	!,
	$pure_trf((not V, W), G1, Rename),
	$pure_trf((not W, V), G2, Rename),
	$trf_disjunct((G1 ; G2), G).
$pure_trf1(not all(V, W), some(V1, not call(W1)), Rename) :-
	var(W),
	!,
	$change_vars(V.W, V1.W1, Rename).
$pure_trf1(not all(V, X ~= Y), Eq, Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars((V, X = Y), (_, Eq), NewRename).
$pure_trf1(not all(V, W), W1, Rename) :-
	!,
	$pure_trf(some(V, not W), W1, Rename).
$pure_trf1(not (X ~= Y), Eq, Rename) :-
	!,
	$change_vars((X = Y), Eq, Rename).
$pure_trf1((not not W), W2, Rename) :-
	!,
	$pure_trf(W, W1, Rename),
	( nonvar(W1), W1 = (A ; B) ->
		$trf_disjunct((A ; B), W2)
	;	W2 = W1
	).
$pure_trf1(not W, not W1, Rename) :-
	!,
	$pure_trf(W, W1, Rename).
$pure_trf1((V <= W), G, Rename) :-
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(not W, W1, Rename),
	$trf_disjunct((V1 ; W1), G).
$pure_trf1((V => W), G, Rename) :-
	!,
	$pure_trf(not V, V1, Rename),
	$pure_trf(W, W1, Rename),
	$trf_disjunct((V1 ; W1), G).
$pure_trf1((V <=> W), (G1, G2), Rename) :-
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(W, W1, Rename),
	$pure_trf(not V, V2, Rename),
	$pure_trf(not W, W2, Rename),
	$trf_disjunct((V2 ; W1), G1),
	$trf_disjunct((V1 ; W2), G2).
$pure_trf1(some(V, W), some(V1, W1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(solutions(V, W, S), solutions(V1, W1, S1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(min(V, W, S), min(V1, W1, S1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(max(V, W, S), max(V1, W1, S1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(count(V, W, S), count(V1, W1, S1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$change_vars(S, S1, Rename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(sum(E, V, W, S), sum(E1, V1, W1, S1), Rename) :-
	!,
	$add_vars(E-V, Rename, NewRename), 
	$change_vars(E-V, E1-V1, NewRename),
	$change_vars(S, S1, Rename),
	$pure_trf(W, W1, NewRename).
$pure_trf1(all(V, W), not some(V1, not call(W1)), Rename) :-
	var(W),
	!,
	$add_vars(V, Rename, NewRename),
	$change_vars(V.W, V1.W1, NewRename).
$pure_trf1(all(V, not W), not some(V1, W1), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars(V, V1, NewRename),
	$pure_trf(W, W1, Rename).
$pure_trf1(all(V, X ~= Y), all(V1, InEq), Rename) :-
	!,
	$add_vars(V, Rename, NewRename), 
	$change_vars((V, X ~= Y), (V1, InEq), NewRename).
$pure_trf1(all(V, W), not W1, Rename) :-
	!,
	$pure_trf(some(V, not W), W1, Rename).
$pure_trf1((if V then W), (if V1 then W1), Rename) :-
	(	var(V)
	;	V \= some(_,_),
		V \= gSome(_,_)
	),
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(W, W1, Rename).
$pure_trf1((if C then W), (if some(LV1, gSome(GV1, C1)) then W1), Rename) :-
	!,
	$str_some(C, LV, GV, C2),
	$add_vars(LV, Rename, NewRename),
	$change_vars((GV.LV), (GV1.LV1), NewRename),
	$pure_trf(C2, C1, NewRename),
	$pure_trf(W, W1, NewRename).
$pure_trf1((if V then W else X), (if V1 then W1 else X1), Rename) :-
	(	var(V)
	;	V \= some(_,_),
		V \= gSome(_,_)
	),
	!,
	$pure_trf(V, V1, Rename),
	$pure_trf(W, W1, Rename),
	$pure_trf(X, X1, Rename).
$pure_trf1((if C then W else X),
		(if some(LV1, gSome(GV1, C1)) then W1 else X1), Rename) :-
	!,
	$str_some(C, LV, GV, C2),
	$add_vars(LV, Rename, NewRename),
	$change_vars((GV.LV), (GV1.LV1), NewRename),
	$pure_trf(C2, C1, NewRename),
	$pure_trf(W, W1, NewRename),
	$pure_trf(X, X1, Rename).
$pure_trf1(W, W1, Rename) :-
	$change_vars(W, W1, Rename).

%	transform disjunct (V1;W1) into G
$trf_disjunct(V, W) :-
	var(V),
	!,
	V = W.
$trf_disjunct((V1 ; W1), G) :-
	nonvar(V1),
	V1 = (not V2),
	!,
	( nonvar(W1), W1 = (not W2) ->
		$trf_disjunct(V2, V3),
		$trf_disjunct(W2, W3),
		G = not((V3, W3))
	;	$trf_disjunct(V2, V3),
		$trf_disjunct(W1, W2),
		G = (if V3 then W2)
	).
$trf_disjunct((V1 ; W1), (if W3 then V2)) :-
	nonvar(W1),
	W1 = (not W2),
	!,
	$trf_disjunct(V1, V2),
	$trf_disjunct(W2, W3).
$trf_disjunct(V1, V1).

%	Replace each variable in _1 by the variable associated with
%	it in the association list in _3. _2 is the reconstructed
%	expression. Use only the first occurence of _1 in _3 -->
%	renaming of variables in nested or disjoint scopes.
$change_vars(A0, A, Rename) :-
	( Rename == [] ->				% BUG!  Is this silliness any real use?
		A = A0
	; var(A0) ->
		( $is_member(A0 - A, Rename) ->
			true
		;	A = A0
		)
	; const(A0) ->
		A = A0
	;	A0 =.. Func.Args,
		$change_list_vars(Args, NArgs, Rename),
		A =.. Func.NArgs
	).

$change_list_vars([], [], _).
$change_list_vars(A.Args_in, A1.Args_out, Rename) :-
	$change_vars(A, A1, Rename),
	$change_list_vars(Args_in, Args_out, Rename).

%	Add variables in _1 to the association list in _2 to give
%	the association list in _3. Associate each variable with a
%	new variable N.
$add_vars(A, Rename0, Rename) :-
	var(A),
	Rename = (A - _N).Rename0.
$add_vars(A, Rename0, Rename) :-
	const(A),
	Rename = Rename0.
$add_vars(A, Rename0, Rename) :-
	term(A),
	A =.. _.Args,
	$add_list_vars(Args, Rename0, Rename).

$add_list_vars([], Rename, Rename).
$add_list_vars(T.L, Rename, NewRename) :-
	$add_vars(T, Rename, Rename1),
	$add_list_vars(L, Rename1, NewRename).

%	True if a particular variable K occurs in the given
%	association list. The associated variable V is returned.
$is_member(X, K1-V1.L) :-
	X = K - V,
	( K == K1 ->
		V = V1
	;	$is_member(X, L)
	).
