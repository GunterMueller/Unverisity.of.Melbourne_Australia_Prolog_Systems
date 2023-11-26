/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: David Morley
 */

% Nepolog DCG converter.

% DCG processing
%       (including ->/2, ;/2, not/1, and append/1)
%
% All terms of the form X --> Y can be converted to clauses of the form
% H :- B, by calling gexpand(X,Y,H,B)
%
% Each nonterminal symbol has a difference list appended to it to form an atom.
%       verb(V)         becomes         verb(V,TokListIn,TokListOut)
% Goals are passed through unchanged :
%       {writeln(X)}    becomes         writeln(X)
% Terminal symbols are checked using unification either implicitly :
%       verb(V), "x"    becomes         verb(V, TokListIn, 0'x.TokListOut)
% or explicitly :
%       !, "x"          becomes         !, TokListIn=0'x.TokListOut
% It would be possible to do all the terminal symbol checking explicitly,
% but this is usually less efficient:
%       bracketted --> "(", thing, ")".
% would become
%       bracketted(Sin, Sout) :- Sin = 0'(.S1, thing(S1,S2), S2=0').Sout
% which is not as efficient as
%       bracketted(0'(.S1, Sout) :- thing(S1, 0').Sout).
% which can benefit from clause indexing, and which will be more efficient
% at finding solutions to thing.
%
% The choice of which way to do this depends upon the preceeding thing.
% If the preceeding thing is a nonterminal, then calling it with a bound output
% token list will not affect the control flow, and may be more efficient.  If it
% is not a terminal,
% then you have to make sure the binding is done after the thing is
% called, otherwise you may affect the control flow (consider attempting a
% unification which fails before a cut - the cut will not be done).
% A similar problem exists with the meta-level stuff :
%       not X, (X -> Y ; Z), etc.
% The converter converts symbols from right to left.
% As it converts a symbol it records whether the converted version of that
% symbol has its input token list partially bound (due to a string of terminals
% appearing at the start of the symbol).  (returned as arg 4 of con/6,
% value = bound or nbound)
% This is used in the conversion of the symbol to its left (passed in as
% arg 3 of con/6).  The symbol to the left 
% uses the token list as its output token list.
% If it is essential that this binding take place AFTER the call is made,
% for instance when calling cut,
% add_bind_end/5 is used to adding in the appropriate explicit binding,
% after the call.
% If it is essential that this thing not cause a binding of the output
% token list of the preceeding thing, such as inside a disjunction,
% add_bind_start/5 is used to add
% in the appropriate binding before the call.
% Whether or not the current thing causes a binding of the output list of the
% previous thing is returned via the fourth arg of con/6.
%
% Note that append/1 inside a DCG ``body'' can be used to insert something
% into the current string -
% this makes the   a, "a" --> b   form of DCG rule redundant.

?- initializing, $dynamic(termExpansion/2).

expandTerm(X, Y) :-
	termExpansion(X, Y),
	!.
expandTerm(G, (H :- B)) :-
	nonvar(G),
	G = (GH --> GB),
					% Commit to expanding the dcg rule.
	!,				% expandTerm/2 fails if G is not well-formed.
	$catchError(
		dcg$gexpand(GH, GB, H, B),
		expandTerm(G, (H :- B)),
		Error,
		Error
	),
	!.
expandTerm(X, X).

% The goal P cannot be run with its output list bound
% Add an explicit unification if would have been.
dcg$add_bind_end(bound, P, Q, S0, S1) :-
	dcg$combine(P, S0 = S1, Q).
dcg$add_bind_end(nbound, P, P, S0, S0).

% We cannot must not allow any bindings of the previous goal's output token list
% Make any bindings explicitly part of the current goal.
dcg$add_bind_start(bound, P, Q, S0, S1) :-
	dcg$combine((S0 = S1), P, Q).
dcg$add_bind_start(nbound, P, P, S0, S0).


% convert P0 --> Q0 into P :- Q
dcg$gexpand((P0, L), Q0, P, Q) :-	% *** NOW A REDUNDANT FORM ***
	!,
	(\+ dcg$islist(L) ->
		$throwError(format(user_error,
			"~N~w is not a valid head for a DCG rule.~n",
			[(P0, L)]))
	),
	dcg$tag(P0, P, S0, S1),
	dcg$con((Q0, append(L)), Q, bound, _, S0, S1).
dcg$gexpand(P0, Q0, P, Q) :-
	dcg$tag(P0, P, S0, S1),
	dcg$con(Q0, Q, bound, _, S0, S1).

% convert some DCG subexpression into the appropriate goal list
dcg$con(X, _, _, _, _, _) :-	% Variable - naughty, naughty!
	var(X),
	!,
	$throwError(format(user_error,
		"~NVariable ~w unexpected as a non-terminal.~n",
		[X])).
dcg$con(X, P, B1, B0, S0, S1) :-	% Special thing
	dcg$con1(X, P, B1, B0, S0, S1),
	!.
dcg$con(X, P, _, nbound, S0, S1) :-	% Average, run of the mill, nonterminal
	dcg$tag(X, P, S0, S1).

% Append difference list to nonterminal to make it a goal
dcg$tag(X, _, _, _) :-	% variable or number
	(	var(X)
	;	number(X)
	),
	!,
	$throwError(format(user_error,
		"~N~w unexpected as a non-terminal.~n",
		[X])).
dcg$tag(X, _, _, _) :-		% system predicate
	systemPredicate(X),
	format(user_error,
		"~NWarning: ~w is a system predicate.~n\c
			It shouldn't be used as a non-terminal.~n",
		[X]),
	fail.
dcg$tag(X, P, S0, S1) :-	% normal predicate
	X =.. FA,
	append(FA, [S0, S1], FAS),
	P =.. FAS,
	!.
dcg$tag(X, _, _, _) :-		%unknown
	$throwError(format(user_error,
		"~N~w unexpectedly unexpected.~n",
		[X])).

% Perform special (meta-level) conversion
dcg$con1(X.L, true, _, bound, X.S0, S1) :-		% Terminal list
	(dcg$islist(L)
	->	append(L, S1, S0)
	;	$throwError(format(user_error,
			"~NGroup of terminal symbols ~w is not a list.~n",
			[X.L]))
	).
dcg$con1([], true, B, B, S0, S0).			% Empty terminal list
dcg$con1(=(X), true, B, B, X, X).			% mark place
dcg$con1((Xa, Xb), Q, B2, B0, S0, S2) :-		% Conjunction
	dcg$con(Xb, Pb, B2, B1, S1, S2),
	dcg$con(Xa, Pa, B1, B0, S0, S1),
	dcg$combine(Pa, Pb, Q).
dcg$con1({X}, P, B1, nbound, S0, S1) :-		% Goal
	dcg$add_bind_end(B1, X, P, S0, S1).
dcg$con1(!, P, B1, nbound, S0, S1) :-		% Cut
	dcg$add_bind_end(B1, !, P, S0, S1).
dcg$con1((Xa -> Xb; Xc), (Pa -> Pb; Pc), _B2, nbound, S0, S2) :-	% -> ;
	!,				% must not bind with -> or ;
	dcg$con_nbs(Xc, Pc, bound, S0, S2),
	dcg$con_nbs(Xb, Pb, bound, S1, S2),
	dcg$con_nbs(Xa, Pa, nbound, S0, S1).
dcg$con1((Xa; Xb), (Pa; Pb), _B1, nbound, S0, S1) :-	% Disjunction
	dcg$con_nbs(Xa, Pa, bound, S0, S1),
	dcg$con_nbs(Xb, Pb, bound, S0, S1).
dcg$con1((Xa -> Xb), (Pa -> Pb; S0 = S2), B2, nbound, S0, S2) :-	% ->
	dcg$con_nbs(Xb, Pb, B2, S1, S2),
	dcg$con_nbs(Xa, Pa, nbound, S0, S1).
dcg$con1((\+ X), P, B1, nbound, S0, S2) :-		% Negation
	dcg$con_nbs(X, P1, nbound, S0, _),		% cannot expand S0 to X+anything
	dcg$add_bind_end(B1, (\+ P1), P, S0, S2).	% don't pass bindings into \+
%dcg$con1(can(X), P, B1, nbound, S0, S2) :-		% can(X) is \+(\+(X)) sort of
%	dcg$con_nbs(X, P1, nbound, S0, _),
%	dcg$add_bind_end(B1, P1, P, S0, S2).
dcg$con1(append(L), P, B, B, S0, S1) :-		% Insert terminal list
	(	(	dcg$islist(L),
			append(L, S0, S1)
		)
	->	P=true						% optimise if possible
	;	P=append(L,S0,S1)			% or use append/3
	).
dcg$con1(X, A, B, C, D, E) :-
	dcg$samething(X, Y),
	!,
	dcg$con1(Y, A, B, C, D, E).

dcg$samething(not(X), (\+ X)).
dcg$samething((if X then Y else Z), (X -> Y ; Z)).
dcg$samething((if X then Y), (X -> Y)).

% Convert a subexpression, ensuring that it doesn't pass on terminals to 
% bind its pedecessor's output token list.
dcg$con_nbs(X, P, B2, S0, S2) :-
	dcg$con(X, P1, B2, B1, S1, S2),
	dcg$add_bind_start(B1, P1, P, S0, S1).

% islist(X) succeeds iff X is currently a []-terminated list
dcg$islist(X) :-
	var(X),
	!,
	fail.
dcg$islist([]).
dcg$islist(_.Y) :-
	dcg$islist(Y).

% Form the simplest conjunction of two goals
dcg$combine(true, X, X) :-
	!.
dcg$combine(X, true, X) :-
	!.
dcg$combine((X, Y), Z, (X, A)) :-
	!,
	dcg$combine(Y, Z, A).
dcg$combine(X, Y, (X, Y)).

phrase(P, L) :-
	dcg$gexpand(a, P, a(L, []), G),
	call(G).
/*
phrase(P, L) :-
	P =.. PL,
	append(PL, [L, []], GL),
	G =.. GL,
	$funcall(G).
*/
