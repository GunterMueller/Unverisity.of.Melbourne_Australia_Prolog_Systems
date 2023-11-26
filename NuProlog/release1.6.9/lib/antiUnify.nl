/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1989 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog anti-unification library.

%	Compute the most specific generalization of TermA and TermB.
antiUnify(TermA, TermB, Term) :-
	antiUnify$antiUnify1(TermA, TermB, Subst0, [], Term),
	keySort(Subst0, Subst1),
	antiUnify$squashSubst(Subst1, _Subst2).

%	Compute the most specific generalization of TermA and TermB and
%	substitutions SubstA and SubstB which yield TermA and TermB when
%	applied to Term.  The substitutions are lists of Var = Term pairs
%	where the Vars are variables occuring in Term.
antiUnify(TermA, TermB, SubstA, SubstB, Term) :-
	antiUnify$antiUnify1(TermA, TermB, Subst0, [], Term),
	keySort(Subst0, Subst1),
	antiUnify$squashSubst(Subst1, Subst2),
	antiUnify$splitSubst(Subst2, SubstA, SubstB).

antiUnify$antiUnify1(TermA, TermB, Subst0, Subst, Term) :-
	( TermA == TermB ->
		Subst0 = Subst,
		Term = TermA
	; nonvar(TermA), nonvar(TermB),
	  functor(TermA, F, N), functor(TermB, F, N) ->
		functor(Term, F, N),
		antiUnify$antiUnify1(N, TermA, TermB, Subst0, Subst, Term)
	; 	Subst0 = (TermA + TermB - Term).Subst
	).

antiUnify$antiUnify1(N, TermA, TermB, Subst0, Subst, Term) :-
	( N > 0 ->
		arg(N, TermA, ArgA),
		arg(N, TermB, ArgB),
		arg(N, Term, ArgU),
		antiUnify$antiUnify1(ArgA, ArgB, Subst0, Subst1, ArgU),
		N1 is N - 1,
		antiUnify$antiUnify1(N1, TermA, TermB, Subst1, Subst, Term)
	;	Subst0 = Subst
	).

?- antiUnify$squashSubst(X, _) when X.
antiUnify$squashSubst([], []).
antiUnify$squashSubst(A0.A, B) :-
	antiUnify$squashSubst(A0, A, B).

?- antiUnify$squashSubst(_, X, _) when X.
antiUnify$squashSubst(A0, [], [A0]).
antiUnify$squashSubst(A0, A1.A, B0) :-
	( A0 = X0 - Var, A1 = X1 - Var, X0 == X1 ->
		antiUnify$squashSubst(A1, A, B0)
	;	B0 = A0.B,
		antiUnify$squashSubst(A1, A, B)
	).

?- antiUnify$splitSubst(X, Y, Z) when X or Y or Z.
antiUnify$splitSubst([], [], []).
antiUnify$splitSubst(
		(TermA + TermB - Var).X, (Var = TermA).A, (Var = TermB).B) :-
	antiUnify$splitSubst(X, A, B).
