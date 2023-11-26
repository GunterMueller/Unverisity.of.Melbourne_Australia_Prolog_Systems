
/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/


 a(( $con(F) :- open(F, N, r), repeat, read(N, X), $do(X) 
		, =(X, ?-(end)) , close(N), !),1).

 a(($do(?-(end)) :- !), 1).
 a(($do(?-(X)) :- !, X, !),2).
 a(($do(X) :- assert(X)),3).

 a((true :- true),1).

 a((assert((X:-Y)) :- !, $nassert((X:-Y),N)),1).
 a((assert(X) :- $nassert((X:-true),N)),2).

 a(($start(Y) :-
		$nassert((fail :- true), 1),
		$ret((fail :- true)),
		write(1, 'file to consult? '),
		read(0, X),
		$con(X),
		fail),1).
 a(($start(Y) :- save(Y), argv(X), $main(X)), 2).
