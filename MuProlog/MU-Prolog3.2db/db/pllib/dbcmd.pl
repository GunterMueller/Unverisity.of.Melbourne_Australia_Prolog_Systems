db_union(X,Y) :-
	call(Y),
	assert(X),
	fail.
db_union(_,_).

db_minus(X,Y) :-
	call(Y),
	retractall(X),
	fail.
db_minus(_,_).

?-protect([db_union(2),db_minus(2)]).
