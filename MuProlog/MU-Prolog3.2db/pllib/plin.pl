% PROLOG formatter and comment remover
% usage:
%	plsave plin.pl		(this creates an "executable" file "plin")
%	plin < file1 > file2	(for Berekely UNIX, at least)
%				(otherwise, use 'prolog plin < file1 > file2')

main(_) :-
	plin.

plin :-
	wflags(22),
	repeat,
	read(T),
	(	eof(T)
	;
		T = (?- op(A, B, C)),
		op(A, B, C),
		fail
	;
		iscurr(T),
		print(T),
		fail
	),
	!.

iscurr((?-wait P)) :-
	!,
	curr(P).
iscurr((?-X)) :-
	!,
	curr((?-X)).
iscurr((H :- T)) :-
	!,
	curr(H).
iscurr(F) :-
	curr(F).

oldcurr(0).

curr(P) :-
	oldcurr(P),
	!.
curr(P) :-
	nl,
	functor(P, F, N),
	functor(C, F, N),
	retract(oldcurr(_)),
	assert(oldcurr(C)),
	!.

portray((?-X)) :-
	portraygoals((?-X)),
	writeln((.)).
portray((H :- T)) :-
	portraycl((H :- T)),
	writeln((.)).
portray(F) :-
	write(F),
	writeln(.).

%##?-consult('LIB/sys/listing.pl').
?-consult('/usr/lib/prolog/sys/listing.pl').	% need to change this
