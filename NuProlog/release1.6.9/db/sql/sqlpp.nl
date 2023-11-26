%
%	sqlpp
%
%	Transform all sql database predicates in program into "sql/3"
%	format.
%
%	Author: J. Zobel, November 1986
%

%-- main --

usage :-
	writeln('Usage : sqlpp file ...').

main(_.File.Args) :-
	(if File = ('-') then
		assert(input(user))
	else
		assert(input(File))	% this will be backtracked on.
	),
	main(_.Args).
main(_.[]) :-
	(\+ input(X) ->
		assert(input(user))
	),
	sqlpp,
	exit(0).
main(_) :-
	usage,
	exit(1).

	% Read clauses, unfold them, write them out.
sqlpp :-
	read_in,		% Read clauses from file F.
	change_calls,		% Transform clauses.
	write_procs.		% Write new program to file G.

% end -- main --


%-- read_in -- Read and assert clauses from file F.

read_in :-
	input(F),
	read_in(F),
	fail.
read_in.

read_in(F) :-
	(F = user ->
		F = S
	;	open(F, read, S)),
	repeat,
	read(S, T),
	(	eof(T)
	;	once(process(T)),
		fail
	),
	(S \= user -> close(S)),
	!.

% end -- read_in --

%-- process -- Process clauses and goals, put them in database "proc".

	% Process a clause.
process((?- G)) :-
	process_goal(G).
process(_) :-
	dont_use.
process((H :- T)) :-
	assert(proc((H :- T))),
	generalize(H, H1),
	note(is_proc(H1)).
process(H) :-
	assert(proc((H :- true))),
	generalize(H, H1),
	note(is_proc(H1)).

	% Process a goal.
process_goal((useIf X)) :-
	(	dont_use,
		assert(nested_use)	% Take care of nesting.
	;	call(X)
	;	note(dont_use)
	).
process_goal(useElse) :-		% Toggle dont_use if not nested.
	(	nested_use
	;	retract(dont_use)
	;	note(dont_use)
	).
process_goal(useEnd) :-
	(	retract(nested_use)
	;	retract(dont_use)
	).
process_goal(_) :-
	dont_use.
process_goal(db_cons(DB)) :-
	atomToString(DB, Path),
	append(Path, "/.con", ConPath),
	atomToString(Con, ConPath),
	do_consult(DB, Con),
	append(Path, "/.rules.nl", RulesPath),
	atomToString(Rules, RulesPath),
	do_consult(DB, Rules),
	assert(is_proc($goal((?- db_cons(DB))))).
process_goal(op(X, Y, Z)) :-
	assert(is_proc($goal((?- op(X, Y, Z))))),
	op(X, Y, Z).
process_goal(X.Y) :-
	read_in_list(X.Y).
process_goal(G) :-
	assert(is_proc($goal((?- G)))).

	% Consult a list of files.
read_in_list([]).
read_in_list(X.Y) :-
	read_in(X),
	read_in_list(Y).

% end -- process --


%-- change_calls -- Change each call to "sql/3" format.

change_calls :-
	retract(proc((H :- B))),
	once((
		change_call(B, B1),
		assert(new_proc((H :- B1)))
	)),
	fail.
change_calls.

	% Break up the body of the clause to the level of individual calls,
	% then reconstruct with each call unfolded.
change_call(B, B) :-
	var(B).
change_call((A => B), (ANew => BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((A <= B), (ANew <= BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((A <=> B), (ANew <=> BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call(not A, not ANew) :-
	change_call(A, ANew).
change_call((A, B), (ANew, BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((A ; B), (ANew ; BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((if A then B), (if ANew then BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((if A then B else C), (if ANew then BNew else CNew)) :-
	change_call(A, ANew), change_call(B, BNew), change_call(C, CNew).
change_call((A -> B), (ANew -> BNew)) :-
	change_call(A, ANew), change_call(B, BNew).
change_call((A -> B ; C), (ANew -> BNew ; CNew)) :-
	change_call(A, ANew), change_call(B, BNew), change_call(C, CNew).
change_call(call(A), call(ANew)) :-
	change_call(A, ANew).
change_call(once(A), once(ANew)) :-
	change_call(A, ANew).
change_call(\+ A, \+ ANew) :-
	change_call(A, ANew).
change_call(some(Vars, A), some(Vars, ANew)) :-
	change_call(A, ANew).
change_call(g_some(Vars, A), g_some(Vars, ANew)) :-
	change_call(A, ANew).
change_call(all(Vars, A), all(Vars, ANew)) :-
	change_call(A, ANew).
change_call(g_all(Vars, A), g_all(Vars, ANew)) :-
	change_call(A, ANew).
change_call(access(DB, Preds), sql(DB, Vars, Query)) :-
	preds_to_sql(Preds, sequel(Vars, Query)).
change_call(Pred, sql(DB, Vars, Query)) :-
	relation(DB, Pred, sql),
	pred_to_sql(Pred, sequel(Vars, Query)).
change_call(Pred, Pred).

% end -- change_calls --

%-- low_level --

	% Write all procedures to standard output.
write_procs :-
	is_proc(P),
	(P = $goal(G) ->
		writeq(G),
		writeln(.)
	;	new_proc((P :- B)),
		portraycl((P :- B)),
		fail
	),
	fail.
write_procs.

note(X) :-
	call(X), !.
note(X) :-
	assert(X).

	% Find the most general form of atom A.
generalize(A, A1) :-
	g_some([A, A1, H, N], (
		functor(A, H, N),
		functor(A1, H, N)
	)).

do_consult(DB, File) :-
	open(File, read, S),
	repeat,
	read(S, T),
	(	nonvar(T),
		eof(T)
	;	(T = relation(P, Type) ->
			assert(relation(DB, P, Type))
		;	assert(T)
		),
		fail
	),
	close(S),
	!.

% end -- low_level --

%-- database --

	% Database clauses.
?- dynamic(relation/3).
?- dynamic(field_names/2).
?- dynamic(field_defn/3).

	% Procedure names are asserted here.
?- dynamic(is_proc/1).

	% Clauses are asserted here.
?- dynamic(proc/1).
?- dynamic(new_proc/1).

	% Facts to take care of useIf.
?- dynamic(dont_use/0).
?- dynamic(nested_use/0).

?- dynamic(input/1).

% end -- database --
