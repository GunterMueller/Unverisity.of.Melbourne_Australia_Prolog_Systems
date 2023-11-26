%
%	Copyright (C) 1986, The University of Melbourne
%	Written by Justin Zobel, October 1986
%

?- use_if dynamic_loading.
%##?- dload(['LIB/dload/sql.o'],
?- dload(['/v/staff/jaz/sql/sql.o'],
	' ',
	['_p_sql_query',	'_p_sql_end',		'_p_sql_modify',
	'_p_sql_abort',		'_p_sql_next'],
	[$sql_query(_,_,_),	$sql_end(_),		$sql_modify(_,_),
	$sql_abort(_),		$sql_next(_,_)]).
?- use_end.

?- use_if muprolog.
?-	$unprotect($db_assert(1, 1, 1)),
	$unprotect($db_retract(1, 1, 1)),
	$unprotect($db_retractall(1, 1, 1)),
	$unprotect($db_define(1, 1, 1, 1)),
	$unprotect($db_undefine(1, 1, 1)),
	$unprotect($db_setup(1, 1, 1)).

?- hide([do_sql_retrieve(2), remove_bound(4), move_bound_var(6), strip_back(2),
	strip_front(2), list_names(2), list_where(2), attach_names_list(4),
	find_select_name(4), transform(2), insert_where(3), struct_to_sql(2),
	make_binds(6), attach_names(4), make_whole_struct(3), make_struct(5),
	find_binds(7), type_vars(4), const_to_sql(3), convert_const(3),
	is_quoted(1), makeblank(2), split_at_word(4), split_at_white(3),
	split_at(4), make_list(2), anything_to_string(2), strip_white(2),
	quoted(1), white(1), put_error(1)]).
?- use_end.

?- assert(($db_assert(D, R, sql) :-
	sql_assert(D, R))).

?- assert(($db_retract(D, R, sql) :-
	sql_retract(D, R))).

?- assert(($db_retractall(D, R, sql) :-
	sql_rall(D, R))).

?- assert(($db_define(D, R, N, sql(Args)) :-
	sql_define(D, R, N, Args))).

?- assert(($db_undefine(DS, R, sql) :-
	sql_undefine(DS, R))).

?- assert(($db_setup(D, R, sql):-
	assert((R :- sql_retrieve(D, R))))).

%-- access -- Top-level interface.
% Predicates to retrieve and modify database.

access(DB, Preds) :-
	preds_to_sql(Preds, SQL),
	do_sql_retrieve(DB, SQL).

	% Use the given SQL expression to modify the database.
modify(DB, Exp) :-
	$sql_modify(DB, Exp).

	% Use the given SQL expression to access the interface.
sequel(DB, Vars, Exp) :-
	do_sql_retrieve(DB, sequel(Vars, Exp)).

	% Optimise the given SQL expression before using it to retrieve
	% information from the database.
	% Bound variables in _1 are changed to comparisons with appropriate
	% fields in the SQL expression in _2. No change is made if the sql
	% expression is of the form "select * ... ".
sql(DB, Vars, Exp) :-
	(ground(Exp) ->
		strip_white(Exp, Exp2),
		(split_at_white(Exp, "select", 42._) ->
				% exp is of the form "select * ... "
			do_sql_retrieve(DB, sequel(Vars, Exp2))
		;
			once(remove_bound(Vars, Exp2, NVars, NExp)),
			do_sql_retrieve(DB, sequel(NVars, NExp))
		)
	;
		put_error('SQL expression not ground.')
	).

	% Get information for the named predicate from the database. It must
	% first be converted to an SQL expression.
sql_retrieve(DB, P) :-
	pred_to_sql(P, SQL),
	do_sql_retrieve(DB, SQL).

	% Create new predicate.
sql_define(_DB, _PName, _N, _Args) :-
	true.

	% Remove predicate.
sql_undefine(_DB, _P) :-
	true.

	% Assert given predicate into database.
sql_assert(DB, P) :-
	P =.. Func.Vars,
	(ground(Vars) ->
		field_names(Func, Names),
		const_str(Func, PName),
		type_vars(Vars, PName, Names, VarList),
		list_names(VarList, Tuple),
		make_list(["insert into ", PName, ": <", Tuple, ">"], Exp),
		$sql_modify(DB, Exp)
	;
		put_error('assert/1 expression should be ground.')
	).

	% Retract all tuples from database that match P.
sql_rall(DB, P) :-
	P =.. Func.Args,
	field_names(Func, Names),
	const_str(Func, PName),
	find_binds(Args, Names, _, PName, _, BindList),
	list_where(BindList, Bind),
	make_list(["delete ", PName, " where ", Bind], Exp),
	$sql_modify(DB, Exp).

	% Retract the first tuple from the database that matches P. UNIFY
	% does the equivalent of a retractall, so the predicate must first
	% be ground.
sql_retract(DB, P) :-
	P =.. Func.Args,
	field_names(Func, Names),
	repeat,
	(
		(ground(P) ->
			true		/* one solution only */
		;
			call(P)		/* ground Vars */
		),
		const_str(Func, PName),
		find_binds(Args, Names, [], PName, [], BindList),
		list_where(BindList, Bind),
		make_list(["delete ", PName, " where ", Bind], Exp),
		$sql_modify(DB, Exp)
	;
		!,
		fail
	).

% end -- access --


%-- retrieve -- Loop for retrieving a series of answers from SQL database.

	% Get answers to a query from the database.
do_sql_retrieve(DB, sequel(Vars, Query)) :-
	$db_onabort($sql_abort(F)),
	$sql_query(DB, 32.Query, F),
	repeat,
	$sql_next(F, Answer),
	(
		Answer = (?- E),
		(
			E = db_end,
			$sql_end(F),
			!,
			fail
		;		 
			E = db_error(Mesg),
			printf("UNIFY error: %s%c", [Mesg, 10]),
			!,
			fail
		)
	;
		sequel(Vars, Query) = Answer
	).

% end -- retrieve --


%-- sql-opt -- Modify SQL expression to push bindings.

	% Remove the bound variables in _1 giving _3. _4 is _2 with an
	% extended "where" clause. If all the variables in _1 are bound,
	% keep the first, so that the select list syntax remains valid.
remove_bound(A.Args, Exp, NVars, NewExp) :-
	split_at_white(Exp, "select", Exp1),
	(ground(A.Args) ->
		move_bound_var(A.Args, Exp1, [], [], Tmp, (F;"=";B)),
		NVars = [B],
		append(F, 32.Tmp, NExp)
	;
		move_bound_var(A.Args, Exp1, [], NVars, NExp, _)
	),
	append("select ", NExp, NewExp).

	% Each argument is ground or non-ground. If it is ground, put it in
	% the var list, put appropriate argument in new expression. If it is
	% not ground, prepend it to `where' list in appropriate form.
move_bound_var(A.Args, Exp, Bind, NVars, NExp, (NewF;"=";A1)) :-
	ground(A),
	split_at_white(Exp, Field, Tmp),
	(Field = "unique" ->
		split_at_white(Tmp, F, Exp1),
		append("unique ", F, NewF)
	;
		F = Field, NewF = F, Exp1 = Tmp
	),
	(F = "from" -> put_error('more variables than field names.'), fail),
	(append(PName, 46.FName, F) ->
		field_defn(FName, PName, Type)
	;
		field_defn(F, _, Type)
	),
	const_to_sql(A, Type, A1),
	move_bound_var(Args, Exp1, (F;"=";A1).Bind, NVars, NExp, _).
move_bound_var(A.Args, Exp, Bind, A.NVars, NExp1, false) :-
	\+ ground(A),
	split_at_white(Exp, Field, Tmp),
	(Field = "unique" ->
		split_at_white(Tmp, F, Exp1),
		make_list(["unique ", F1, NExp], NExp1)
	;
		F = Field, Exp1 = Tmp,
		append(F1, NExp, NExp1)
	),
	(F = "from" -> put_error('more variables than field names.'), fail),
	move_bound_var(Args, Exp1, Bind, NVars, NExp, _),
	(ground(Args) ->
		append(F, " ", F1)
	;
		append(F, ", ", F1)
	).
		% Final case: variable list is exhausted. Insert `where' list
		% into expression to give new expression.
move_bound_var([], Exp, BindList, [], NExp, false) :-
	list_where(BindList, Bind),
	(split_at_white(Exp, "from", _) ->
		insert_where(Exp, Bind, NExp)
	;
		put_error('fewer variables than field names.'), fail
	).

	% Insert `where' list, if any, into expression.
insert_where(Exp, [], Exp).
insert_where(Exp, Where, NExp) :-
	(split_at_word("where", Exp, Head, Tail) ->
		make_list([Head, 32.Where, " and ", Tail], NExp)
	;
		make_list([Exp, " where ", Where], NExp)
	).

% end -- sql-opt --


%-- pred_to_sql -- Convert a predicate or a list of predicates to SQL.

	% Convert P to a predicate of the form
	%	sequel([Vars], "select ... ").
pred_to_sql(P, SQL) :-
	P =.. Func.Vs,
	field_names(Func, Names),
	make_struct(Func, Vs, Names, Struct, []),
	struct_to_sql(Struct, SQL).

	% Convert the list of predicates into a single pred of the form
	%	sequel([Vars], "select ... ").
preds_to_sql(Preds, SQL) :-
	make_whole_struct(Preds, Struct, "xYzZy"),	% Nonsense prefix.
	struct_to_sql(Struct, SQL).

	% Convert each element of the list of predicates into a struct
	% as described below, and compose structs. The last argument is
	% a prefix which is used to distinguish separate occurences of
	% the same relation.
make_whole_struct([], ([], [], [], []), _).
make_whole_struct(P.Preds, (NF, NV, NS, NB), Prefix) :-
	make_whole_struct(Preds, (F, V, S, B), 97.Prefix),
	P =.. Func.Vs,
	(
		Vs = AL.AR.[],
		occurs(Func, set((<), (=<), (=), (>=), (>))),
		!,
		(Func = (=<) ->
			Oper = "<="
		;
			const_str(Func, Oper)
		),
		NF = F, NV = V, NS = S, NB = (AL;Oper;AR).B
	;
		field_names(Func, Names),
		!,
		make_struct(Func, Vs, Names, ([PName],Vars,Sel,Bind), Prefix),
		NF = PName.F,
		append(Vars, V, NV),
		append(Sel, S, NS),
		append(Bind, B, NB)
	;
		functor(P, Func, A),
		printf("Error: %s/%d not database predicate", Func, A),
		printf(stderr, ": being ignored.%c", [10]),
		NF = F, NV = V, NS = S, NB = B
	).

	% Make struct of four lists - name of predicate, variables to be bound
	% by query, field names corresponding to these variables, and the
	% other field names against their bindings. If _2 contains '_' only,
	% _4._2 and _4._3 duplicate the first element of _4._4.
make_struct(Func, A.Args, N.Names, ([PNameOut], Vars, Select, Bind), Prefix) :-
	const_str(Func, PName),
	(Prefix = [] ->
		PName = PNameOut, PName = EPName
	;
		append(Prefix, PName, EPName),
		append(PName, 32.EPName, PNameOut)
	),
	(ground(A.Args) ->
		find_binds(A.Args, N.Names, [], PName, EPName, [],
						(Name;"=";B).Binds),
		Vars = [_], Select = [Name], Bind = (Name;"=";B).Binds
	;
		find_binds(A.Args, N.Names, Vars, PName, EPName, Select, Bind)
	).

% end -- pred_to_sql --


%-- convert_style -- Convert from structure to SQL query.

	% Each element of _1 is moved to _6 if it is ground, and _4
	% otherwise. Its binding is converted to the appropriate type
	% if it is ground.
find_binds(A.Args, N.Names, Vars, PName, EPName, Select, (Name;"=";A1).Bind) :-
	ground(A),
	field_defn(N, PName, Type),
	const_to_sql(A, Type, A1),
	append(EPName, 46.N, Name),
	find_binds(Args, Names, Vars, PName, EPName, Select, Bind).
find_binds(A.Args, N.Names, A.Vars, PName, EPName, Name.Select, Bind) :-
	var(A),
	append(EPName, 46.N, Name),
	find_binds(Args, Names, Vars, PName, EPName, Select, Bind).
find_binds([], [], [], _, _, [], []).

	% Convert the given structure to an expression of the form
	%	sequel([Vars], "select ... ").
struct_to_sql((PNameList, Vars, SelectList, BindList), sequel(NVars, Exp)) :-
	make_binds(Vars, NVars, SelectList, NSelectList, BindList, NBindList),
	attach_names_list(NVars, NSelectList, NBindList, NewBindList),
	list_names(PNameList, PNames),
	list_names(NSelectList, Select),
	list_where(NewBindList, Bind),
	(Bind = [] ->
		make_list(["select ", Select, " from ", PNames], Exp)
	;
		make_list(["select ",Select," from ",PNames," where ",Bind],Exp)
	).

	% If any element of _1 is duplicated in _1, remove one occurence and
	% add a new binding between the appropriate field names.
make_binds(V.Vars, V.NVars, S.Select, S.NSelect, Bind, NBind) :-
	\+ occurs(V, Vars),
	make_binds(Vars, NVars, Select, NSelect, Bind, NBind).
make_binds(V.Vars, NVars, S.Select, NSelect, Bind, (S;"=";Name).NBind) :-
	occurs(V, Vars),
	find_select_name(V, Vars, Select, Name),
	make_binds(Vars, NVars, Select, NSelect, Bind, NBind).
make_binds([], [], [], [], Bind, Bind).

	% Attach a name to each variable in _3, where _1 is a list of variables
	% and _2 is an associated list of SQL field names.  It is an error for
	% a variable not to have a field name.
attach_names(Vs, Ss, Var, Name) :-
	var(Var),
	find_select_name(Var, Vs, Ss, Name),
	!.
attach_names(_, _, Int, List) :-
	atom(Int),
	int_const(_, Int),
	!,
	const_str(Int, List).
attach_names(_, _, Int, List) :-
	int(Int),
	!,
	str_int(List, Int).
attach_names(_, _, Var, _) :-
	(var(Var) ; atom(Var)),
	!,
	put_error('relational expression has atom or variable'),
	fail.
attach_names(_, _, C.Name, C.Name) :-
	C >= 1, C =< 126,
	!.
attach_names(Vs, Ss, Exp, NewExp) :-
	Exp =.. Func.Args,
	attach_names_list(Vs, Ss, Args, NewArgs),
	(
		occurs(Func, ((*), (/), (+), (-))),
		NewArgs = AL.AR.[],
		!,
		const_str(Func, PName),
		make_list([AL, 32.PName, 32.AR], NewExp)
	;
		NewExp =.. Func.NewArgs
	).

attach_names_list(_, _, [], []).
attach_names_list(Vs, Ss, A.Arg, NA.NewArg) :-
	attach_names(Vs, Ss, A, NA),
	attach_names_list(Vs, Ss, Arg, NewArg).

	% Find the field name corresponding to the duplicated variable.
find_select_name(V, V1._, Name._, Name) :-
	V == V1.
find_select_name(V, V1.Vars, _.Select, Name) :-
	V \== V1,
	find_select_name(V, Vars, Select, Name).

% end -- convert_style --


%-- transform --

	% Called by user to format output
transform([], []).
transform(V.Vars, NV.NVars) :-
	(int(V) ->
		NV = V
	;
		strip_front(V, Tmp),
		strip_back(Tmp, NV)
	),
	transform(Vars, NVars).

strip_back(L1, L2) :-
	reverse(L1, Tmp1),
	strip_front(Tmp1, Tmp2),
	reverse(Tmp2, L2).

strip_front(39.L1, L2) :-
	strip_white(L1, L2).
strip_front(C.L1, L2) :-
	C ~= 39,
	strip_white(C.L1, L2).

% end -- transform --


% Low level code

%-- list_names --

	% Make the list of lists in _1 into a single list with comma separators.
list_names([Name], Name).
list_names(N.N1.Names, List) :-
	list_names(N1.Names, IsList),
	append(N, 44.32.IsList, List).

	% Make the list of lists in _1 into a single list with assignments
	% and "and" separators.
list_where([], []).
list_where((Name;Func;Bind).[], Assign) :-
	make_list([Name, 32.Func, 32.Bind], Assign).
list_where((Name;Func;Bind).N.Names, List) :-
	list_where(N.Names, IsList),
	make_list([Name, 32.Func, 32.Bind, " and ", IsList], List).

% end -- list_names


%-- const_type --

	% _4 is each element of _1 modified to be of the appropriate type.
	% _2 is the predicate name, _3 is the variable names.
type_vars([], _, [], []).
type_vars(A.Args, PName, N.Names, A1.Vars) :-
	field_defn(N, PName, Type),
	const_to_sql(A, Type, A1),
	type_vars(Args, PName, Names, Vars).

	% Convert the given constant into the appropriate format.
	% Strings must be blank-padded.
const_to_sql(C, Type, C1) :-
	anything_to_string(C, Const),
	convert_const(Const, Type, C1).

convert_const(C, date-_, C).
convert_const(C, time-_, C).
convert_const(C, comb-_, C).
convert_const(C, string-_, C) :-
	is_quoted(C).
convert_const(C, string-Len, C1) :-
	\+ is_quoted(C),
	length(C, N),
	NL is Len - N,
	makeblank(Blanks, NL),
	make_list(["'", C, Blanks, "'"], C1).
convert_const(C, T-_, C) :-
	occurs(T, (long, integer, numeric, float, amount)),
	\+ is_quoted(C).
convert_const(Q.C, T-_, C1) :-
	occurs(T, (long, integer, numeric, float, amount)),
	is_quoted(Q.C),
	append(C1, "'", C).

% end -- const_type --


%-- strings --

is_quoted(39.Str) :-	% 39 == single quote
	quoted(Str).

quoted(39.[]).
quoted(_.Str) :-
	quoted(Str).

	% _1 is a list of _2 spaces.
makeblank([], 0).
makeblank(32.Str, N) :-
	N > 0,
	N1 is N - 1,
	makeblank(Str, N1).

	% Split _2 at the white-space delimited word in _1, giving _3
	% (top half) and _4 (bottom half). _1 is left at the end of _3.
split_at_word(_, [], [], []).
split_at_word(Word, Exp, Head, Tail) :-
	split_at(set(9, 10, 13, 32), Exp, W, Tmp1),
	strip_white(Tmp1, Tmp2),
	(W = Word ->
		W = Head, Tail = Tmp2
	;
		split_at_word(Word, Tmp2, Head1, Tail),
		append(W, 32.Head1, Head)
	).

	% Split _1 at the first white space giving _2 and _3. The
	% white space is left at the front of _3.
split_at_white(List, Head, NTail) :-
	split_at(set(9, 10, 13, 32, 44), List, Head, Tail),
	strip_white(Tail, NTail).

	% Split _2 at the first element of _1 encountered. This element is
	% left at the front of _4 (tail). _3 is the head of _2.
split_at(_, [], [], []).
split_at(Set, El.List, [], El.List) :-
	occurs(El, Set).
split_at(Set, El.List, El.Res, List1) :-
	\+ occurs(El, Set),
	split_at(Set, List, Res, List1).

	% _2 is the concatenation of the lists in _1.
make_list([], []).
make_list(L.List, NewList) :-
	make_list(List, Tmp),
	append(L, Tmp, NewList).

anything_to_string(V, V1) :-
	atom(V),
	const_str(V, V1).
anything_to_string(V, V1) :-
	int(V),
	str_int(V1, V).
anything_to_string(V.L, V.L) :-
	V >= 1, V =< 126.

	% Remove any white space from the front of _1, giving _2.
strip_white(X.List1, List2) :-
	white(X),
	strip_white(List1, List2).
strip_white(X.List, X.List) :-
	\+ white(X).

	% X is any white space (including comma)
white(X) :-
	occurs(X, set(9, 10, 13, 32, 44)).	% Tab, lf, cr, space, comma

% end -- strings --


put_error(Mesg) :-
	write(user_error, 'Error: '), writeln(user_error, Mesg).

?- use_if muprolog.
?- lib convert, lib lists, lib ground.
?- hidden.
?- protect([access(2), modify(2), sequel(3), sql(3), sql_retrieve(2),
	sql_define(4), sql_undefine(2), sql_assert(2), sql_rall(2),
	sql_retract(2), pred_to_sql(2), preds_to_sql(2), $db_assert(3),
	$db_retract(3), $db_retractall(3), $db_define(4), $db_undefine(3),
	$db_setup(3)]).
?- use_end.
