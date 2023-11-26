% specify the location of a binary

?- use_if dynamic_loading.
%##?- dload(['LIB/sql.o'],
?- dload(['/mip/jaz/mup/db/sql/sql.o'],
	' ',
	['_p_sql_query',	'_p_sql_end',		'_p_sql_modify',
	'_p_sql_abort',		'_p_sql_next'],
	[$sql_query(_,_,_),	$sql_end(_),		$sql_modify(_,_),
	$sql_abort(_),		$sql_next(_,_)]).
?- use_end.

?-	$unprotect($db_assert(1, 1, 1)),
	$unprotect($db_retract(1, 1, 1)),
	$unprotect($db_retractall(1, 1, 1)),
	$unprotect($db_define(1, 1, 1, 1)),
	$unprotect($db_undefine(1, 1, 1)),
	$unprotect($db_setup(1, 1, 1)).

?- hide([
	construct_tuple(4), pred_to_sql(2), make_sql(4), make_exp(8),
	star_exp(1), remove_bound(4), move_bound_var(5), insert_where(3),
	get_field_defn(3), const_to_sql(3), convert_const(4), is_quoted(1),
	quoted(1), strlen(2), makeblank(2), split_at_word(4), split_at_white(3),
	split_at(4), strip_white(2), white(1), make_list(2), do_transform(2),
	strip_back(2), strip_front(2), anything_to_string(2)]).

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


modify(DB, Exp) :-
	$sql_modify(DB, modify(Exp)).

sequel(DB, Vars, Exp) :-
	sql_retrieve(DB, sequel(Vars, Exp)).

sql_retrieve(D, P) :-		% get answers to a query from external db
	$db_onabort($sql_abort(F)),
	pred_to_sql(P, P1),	% fails if pred is modify.
	$sql_query(D, P1, F),
	repeat,
	$sql_next(F, Q),
	(
		Q = (?-E),
		(
			E = db_end,
			$sql_end(F),
			!,
			fail
		;		 
			E = db_error(Mesg),
			printf("SQL error: %s%c", [Mesg, 10]),
			!,
			fail
		)
	;
		P1 = Q
	).


sql_define(DS, Rname, N, Args) :-		% create new relation
	true.			/* not yet relevant */

sql_undefine(DS, R) :-			% remove relation
	true.			/* not yet relevant */


sql_assert(D, R) :-
	R =.. Pred.Vars,
	Pred ~= sequel, Pred ~= modify,
	(if g_some(Vars, ground(Vars)) then
		field_names(N),
		N =.. Pred.Names,
		construct_tuple(Vars, Pred, Names, Tuple),
		const_str(Pred, P),
		make_list(["insert into ", P, ": <", Tuple, ">"], Tmp),
		Exp =.. [modify, Tmp],
		$sql_modify(D, Exp)
	else
		writeln('Error: assert/1 expression should be ground.')
	).


construct_tuple(V.Vars, Pred, N.Names, Tuple) :-
	field_defn(N, Pred, Type),
	const_to_sql(V, Type, V1),
	(if Vars = [] then
		Tuple = V1
	else
		construct_tuple(Vars, Pred, Names, Tmp),
		make_list([V1, ", ", Tmp], Tuple)
	).


sql_rall(D, P) :-
	P =.. Pred.Vars,
	Pred ~= sequel, Pred ~= modify,
	field_names(N),
	N =.. Pred.Names,
	make_exp(Vars, _, Names, Pred, [], Where, [], _),
	const_str(Pred, Tmp),
	make_list(["delete ", Tmp, " where ", Where], Exp),
	R =.. [modify, Exp],
	$sql_modify(D, R).


sql_retract(D, P) :-
	P =.. Pred.Vars,
	Pred ~= sequel, Pred ~= modify,
	field_names(N),
	N =.. Pred.Names,
	repeat,
	(
		call(P),	/* ground Vars */
		make_exp(Vars, _, Names, Pred, [], Where, [], _),
		const_str(Pred, Tmp),
		make_list(["delete ", Tmp, " where ", Where], Exp),
		R =.. [modify, Exp],
		$sql_modify(D, R)
	;
		!,
		fail
	).


/* On the fly predicate manipulation */

	% Convert P to a predicate of the form
	%	sequel([Vars], "select ... ").
pred_to_sql(P, P1) :-
	(if g_some([P, Vars], P =.. sequel.Vars) then
		P1 = P
	else
		P =.. Pred.Vs,
		Pred ~= modify,
		field_names(N),
		N =.. Pred.Names,
		make_sql(Vs, Names, Pred, P1)
	).


	% If all of _1 is ground, make expression with duplication of first
	% field name between select list and where list. Otherwise, no
	% ground variables in select list.
make_sql(V.Vars, N.Names, Pred, SQL) :-
	(if g_some(V.Vars, ground(V.Vars)) then
		const_str(N, Exp),
		make_exp(V.Vars, _, N.Names, Pred, [], Where, Exp, NExp),
		field_defn(N, Pred, Type),
		const_to_sql(V, Type, V1),
		RVars = [V1]
	else
		make_exp(V.Vars, RVars, N.Names, Pred, [], Where, [], NExp)
	),
	const_str(Pred, Pname),
	make_list(["select ", NExp, " from ", Pname], Tmp),
	(if Where = [] then
		NExp1 = Tmp
	else
		make_list([Tmp, " where ", Where], NExp1)
	),
	SQL =.. [sequel, RVars, NExp1].


make_exp(V.Vars, RVars, N.Names, Pred, Where, NWhere, Exp, NExp) :-
	ground(V),
	!,
	field_defn(N, Pred, Type),
	const_to_sql(V, Type, V1),
	const_str(N, Name),
	make_list([Name, " = ", V1], State),
	(if Where = [] then
		Tmp = State
	else
		make_list([State, " and ", Where], Tmp)
	),
	make_exp(Vars, RVars, Names, Pred, Tmp, NWhere, Exp, NExp).

make_exp(V.Vars, V.RVars, N.Names, Pred, Where, NWhere, Exp, NExp1) :-
	const_str(N, Name),
	(if Exp = [] then
		NExp = Name
	else
		make_list([Exp, ", ", Name], NExp)
	),
	make_exp(Vars, RVars, Names, Pred, Where, NWhere, NExp, NExp1).

make_exp([], [], [], _, Where, Where, Exp, Exp).


/* Code to modify an expression of the form sql(X, "select .. "), so that
	bindings are pushed into the sql expression */

	% Move bound variables in _1 to comparisons with equivalent fields
	% in the sql expression in _2. Do nothing if the sql expression is
	% of the form "select * ... "
sql(DB, Vars, Exp) :-
	(if g_some(Exp, ground(Exp)) then
		strip_white(Exp, Exp2),
		(if star_exp(Exp2) then
			sql_retrieve(DB, sequel(Vars, Exp2))
		else
			remove_bound(Vars, Exp2, NVars, NExp),
			!,
			sql_retrieve(DB, sequel(NVars, NExp))
		)
	else
		writeln('Error: sql expression not ground.')
	).


	% _1 is of the form "select * ... "
star_exp(Exp) :-
	split_at_white(Exp, "select", Tmp),
	strip_white(Tmp, 42._).			% 42 == '*'


	% Remove the bound variables in _1 giving _3. _4 is _2 with an
	% extended "where" clause. If all the variables in _1 are bound,
	% keep the first, so that the select list syntax remains valid.
remove_bound(V.Vars, Exp1, NVars1, NExp1) :-
	split_at_white(Exp1, "select", Exp2),
	strip_white(Exp2, Exp3),
	(if g_some(V.Vars, ground(V.Vars)) then
		split_at_white(Exp3, Field, Tmp),
		strip_white(Tmp, Exp4),
		(if Field = "unique" then
			split_at_white(Exp4, F, _),
			move_bound_var(V.Vars, Exp4, [], [], NExp2)
		else
			F = Field,
			move_bound_var(V.Vars, Exp3, [], [], NExp2)
		),
		get_field_defn(F, _, Type),
		const_to_sql(V, Type, C),
		NVars1 = [C],
		(if Field = "unique" then
			make_list(["unique ", F, " ", NExp2], NExp)
		else
			make_list([F, " ", NExp2], NExp)
		)
	else
		move_bound_var(V.Vars, Exp3, [], NVars, NExp),
		NVars1 = NVars
	),
	append("select ", NExp, NExp1).


	% Each variable is var or non-var. If it is var, put it in new var
	% list, put appropriate argument in new expression. If it is not
	% var, prepend it to `where' list in appropriate form.
move_bound_var(V.Vars, Exp, Where, NVars, NExp) :-
	ground(V),
	!,
	split_at_white(Exp, Field, Tmp1),
	strip_white(Tmp1, Tmp2),
	(if Field = "unique" then
		split_at_white(Tmp2, F2, Tmp3),
		strip_white(Tmp3, Exp2)
	else
		F2 = Field,
		Exp2 = Tmp2
	),
	(if F2 = "from" then
		writeln('Error: more variables than field names.'),
		fail
	else
		get_field_defn(F2, _, Type),
		const_to_sql(V, Type, V1),
		make_list([F2, " = ", V1], State),
		(if Where = [] then
			NWhere = State
		else
			make_list([State, " and ", Where], NWhere)
		),
		move_bound_var(Vars, Exp2, NWhere, NVars, NExp)
	).

	% Fall through - there is a variable in V, or arg/field mismatch
move_bound_var(V.Vars, Exp, Where, V.NVars, NExp1) :-
	split_at_white(Exp, Field, Tmp1),
	strip_white(Tmp1, Tmp2),
	(if Field = "unique" then
		split_at_white(Tmp2, F2, Tmp3),
		strip_white(Tmp3, Exp2)
	else
		F2 = Field,
		Exp2 = Tmp2
	),
	(if F2 = "from" then
		writeln('Error: more variables than field names.'),
		fail
	else
		move_bound_var(Vars, Exp2, Where, NVars, NExp),
		(if g_some(Vars, ground(Vars)) then
			append(F2, " ", F)
		else
			append(F2, ", ", F)
		),
		(if Field = "unique" then
			make_list(["unique ", F, NExp], NExp1)
		else
			append(F, NExp, NExp1)
		)
	).

	% Final case: variable list is exhausted. Insert `where' list
	% into expression to give new expression.
move_bound_var([], Exp, Where, [], NExp) :-
	(if split_at_white(Exp, "from", _) then
		insert_where(Exp, Where, NExp)
	else
		writeln('Error: fewer variables than field names.'),
		fail
	).


	% Insert `where' list, if any, into expression.
insert_where(Exp, [], Exp).

insert_where(Exp, Where, NExp) :-
	(if some([Hd, Tl], split_at_word("where", Exp, Hd, Tl)) then
		make_list([Hd, " ", Where, " and ", Tl], NExp)
	else
		make_list([Exp, " where ", Where], NExp)
	).


get_field_defn(F, Pred, T) :-
	split_at(set(46), F, _, 46.Field),
	const_str(F1, Field),
	field_defn(F1, Pred, T).

get_field_defn(F, Pred, T) :-
	const_str(F1, F),
	field_defn(F1, Pred, T).


	% Convert the given constant into the appropriate format.
	% Strings must be blank-padded.
const_to_sql(C, T, C1) :-
	anything_to_string(C, Const),
	T =.. [Type, Length],
	convert_const(Const, Type, Length, C1).

convert_const(C, date, _, C).

convert_const(C, time, _, C).

convert_const(C, comb, _, C).

convert_const(C, string, _, C) :-
	is_quoted(C).

convert_const(C, string, Len, C1) :-
	~ is_quoted(C),
	strlen(C, N),
	NL is Len - N,
	makeblank(Blanks, NL),
	make_list(["'", C, Blanks, "'"], C1).

convert_const(C, T, _, C) :-
	occurs(T, set(long, numeric, float, amount)),
	~ is_quoted(C).

convert_const(Q.C, T, _, C1) :-
	occurs(T, set(long, numeric, float, amount)),
	is_quoted(Q.C),
	append(C1, "'", C).


is_quoted(39.Str) :-	% 39 == single quote
	quoted(Str).


quoted(39.[]).

quoted(_.Str) :-
	quoted(Str).


strlen([], 0).

strlen(_.Str, N) :-
	strlen(Str, N1),
	N is N1 + 1.


makeblank([], 0).

makeblank(32.Str, N) :-
	N > 0,
	N1 is N - 1,
	makeblank(Str, N1).



	% Split _2 at _1 giving _3 (top half) and _4 (bottom half).
	% _1 is left at the end of _3, and must be a whole word.
split_at_word(_, [], [], []).

split_at_word(Word, Exp, Head, Tail) :-
	split_at(set(9, 10, 13, 32), Exp, W, Tmp1),
	strip_white(Tmp1, Tmp2),
	(if W = Word then
		W = Head,
		Tail = Tmp2
	else
		split_at_word(Word, Tmp2, Head1, Tail),
		make_list([W, " ", Head1], Head)
	).


	% Split _1 at the first white space giving _2 and _3. The
	% white space is left at the front of _3.
split_at_white(List, Head, Tail) :-
	split_at(set(9, 10, 13, 32, 44), List, Head, Tail).


split_at(_, [], [], []).

split_at(Set, El.List, [], El.List) :-
	occurs(El, Set).

split_at(Set, El.List, El.Res, List1) :-
	~ occurs(El, Set),
	split_at(Set, List, Res, List1).


	% Remove any white space from the front of _1, giving _2.
strip_white(X.List1, List2) :-
	white(X),
	strip_white(List1, List2).

strip_white(X.List, X.List) :-
	~ white(X).


	% X is any white space (including comma)
white(X) :-
	occurs(X, set(9, 10, 13, 32, 44)).	% Tab, lf, cr, space, comma


	% _2 is the concatenation of the lists in _1.
make_list([], []).

make_list(L.List, Res) :-
	make_list(List, Tmp),
	append(L, Tmp, Res).

/* cruddy thing called by user to format output  */

transform([], []).

transform(V.Vars, NV.NVars) :-
	do_transform(V, NV),
	transform(Vars, NVars).


do_transform(I, I) :-
	int(I).

do_transform(C.String, Res) :-
	strip_front(C.String, Tmp),
	strip_back(Tmp, Res).


strip_back(L1, L2) :-
	reverse(L1, Tmp1),
	strip_front(Tmp1, Tmp2),
	reverse(Tmp2, L2).


strip_front(39.L1, L2) :-
	strip_white(L1, L2).

strip_front(C.L1, L2) :-
	C ~= 39,
	strip_white(C.L1, L2).


?- lib reverse.

/* end crud */

anything_to_string(V, V1) :-
	atom(V),
	const_str(V, V1).
anything_to_string(V, V1) :-
	int(V),
	str_int(V1, V).
anything_to_string(V.L, V.L) :-
	const_str(_, V.L).

?- lib convert.
?- lib append.
?- lib ground.
?- lib nu_neg.

?- protect([
	sequel(3), $db_assert(3), $db_retract(3), $db_retractall(3),
	$db_define(4), $db_undefine(3), $db_setup(3), sql_retrieve(2),
	sql_define(4), sql_undefine(2), sql_assert(2), sql_rall(2),
	sql_retract(2), sql(3), modify(2), transform(2)]).

?- hidden.
