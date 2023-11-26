/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog IO predicates.

printf(Format, Args) :-
	currentOutput(Stream),
	printf(Stream, Format, Args).

printf(Stream, Format, Args) :-
	( $nameToString(Format, FormatString) ->
		true
	;	format(user_error,
			"~NError in ~w -- format not a string or atom.~n",
			[printf(Stream, Format, Args)]),
		fail
	),
	$printf(Stream, FormatString, Args).

skip(N) :-
	currentInput(S),
	skip(S, N).

skip(S, N) :-
	integer(N),
	!,
	repeat,
	get0(S, M),
	M =:= N or M =:= -1,
	!,
	M \== -1.
skip(S, N) :-
	repeat,
	get0(S, M),
	( M \== -1 ->
		member(M, N)
	),
	!,
	M \== -1.

tab(N) :-
	currentOutput(S),
	tab(S, N).

tab(S, T) :-
	N is T,
	integer(N),
	$tab(S, N).

$tab(S, T) :-
	( T > 0 ->
		N is T - 1,
		put(S, 32),
		$tab(S, N)
	;	T = 0			% Tidiness.
	).

eof(end_of_file).

isEof(T) :-
	T == end_of_file.

getl(Line) :-
	currentInput(Stream),
	getl(Stream, Line).

getl(Stream, Line) :-
	get0(Stream, C),
	( C == -1 ->
		Line = []
	; C == 0'\n ->
		Line = [0'\n]
	;	Line = C.LineT,
		getl(Stream, LineT)
	).

getToken(Token, Type) :-
	currentInput(Stream),
	getToken(Stream, Token, Type).

%	Get a list of (Token.Type) pairs up to end of term or eof.
getTokenList(TokenList) :-
	currentInput(Stream),
	getTokenList(Stream, TokenList).

getTokenList(Stream, TokenList) :-
	getToken(Stream, Token, Type),
	$getTokenList(Stream, Token, Type, TokenList).

:- $getTokenList(_, X, _, _) when X.
$getTokenList(_, '. ', end, []) :-
	!.
$getTokenList(_, '. ', atom, []) :-
	!.
$getTokenList(_, [], end_of_file, [([].end_of_file)]) :-
	!.
$getTokenList(Stream, Token, Type, (Token.Type).TokenList) :-
	getTokenList(Stream, TokenList).

%	Read and readTerm keep trying after syntax errors.
read(X) :-
	repeat,
		parser$r(Term, _),
	!,
	X = Term.

read(S, X) :-
	repeat,
		parser$r(S, Term, _),
	!,
	X = Term.

%	Fail on syntax error.
read1(X) :-
	parser$r(X, _).

read1(S, X) :-
	parser$r(S, X, _).

%	Read a term and return lists of the named vars and corresponding variables.
readTerm(X, VarNames, Vars) :-
	repeat,
		parser$r(Term, VarNames.Vars),
	!,
	X = Term.

readTerm(S, X, VarNames, Vars) :-
	repeat,
		parser$r(S, Term, VarNames.Vars),
	!,
	X = Term.

%	Fail on syntax error.
read1Term(X, VarNames, Vars) :-
	parser$r(X, VarNames.Vars).

read1Term(S, X, VarNames, Vars) :-
	parser$r(S, X, VarNames.Vars).

%	Read a term from a String.
%	Note that sread expects "abc" rather than "abc. ".
sread(String, X) :-
	parser$s(String, Term, _),
	!,
	X = Term.

%	Read a term and return lists of the named vars and corresponding variables.
sreadTerm(String, X, VarNames, Vars) :-
	parser$s(String, Term, VarNames.Vars),
	!,
	X = Term.

%	Read a term from a TokenList.
tread(TokenList, X) :-
	parser$t(TokenList, Term, _),
	!,
	X = Term.

%	Read a term and return lists of the named vars and corresponding variables.
treadTerm(TokenList, X, VarNames, Vars) :-
	parser$t(TokenList, Term, VarNames.Vars),
	!,
	X = Term.

%	Strip the first token off the front of the list S0,
%	leaving the remainder of the list in S1.
tokenize(S0, Token, Type, S1) :-
	$listToString(S0, String),
	$tokenize(String, Token, Type, S1).

%	Get a list of (Token.Type) pairs from a string.
tokenize(String, TokenList) :-
	$listToString(String, S),
	$getStringTokens(S, TokenList).

$getStringTokens(String, TokenList) :-
	$tokenize(String, Token, Type, Rest),
	$getStringTokens(Rest, Token, Type, TokenList).

:- $getStringTokens(_, X, _, _) when X.
$getStringTokens(_, '. ', end, []) :-
	!.
$getStringTokens(_, '. ', atom, []) :-
	!.
$getStringTokens(_, [], end_of_file, []) :-
	!.
$getStringTokens(String, Token, Type, (Token.Type).TokenList) :-
	$getStringTokens(String, TokenList).

%	General interface to $sprt.
$writev(Flags, X, S) :-
	( integer(Flags) ->
		( $sprt(Flags, X, S) ->
			true
		;	sys$error(warning, "Term too big to write"),
			fail
		)
	;	$writeFlag(Flags, cons, list, 1, F1),
		$writeFlag(Flags, nostring, string, 1, F2),
		$writeFlag(Flags, noquote, quote, 0, F3),
		$writeFlag(Flags, noquoteall, quoteall, 0, F4),
		$writeFlag(Flags, prefix, ops, 1, F5),
		(	(	member(prec = Prec, Flags),
				integer(Prec),
				1 =< Prec and Prec =< 1200) ->
			true
		;	Prec = 1200
		),
		(	(	member(base = Base, Flags),
				integer(Base),
				2 =< Base and Base =< 36) ->
			true
		;	Base = 10
		),
		!,
		F is Base
			+ (F1 << 6) + (F2 << 7) + (F3 << 8) + (F4 << 9) + (F5 << 10)
			+ (Prec << 11),
		( $sprt(F, X, S) ->
			true
		;	sys$error(warning, "Term too big to write"),
			fail
		)
	).

writev(Stream, Flags, X) :-
	(	$writev(Flags, X, String),
		putl(Stream, String),
		fail
	;	true
	).

writev(Flags, X) :-
	currentOutput(Stream),
	writev(Stream, Flags, X).

%	WriteCanonical/[1,2] use initial precedence 999 to ensure that terms like
%	(a,b) are quoted properly.
writeCanonical(X) :-
	prologFlag(vars, Old, off),
	writev(2'111110011100100001010, X),
	prologFlag(vars, off, Old).

writeCanonical(S, X) :-
	prologFlag(vars, Old, off),
	writev(S, 2'111110011100100001010, X),
	prologFlag(vars, off, Old).

writeq(X) :-
	writev(2'10111001010, X).

writeq(S, X) :-
	writev(S, 2'10111001010, X).

write(X) :-
	writev(2'10011001010, X).

write(S, X) :-
	writev(S, 2'10011001010, X).

display(X) :-
	prologFlag(vars, Old, off),
	writev(2'00000001010, X),
	prologFlag(vars, off, Old).

display(S, X) :-
	prologFlag(vars, Old, off),
	writev(S, 2'00000001010, X),
	prologFlag(vars, off, Old).

%	Specified to always succeed.
writeln(X) :-
	write(X), (nl, fail; true).

writeln(S, X) :-
	write(S, X), (nl(S), fail; true).

$writeFlag(Flags, Off, _, _, 0) :-
	member(Off, Flags),
	!.
$writeFlag(Flags, _, On, _, 1) :-
	member(On, Flags),
	!.
$writeFlag(_, _, _, Default, Default).

print(S, X, D, P) :-
	currentOutput(O),
	setOutput(S),
	print$printPrec(X, D, P),
	setOutput(O).

print(S, X, D) :-
	print(S, X, D, 1200).

print(S, X) :-
	D is maxint,
	print(S, X, D, 1200).

print(X) :-
	D is maxint,
	print$printPrec(X, D, 1200).

print$printPrec(X, D, P) :-
	print$print(X, P, D),
	fail.
print$printPrec(_, _, _).

print$print(V, _, _) :-
	var(V),
	!,
	write(V).
print$print(X, _, D) :-
	$list(X),
	D > 0,
	$defined(portray, 1),
	portray(X),
	!.
print$print(X, _, D) :-
	D > 0,
	$defined(portray, 1),
	portray(X),
	!.
print$print($VAR(V), _, _) :-
	!,
	write($VAR(V)).
print$print(write$var(V), _, _) :-
	!,
	write(write$var(V)).
print$print(X, _, _) :-
	cons(X),
	ground(X),
	isPrintL(X),
	!,
	write(X).
print$print(X, _, D) :-
	term(X),
	D =< 0,
	!,
	putl("**depth*bound**").
print$print(X, _, D) :-
	cons(X),
	!,
	put(0'[),
	D1 is D - 1,
	print$printL(X, D1),
	put(0']).
print$print((A, B), Prec, D) :-
	print$op(',', infix, L, P, R),
	!,
	print$open(P, Prec),
	D1 is D - 1,
	print$print(A, L, D1),
	putl(", "),
	print$print(B, R, D1),
	print$close(P, Prec).
print$print({X}, _, D) :-
	!,
	put(0'{),
	D1 is D - 1,
	print$print(X, 1200, D1),
	put(0'}).
print$print(X, Prec, D) :-
	functor(X, Op, 2),
	print$op(Op, infix, LP, OpP, RP),
	!,
	arg(1, X, L),
	arg(2, X, R),
	print$open(OpP, Prec),
	D1 is D - 1,
	print$print(L, LP, D1),
	put(0'\s),
	write(Op),
	put(0'\s),
	print$print(R, RP, D1),
	print$close(OpP, Prec).
print$print(X, Prec, D) :-
	functor(X, Op, 2),
	print$op(Op, bprefix, LP, OpP, RP),
	!,
	arg(1, X, L),
	arg(2, X, R),
	print$open(OpP, Prec),
	write(Op),
	put(0'\s),
	D1 is D - 1,
	print$print(L, LP, D1),
	put(0'\s),
	print$print(R, RP, D1),
	print$close(OpP, Prec).
print$print(X, Prec, D) :-
	functor(X, Op, 1),
	print$op(Op, prefix, _, OpP, RP),
	!,
	arg(1, X, R),
	print$open(OpP, Prec),
	write(Op),
	put(0'\s),
	D1 is D - 1,
	print$print(R, RP, D1),
	print$close(OpP, Prec).
print$print(X, Prec, D) :-
	functor(X, Op, 1),
	print$op(Op, postfix, LP, OpP, _),
	!,
	arg(1, X, L),
	print$open(OpP, Prec),
	D1 is D - 1,
	print$print(L, LP, D1),
	put(0'\s),
	write(Op),
	print$close(OpP, Prec).
print$print(X, _, D) :-
	term(X),
	!,
	X =.. [F|Args],
	write(F),
	put(0'(),
	D1 is D - 1,
	print$printL(Args, D1),
	put(0')).
print$print(X, _, _) :-
	write(X).

%	Note that print$printL is always passed a cons cell.
print$printL(X.XT, D) :-
	( cons(XT) ->
		print$print(X, 999, D),
		putl(", "),
		print$printL(XT, D)
	; XT == [] ->
		print$print(X, 999, D)
	;	print$print(X, 999, D),
		putl("| "),
		print$print(XT, 999, D)
	).

print$open(OpP, Prec) :-
	OpP > Prec -> put(0'().

print$close(OpP, Prec) :-
	OpP > Prec -> put(0')).

%	BUG!  Should use parser$op.
print$op(Op, F, L, P, R) :-
	currentOp(P, X, Op),
	print$nop(X, F, L, P, R).

print$nop(xfx, infix, Q, P, Q) :- Q is P - 1.
print$nop(xfy, infix, Q, P, P) :- Q is P - 1.
print$nop(yfx, infix, P, P, Q) :- Q is P - 1.
print$nop(fxx, bprefix, Q, P, Q) :- Q is P - 1.
print$nop(fxy, bprefix, Q, P, P) :- Q is P - 1.
print$nop(fyx, bprefix, P, P, Q) :- Q is P - 1.
print$nop(fyy, bprefix, P, P, P).
print$nop(fx, prefix, x, P, Q) :- Q is P - 1.
print$nop(fy, prefix, x, P, P).
print$nop(xf, postfix, Q, P, x) :- Q is P - 1.
print$nop(yf, postfix, P, P, x).

termToString(Term, String) :-
	$writev(2'10011001010, Term, String).

termToString(Flags, Term, String) :-
	$writev(Flags, Term, String).

%	The compiler sometimes arranges to call $format/2 directly.
format(Format, Args) :-
	$format(Format, Args).

$format(Format, Args) :-
	currentOutput(Stream),
	$format(Stream, Format, Args).

%	The compiler sometimes arranges to call $format/3 directly.
format(Stream, Format, Args) :-
	$format(Stream, Format, Args).

$format(Stream, Format, Args) :-
	$catchError(
		format$format(Stream, Format, Args),
		format(Stream, Format, Args),
		Error,
		Error
		),
	fail.
$format(_, _, _).

$quickFormat(Stream, Format, Args) :-
	$catchError(
		format$quickFormat(Stream, Format, Args),
		format(Stream, Format, Args),
		Error,
		Error
		),
	fail.
$quickFormat(_, _, _).

%	Note that we could do sformat with format/3 and string-streams rather
%	than duplicate so much of format/3.

%	The compiler sometimes arranges to call $sformat/3 directly.
sformat(Format, Args, String) :-
	$sformat(Format, Args, String).

$sformat(Format, Args, String) :-
	$catchError(
		format$sformat(Format, Args, String),
		sformat(Format, Args, String),
		Error,
		Error).

format$sformat(Format, Args, String) :-
	( cons(Args) ->
		RealArgs = Args
	; Args == [] ->
		RealArgs = Args
	;	RealArgs = [Args]
	),
	$nameToString(Format, FormatString),
	format$parse(FormatString, RealArgs, RawString),
	format$fill(0, 0, [], RawString, String),
	!.
format$sformat(_Format, _Args, _String) :-
	$throwError(format(user_error, "~NUnexplained error!~n", [])).

format$format(Stream, Format, Args) :-
	( cons(Args) ->
		RealArgs = Args
	; Args == [] ->
		RealArgs = Args
	;	RealArgs = [Args]
	),
	($validOutputStream(Stream) ->
		true
	;	$throwError(format(user_error, "~NInvalid stream!~n", []))
	),
	$nameToString(Format, FormatString),
	format$parse(FormatString, RealArgs, RawString),
	linePosition(Stream, LinePos),
	format$fill(LinePos, 0, [], RawString, String),
	putl(Stream, String),
	!.
format$format(_Stream, _Format, _Args) :-
	$throwError(format(user_error, "~NUnexplained error!~n", [])).

%	A quicker version generated by the compiler for formats having no
%	position-sensitive commands.
format$quickFormat(Stream, Format, Args) :-
	( cons(Args) ->
		RealArgs = Args
	; Args == [] ->
		RealArgs = Args
	;	RealArgs = [Args]
	),
	($validOutputStream(Stream) ->
		true
	;	$throwError(format(user_error, "~NInvalid stream!~n", []))
	),
	$nameToString(Format, FormatString),
	format$parse(FormatString, RealArgs, String),
	putl(Stream, String),
	!.
format$quickFormat(_Stream, _Format, _Args) :-
	$throwError(format(user_error, "~NUnexplained error!~n", [])).

:- format$fill(_, _, _, [], _) when ever.
:- format$fill(_, _, _, X._, _) when X.
format$fill(_, _, _, [], []).
format$fill(_LinePos, _Tab, Tabs, 0'\n.RawString, String0) :-
	!,
	String0 = 0'\n.String,
	( cons(Tabs) ->
		$throwError(format(user_error, "~N~~t without following ~~|.~n", []))
	),
	format$fill(0, 0, [], RawString, String).
format$fill(LinePos, Tab, Tabs, C.RawString, C.String) :-
	integer(C),
	!,
	LinePos1 is LinePos + 1,
	format$fill(LinePos1, Tab, Tabs, RawString, String).
format$fill(LinePos, Tab, Tabs, nl.RawString, String) :-
	( LinePos == 0 ->
		format$fill(LinePos, Tab, Tabs, RawString, String)
	;	format$fill(LinePos, Tab, Tabs, 0'\n.RawString, String)
	).
format$fill(LinePos, _Tab, Tabs, tab(T).RawString, String) :-
	( T == dot ->
		NewTab = LinePos
	;	NewTab = T
	),
	( NewTab > LinePos ->
		NewLinePos = NewTab
	;	NewLinePos = LinePos
	),
	format$tabs(NewTab, LinePos, Tabs),
	format$fill(NewLinePos, NewTab, [], RawString, String).
format$fill(LinePos, Tab, Tabs, (+ T).RawString, String) :-
	NewTab is Tab + T,
	( NewTab > LinePos ->
		NewLinePos = NewTab
	;	NewLinePos = LinePos
	),
	format$tabs(NewTab, LinePos, Tabs),
	format$fill(NewLinePos, NewTab, [], RawString, String).
format$fill(LinePos, Tab, Tabs, fill(FillChar).RawString, String1) :-
	format$fill(
		LinePos, Tab,
		fill(FillChar, String1, String2).Tabs, RawString, String2).

format$tabs(NewTab, LinePos, Tabs) :-
	Spaces is NewTab - LinePos,		% BUG?  Should we complain if < 0?
	( Tabs == [], Spaces > 0 ->
		$throwError(format(user_error,
			"~NTabs set with no fill position between them.~n", []))
	; length(Tabs, Gaps) ->
		format$tabs(Spaces, Gaps, 0, Tabs)
	;	$throwError(format(user_error,
			"~NTabs and ~~p format don't mix.~n", []))
	).

:- format$tabs(_, _, _, Tabs) when Tabs.
format$tabs(_, _, _, []).
format$tabs(Spaces, Gaps, Remainder, fill(FillChar, S1, S2).Tabs) :-
	Gap is (Spaces + Remainder) // Gaps,
	format$gap(Gap, FillChar, S1, S2),
	NewRemainder is (Spaces + Remainder) mod Gaps,
	format$tabs(Spaces, Gaps, NewRemainder, Tabs).

:- format$gap(Count, _, String, _) when Count or String.
format$gap(Count, Arg, S0, S) :-
	( Count > 0 ->
		S0 = Arg.S1,
		Count1 is Count - 1,
		format$gap(Count1, Arg, S1, S)
	;	S0 = S
	).

format$parse([], Args, []) :-
	( cons(Args) ->
		$throwError(format(user_error,
			"~NUnused arguments to format: ~w.~n", [Args]))
	).
format$parse(0'~.Format, Args, String) :-
	!,
	format$control(Format, RestOfFormat, Args, Args1, ControlArg, Control),
	( format$field(Control, ControlArg, Args1, RestOfArgs, Field) ->
		true
	; integer(ControlArg) ->
		$throwError(format(user_error,
			"~NUnable to format first element of ~w using ~~~d~c.~n",
			[Args1, ControlArg, Control]))
	;	$throwError(format(user_error,
			"~NUnable to format first element of ~w using ~~~c.~n",
			[Args1, Control]))
	),
	append(Field, RestOfString, String),
	format$parse(RestOfFormat, RestOfArgs, RestOfString).
format$parse(C.Format, Args, C.String) :-
	format$parse(Format, Args, String).

:- format$control([], _, _, _, _, _) when ever.
:- format$control(X._, _, _, _, _, _) when X.				% Really index.
format$control(Format, RestOfFormat, Args, Args, ControlArg, Control) :-
	format$digits(Digits, Format, Control.RestOfFormat),
	cons(Digits),
	!,
	intToString(ControlArg, Digits).
format$control(0'*.Control.Format, Format,
		ControlArg.Args, Args, ControlArg, Control) :-
	!,
	(\+ integer(ControlArg) ->
		$throwError(format(user_error,
			"~NInteger expected -- ~w found.~n", [ControlArg]))
	).
format$control(0''.Fill.Control.Format, Format,
		Args, Args, Fill, Control) :-
	!.
format$control(Control.Format, Format, Args, Args, Default, Control) :-
	format$defaultControl(Control, Default),
	!.
format$control(Format, _, Args, _, _, _) :-
	$throwError(format(user_error,
		"~NUnable to parse field description in ~w.~n",
		[format(Format, Args)])).

format$digits(Digits0, X0, X) :-
	X0 = D.X1,
	( 0'0 =< D and D =< 0'9 ->
		Digits0 = D.Digits,
		format$digits(Digits, X1, X)
	;	Digits0 = [],
		X0 = X
	).

format$defaultControl(0'a, 0).
format$defaultControl(0'c, 1).
format$defaultControl(0'e, 6).
format$defaultControl(0'E, 6).
format$defaultControl(0'f, 6).
format$defaultControl(0'g, 6).
format$defaultControl(0'G, 6).
format$defaultControl(0'd, 0).
format$defaultControl(0'D, 0).
format$defaultControl(0'r, 8).
format$defaultControl(0'R, 8).
format$defaultControl(0's, length).
format$defaultControl(0'i, 0).
format$defaultControl(0'k, 0).
format$defaultControl(0'p, 0).
format$defaultControl(0'q, 0).
format$defaultControl(0'w, 0).
format$defaultControl(0'~, 0).
format$defaultControl(0'n, 1).
format$defaultControl(0'N, 0).
format$defaultControl(0'|, dot).
format$defaultControl(0'+, 8).
format$defaultControl(0't, 0'\s).

format$field(0'a, _, Arg.Args, Args, Name) :-
	name(Arg, Name).
format$field(0'c, Count, Arg.Args, Args, Field) :-
	integer(Arg),
	0 =< Arg and Arg =< 127,
	format$c(Count, Arg, Field).
format$field(0'e, Count, Arg.Args, Args, Field) :-
	format$floatingField(0'e, Count, Arg, Field).
format$field(0'E, Count, Arg.Args, Args, Field) :-
	format$floatingField(0'E, Count, Arg, Field).
format$field(0'f, Count, Arg.Args, Args, Field) :-
	format$floatingField(0'f, Count, Arg, Field).
format$field(0'g, Count, Arg.Args, Args, Field) :-
	format$floatingField(0'g, Count, Arg, Field).
format$field(0'G, Count, Arg.Args, Args, Field) :-
	format$floatingField(0'G, Count, Arg, Field).
format$field(0'd, Shift, Arg.Args, Args, Field) :-
	ground(Arg),
	isExpression(Arg),
	Number is Arg,
	integer(Number),
	Shift >= 0,
	format$d(Shift, Number, [], Field).
format$field(0'D, Shift, Arg.Args, Args, Field) :-
	ground(Arg),
	isExpression(Arg),
	Number is Arg,
	integer(Number),
	Shift >= 0,
	format$d(Shift, Number, 0',, Field).
format$field(0'r, Radix, Arg.Args, Args, Field) :-
	ground(Arg),
	isExpression(Arg),
	Number is Arg,
	integer(Number),
	1 < Radix and Radix =< 36,
	intToString(Radix, 0'a, Number, Field).
format$field(0'R, Radix, Arg.Args, Args, Field) :-
	ground(Arg),
	isExpression(Arg),
	Number is Arg,
	integer(Number),
	1 < Radix and Radix =< 36,
	intToString(Radix, 0'A, Number, Field).
format$field(0's, Length, Arg.Args, Args, Field) :-
	ground(Arg),
	isAsciiL(Arg),
	( integer(Length) ->
		length(Arg, N),
		length(Field, Length),
		( Length =< N ->
			append(Field, _, Arg)
		;	NPadding is Length - N,
			format$c(NPadding, 0'\s, Padding),
			append(Padding, Arg, Field)
		)
	;	Length = length,
		Arg = Field
	).
format$field(0'i, _, _.Args, Args, []).
format$field(0'k, _, Arg.Args, Args, Field) :-
	prologFlag(vars, Old, off),
	termToString(2'111110011100100001010, Arg, Field),
	prologFlag(vars, off, Old).
format$field(0'p, _, Arg.Args, Args, Field) :-
	openStreamToString(S),
	print(S, Arg),
	closeStreamToString(S, Field).
format$field(0'q, _, Arg.Args, Args, Field) :-
	termToString(2'10111001010, Arg, Field).
format$field(0'w, _, Arg.Args, Args, Field) :-
	termToString(2'10011001010, Arg, Field).
format$field(0'~, _, Args, Args, "~").
format$field(0'n, N, Args, Args, Field) :-
	format$c(N, 0'\n, Field).
format$field(0'N, _, Args, Args, [nl]).
format$field(0'|, N, Args, Args, [tab(N)]).
format$field(0'+, N, Args, Args, [+ N]).
format$field(0't, N, Args, Args, [fill(N)]).

format$floatingField(Control, Count, Arg, Field) :-
	ground(Arg),
	isExpression(Arg),
	Number is Arg,
	$printNumber(Control, Count, Number, Field).

format$c(Count, Arg, Field0) :-
	(Count > 0 ->
		Field0 = Arg.Field,
		Count1 is Count - 1,
		format$c(Count1, Arg, Field)
	;	Field0 = []
	).

format$d(Shift, Arg, Comma, Field) :-
	(Arg >= 0 ->
		Field = UnsignedField,
		I = Arg
	;	Field = 0'-.UnsignedField,
		I is -Arg						% BUG!
	),
	intToString(I, DigitString),
	length(DigitString, Digits),
	length(Right, Shift),
	(Digits =< Shift ->
		append(Zeros, DigitString, Right),
		NZeros is Shift - Digits,
		format$c(NZeros, 0'0, Zeros),
		Left = "0"
	;	append(Left, Right, DigitString)
	),
	(integer(Comma) ->
		format$insertCommas(Left, Comma, CommaLeft)
	;	CommaLeft = Left
	),
	(Shift =:= 0 ->
		UnsignedField = CommaLeft
	;	append(".", Right, F),
		append(CommaLeft, F, UnsignedField)
	).

format$insertCommas(Left, Comma, CommaLeft) :-
	reverse(Left, LeftR),
	format$insertCommasReversed(LeftR, Comma, CommaLeftR),
	reverse(CommaLeftR, CommaLeft).

format$insertCommasReversed(A.B.C.Left, Comma, A.B.C.Comma.CommaLeft) :-
	cons(Left),
	!,
	format$insertCommasReversed(Left, Comma, CommaLeft).
format$insertCommasReversed(Left, _, Left).

close(Var) :-
	var(Var),
	!,
	format(user_error,
		"~NError in ~w -- variable unexpected.~n", [close(Var)]),
	fail.
close(user) :-
	!,
	format(user_error,
		"~NError in ~w -- ambiguous.~n", [close(user)]),
	fail.
close(Stream) :-
	$anyCurrentStream(_, _, Stream),
	!,
	$close(Stream).
close(FileName) :-
	$expandFileName(FileName, File),
	!,
	( currentStream(File, _, Stream) ->
		$close(Stream)
	;	prologFlag(fileErrors, on),
		format(user_error,
			"~NError in ~w -- no stream open on ~w.~n",
			[close(FileName), FileName]),
		fail
	).
close(X) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n", [close(X)]),
	fail.

open(FileName, Mode, Stream) :-
	ground(FileName),
	$expandFileName(FileName, File),
	!,
	( atom(Mode), $validIOMode(Mode, file) ->
		true
	;	format(user_error,
			"~NError in ~w -- invalid mode.~n",
			[open(FileName, Mode, Stream)]),
		fail
	),
	$open(File, Mode, Stream).
open(FileName, Mode, Stream) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n",
		[open(FileName, Mode, Stream)]).

$validIOMode(read, file).
$validIOMode(stringRead, string).
$validIOMode(write, file).
$validIOMode(stringWrite, string).
$validIOMode(append, file).

$validIMode(read, file).
$validIMode(stringRead, string).

$validOMode(write, file).
$validOMode(stringWrite, string).
$validOMode(append, file).

openNullStream(Stream) :- $open('/dev/null', write, Stream).

%	Open a stream reading from a list of characters.
openStreamFromString(String, Stream) :-
	ground(String),
	isAsciiL(String),
	!,
	$open(String, stringRead, Stream).
openStreamFromString(String, Stream) :-
	format(user_error,
		"~NError in ~w -- String must be a list of character codes.~n",
		[openStreamFromString(String, Stream)]),
	fail.

%	Open a stream writing to an internal buffer.
openStreamToString(Stream) :-
	$open(100, stringWrite, Stream).

closeStreamToString(Var, Chars) :-
	var(Var),
	!,
	format(user_error,
		"~NError in ~w -- variable unexpected.~n",
		[closeStreamToString(Var, Chars)]),
	fail.
closeStreamToString(Stream, Chars) :-
	$anyCurrentStream(_, _, Stream),
	!,
	$close(Stream, Chars).

ftell(Stream, Offset) :-
	var(Stream),
	!,
	format(user_error,
		"~NError in ~w -- variable unexpected.~n", [ftell(Stream, Offset)]),
	fail.
ftell(user, Offset) :-
	!,
	format(user_error,
		"~NError in ~w -- ambiguous.~n", [ftell(user, Offset)]),
	fail.
ftell(Stream, Offset) :-
	$anyCurrentStream(_, _, Stream),
	!,
	$ftell(Stream, Offset).
ftell(FileName, Offset) :-
	ground(FileName),
	$expandFileName(FileName, File),
	!,
	( currentStream(File, _, Stream) ->
		$ftell(Stream, Offset)
	;	prologFlag(fileErrors, on),
		format(user_error,
			"~NError in ~w -- no stream open on ~w.~n",
			[ftell(FileName, Offset), FileName]),
		fail
	).
ftell(X, Offset) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n", [ftell(X, Offset)]),
	fail.

fseek(Stream, Offset, Whence) :-
	\+ ground(Stream.Offset.Whence),
	!,
	format(user_error,
		"~NError in ~w -- all arguments must be ground.~n",
		[fseek(Stream, Offset, Whence)]),
	fail.
fseek(user, Offset, Whence) :-
	!,
	format(user_error,
		"~NError in ~w -- ambiguous.~n", [fseek(user, Offset, Whence)]),
	fail.
fseek(Stream, Offset, Whence) :-
	$anyCurrentStream(_, _, Stream),
	!,
	( io$whence(Whence, W) ->
		true
	;	findall(N, io$whence(N, _), WS),
		format(user_error,
			"~NError in ~w -- one of ~w expected.~n",
			[fseek(File, Offset, Whence), WS]),
		fail
	),
	( $fseek(Stream, Offset, W) ->
		true
	;	prologFlag(fileErrors, on),
		format(user_error,
			"~NError in ~w -- unable to seek as requested.~n",
			[fseek(File, Offset, Whence), File]),
		fail
	).
fseek(FileName, Offset, Whence) :-
	$expandFileName(FileName, File),
	!,
	( currentStream(File, _, Stream) ->
		fseek(Stream, Offset, Whence)		% Go round again!
	;	prologFlag(fileErrors, on),
		format(user_error,
			"~NError in ~w -- no stream open on ~w.~n",
			[fseek(FileName, Offset, Whence), FileName]),
		fail
	).
fseek(X, Offset, Whence) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n",
		[fseek(X, Offset, Whence)]),
	fail.

io$whence(beginning, 0).
io$whence(current, 1).
io$whence(end, 2).

fileErrors :-
	$setFlag(fileErrors, on).

noFileErrors :-
	$setFlag(fileErrors, off).

see(Var) :-
	var(Var),
	!,
	format(user_error,
		"~NError in ~w -- variable unexpected.~n", [see(Var)]),
	fail.
see(user) :-
	!,
	setInput(user).
see(Stream) :-
	$anyCurrentStream(_, _, Stream),	% Want to succeed on output streams too.
	!,
	setInput(Stream).
see(FileName) :-
	ground(FileName),
	$expandFileName(FileName, File),
	!,
	( currentStream(File, read, Stream) ->
		true
	;	open(File, read, Stream)
	),
	setInput(Stream).
see(X) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n", [see(X)]),
	fail.

seeing(Stream) :-
	currentInput(Stream).

seen :-
	currentInput(Stream),
	close(Stream),
	setInput(user_input).

tell(Var) :-
	var(Var),
	!,
	format(user_error,
		"~NError in ~w -- variable unexpected.~n", [tell(Var)]),
	fail.
tell(user) :-
	!,
	setOutput(user).
tell(Stream) :-
	$anyCurrentStream(_, _, Stream),	% Want to succeed on input streams too.
	!,
	setOutput(Stream).
tell(FileName) :-
	ground(FileName),
	$expandFileName(FileName, File),
	!,
	( currentStream(File, Mode, Stream), $validOMode(Mode, file) ->
		true
	;	open(File, write, Stream)
	),
	setOutput(Stream).
tell(X) :-
	format(user_error,
		"~NError in ~w -- file name or stream expected.~n", [tell(X)]),
	fail.

telling(Stream) :-
	currentOutput(Stream).

told :-
	currentOutput(Stream),
	close(Stream),
	setOutput(user_output).

$anyCurrentStream(File, Mode, Stream) :-
	prologFlag(nstreams, NStreams),
	NStreams1 is NStreams - 1,
	between(0, NStreams1, FD),
	$currentStream(FD, File, Mode, Stream).

$anyCurrentStream(FD, Stream) :-
	prologFlag(nstreams, NStreams),
	NStreams1 is NStreams - 1,
	between(0, NStreams1, FD),
	$currentStream(FD, _File, _Mode, Stream).

$getStreamInfo(Stream, Info) :-
	prologFlag(nstreams, NStreams),
	NStreams1 is NStreams - 1,
	between(0, NStreams1, FD),
	$getStream(FD, Info),
	arg(3, Info, Stream0),			% BUG!  Magic number.
	Stream0 \== [],
	( Stream == Stream0 ->
		!							% BUG!  Severe karma reduction.
	;	Stream = Stream0
	).

currentStream(File, Mode, Stream) :-
	prologFlag(nstreams, NStreams),
	NStreams1 is NStreams - 1,
	between(3, NStreams1, FD),
	$currentStream(FD, File, Mode, Stream).

$validInputStream(user).
$validInputStream(Stream) :-
	$anyCurrentStream(_, Mode, Stream),
	$validIMode(Mode, _).

$validOutputStream(user).
$validOutputStream(Stream) :-
	$anyCurrentStream(_, Mode, Stream),
	$validOMode(Mode, _).

characterCount(Stream, Chars) :-
	(	Stream = user,
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = RealStream
	),
	$getStreamInfo(RealStream, Info),
	arg(5, Info, Chars).			% BUG!  Magic number.

lineCount(Stream, Lines) :-
	(	Stream = user,
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = RealStream
	),
	$getStreamInfo(RealStream, Info),
	arg(6, Info, Lines).			% BUG!  Magic number.

linePosition(Stream, LinePos) :-
	(	Stream = user,
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = RealStream
	),
	$getStreamInfo(RealStream, Info),
	arg(7, Info, LinePos).			% BUG!  Magic number.

streamEof(Stream) :-
	( Stream == user ->
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = user,
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = RealStream
	),
	$getStreamInfo(RealStream, Info),
	arg(4, Info, eof).			% BUG!  Magic number.
	

streamPosition(Stream, Old, New) :-
	( Stream == user ->
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = user,
		RealStream = user_input		% so $getStreamInfo will work
	;	Stream = RealStream
	),
	$getStreamInfo(RealStream, Info),
	Old = $streamPosition(OldOffset, OldChars, OldLines, OldLinePos),
	arg(5, Info, OldChars),			% BUG!  Magic number.
	arg(6, Info, OldLines),			% BUG!  Magic number.
	arg(7, Info, OldLinePos),		% BUG!  Magic number.
	ftell(RealStream, OldOffset),
	ground(New),
	New = $streamPosition(NewOffset, NewChars, NewLines, NewLinePos),
	integer(NewOffset), NewOffset >= 0,
	integer(NewChars), NewChars >= 0,
	integer(NewLines), NewLines > 0,
	integer(NewLinePos), NewLinePos >= 0,
	$replacn(5, Info, NewChars),	% BUG!  Magic number.
	$replacn(6, Info, NewLines),	% BUG!  Magic number.
	$replacn(7, Info, NewLinePos),	% BUG!  Magic number.
	fseek(RealStream, NewOffset, beginning).

:- isAlnum(X) when X.
isAlnum(X) :-
	integer(X),
	X >= 0'a and X =< 0'z or X >= 0'A and X =< 0'Z or X >= 0'0 and X =< 0'9.

:- isPrint(X) when X.
isPrint(X) :-
	integer(X),
	32 =< X and X < 127 or (X =:= 9) or (X =:= 10) or (X =:= 12) or (X =:= 13).

:- isPrintL(X) when ground(X).
isPrintL(L) :-
	ground(L),
	mapList(isPrint, L).

:- isAlpha(X) when X.
isAlpha(X) :-
	integer(X),
	X >= 0'a and X =< 0'z or X >= 0'A and X =< 0'Z.

:- isAlphaL(X) when ground(X).
isAlphaL(L) :-
	ground(L),
	mapList(isAlpha, L).

:- isAscii(X) when X.
isAscii(X) :-
	integer(X),
	0 < X and X =< 127.

:- isAsciiL(X) when ground(X).
isAsciiL(L) :-
	ground(L),
	mapList(isAscii, L).

:- isCntrl(X) when X.
isCntrl(X) :-
	integer(X),
	1 =< X and X < 32.

:- isDigit(X) when X.
isDigit(X) :-
	integer(X),
	0'0 =< X and X =< 0'9.

:- isLower(X) when X.
isLower(X) :-
	integer(X),
	0'a =< X and X =< 0'z.

:- isUpper(X) when X.
isUpper(X) :-
	integer(X),
	0'A =< X and X =< 0'Z.
