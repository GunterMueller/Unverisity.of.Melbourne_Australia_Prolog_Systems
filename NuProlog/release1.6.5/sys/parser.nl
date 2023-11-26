/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: David Morley
 * Mangler: Jeff Schultz
 *
 * This code is often awful.  If there's a bit you think particularly nasty
 * the other author must have done it!
 */

% Nepolog parser.

% prefix
parser$convert1a(fx, Prec,
		op(Prec, _, Bpre, Post, In, _, P2, P3, P4),
		op(Prec, y, Bpre, Post, In, 1, P2, P3, P4)).
parser$convert1a(fy, Prec,
		op(Prec, _, Bpre, Post, In, _, P2, P3, P4),
		op(Prec, y, Bpre, Post, In, 0, P2, P3, P4)).
% binary prefix
parser$convert1a(fxx, Prec,
		op(Prec, Pre, _, Post, In, _, _, P3, P4),
		op(Prec, Pre, y, Post, In, 1, 1, P3, P4)).
parser$convert1a(fyx, Prec,
		op(Prec, Pre, _, Post, In, _, _, P3, P4), 
		op(Prec, Pre, y, Post, In, 0, 1, P3, P4)).
parser$convert1a(fxy, Prec,
		op(Prec, Pre, _, Post, In, _, _, P3, P4), 
		op(Prec, Pre, y, Post, In, 1, 0, P3, P4)).
% postfix
parser$convert1a(xf, Prec,
		op(Prec, Pre, Bpre, _, In, P1, P2, _, P4), 
		op(Prec, Pre, Bpre, y, In, P1, P2, 1, P4)).
parser$convert1a(yf, Prec,
		op(Prec, Pre, Bpre, _, In, P1, P2, _, P4), 
		op(Prec, Pre, Bpre, y, In, P1, P2, 0, P4)).
% infix
parser$convert1a(xfx, Prec,
		op(Prec, Pre, Bpre, Post, _, P1, P2, _, _), 
		op(Prec, Pre, Bpre, Post, y, P1, P2, 1, 1)).
parser$convert1a(yfx, Prec,
		op(Prec, Pre, Bpre, Post, _, P1, P2, _, _), 
		op(Prec, Pre, Bpre, Post, y, P1, P2, 0, 1)).
parser$convert1a(xfy, Prec,
		op(Prec, Pre, Bpre, Post, _, P1, P2, _, _), 
		op(Prec, Pre, Bpre, Post, y, P1, P2, 1, 0)).

parser$convert1(Type,Prec,Old,New) :-
	% attempt to modify old definition
	parser$convert1a(Type,Prec,Old,New),
	\+ parser$incompat(Old,New).
parser$convert1(Type, Prec, op(_, Pre, Bpre, Post, In, _, _, _, _), OP) :-
	% replace existing definition of same `fix'
	OP=op(Prec,Pre,Bpre,Post,In,_,_,_,_),
	parser$convert1a(Type, Prec, op(Prec,n,n,n,n,n,n,n,n), OP).
	
parser$convert2(fx, Prec,	op(Prec, y, _, _, _, 1, _, _, _)).
parser$convert2(fy, Prec,	op(Prec, y, _, _, _, 0, _, _, _)).
parser$convert2(fxx, Prec,	op(Prec, _, y, _, _, 1, 1, _, _)).
parser$convert2(fyx, Prec,	op(Prec, _, y, _, _, 0, 1, _, _)).
parser$convert2(fxy, Prec,	op(Prec, _, y, _, _, 1, 0, _, _)).
parser$convert2(xf, Prec,	op(Prec, _, _, y, _, _, _, 1, _)).
parser$convert2(yf, Prec,	op(Prec, _, _, y, _, _, _, 0, _)).
parser$convert2(xfx, Prec,	op(Prec, _, _, _, y, _, _, 1, 1)).
parser$convert2(yfx, Prec,	op(Prec, _, _, _, y, _, _, 0, 1)).
parser$convert2(xfy, Prec,	op(Prec, _, _, _, y, _, _, 1, 0)).

parser$incompat(op(_,_,_,_,_,0,_,_,_), op(_,_,_,_,_,1,_,_,_)).
parser$incompat(op(_,_,_,_,_,1,_,_,_), op(_,_,_,_,_,0,_,_,_)).
parser$incompat(op(_,_,_,_,_,_,0,_,_), op(_,_,_,_,_,_,1,_,_)).
parser$incompat(op(_,_,_,_,_,_,1,_,_), op(_,_,_,_,_,_,0,_,_)).
parser$incompat(op(_,_,_,_,_,_,_,0,_), op(_,_,_,_,_,_,_,1,_)).
parser$incompat(op(_,_,_,_,_,_,_,1,_), op(_,_,_,_,_,_,_,0,_)).
parser$incompat(op(_,_,_,_,_,_,_,_,0), op(_,_,_,_,_,_,_,_,1)).
parser$incompat(op(_,_,_,_,_,_,_,_,1), op(_,_,_,_,_,_,_,_,0)).

% bracketed term ( ... )			(b)
% functor f( ... , ... , ... )		(f)
% list [ ... , ... , ... | ... ]	(s and p)
% braces { ... }					(c)
% term ... .						(t)
parser$open('(').
parser$open(' (').
parser$open('[').
parser$open('{').

parser$close(')').
parser$close(']').
parser$close('}').
parser$close('. ').

parser$close(')', b).
parser$close(')', f).
parser$close(']', s).
parser$close(']', p).
parser$close('}', c).
parser$close('. ', t).

parser$middle2(f).
parser$middle2(s).

parser$token((Tok.Type).Rest, Rest, Tok, Type).
parser$token([], [], '. ', end).

parser$gtx([], V, TL) -->
	parser$gt([], V, TL, '. ', end).
parser$gtx((Tok.Type).Src, V, TL) -->
	parser$gt(Src, V, TL, Tok, Type).

?- parser$gt(_, _, _, _, X, _, _) when X.					% Really index.
parser$gt(_Src, V, T._, _, end_of_file) -->	% end of file
	=([pun(c, end_of_file)]),
	parser$error(unexp(T)),
	{parser$endvars(V)}.
parser$gt(Src, V, TL, '. ', end) -->
	parser$gtEnd(Src, V, TL).
parser$gt(Src, V, TL, A, atom) -->
	parser$gtAtom(Src, V, TL, A).
parser$gt(Src, V, _, ASCII, junk) -->	% junk character
	=(junk(ASCII).Rest),
	parser$error(junk(ASCII)),
	{	parser$endvars(V),
		parser$gtex(Src, Rest)
	}.
parser$gt(Src, V, TL, X, var) -->			% old variable
	{parser$lookup(X, Var, V)},
	!,
	[var(Var, X)],
	parser$gtx(Src, V, TL).
parser$gt(Src, vars(NL, VL, NL1, VL1), TL, X, var) -->	% new variable
	!,
	[var(Var, X)],
	parser$gtx(Src, vars(X.NL, Var.VL, NL1, VL1), TL).
parser$gt(Src, V, TL, X, var) -->				% simple thing
	[thing(X, var)],
	parser$gtx(Src, V, TL).
parser$gt(Src, V, TL, X, string) -->			% simple thing
	[thing(X, string)],
	parser$gtx(Src, V, TL).
parser$gt(Src, V, TL, X, number) -->			% simple thing
	[thing(X, number)],
	parser$gtx(Src, V, TL).
parser$gt(Src, V, TL, X, quoted) -->
	[atom(X, quoted, OP)],
	parser$gtQA(Src, V, TL, X, OP).

parser$gtEnd(_Src, V, [t]) -->	% end of term
	[pun(c, '. ')],
	=([]),
	{parser$endvars(V)},
	!.
parser$gtEnd(_Src, V, T._) -->	% unexpected end of term
	=([pun(c, '. ')]),
	parser$error(unexp(T)),
	{parser$endvars(V)}.
%parser$gtEnd(Src, V, t.TL)	-->	% current close bracket
%%	{parser$close('. ', t)},
%	!,
%	parser$gtAtomClose(Src, V, TL, '. ').
%parser$gtEnd(Src, V, TL) -->	% unmatched close bracket
%%	{parser$close('. ')},
%	!,
%	parser$gtAtomUnmatchedCB(Src, V, TL, '. ').

?- parser$gtAtom(_, _, _, A, _, _) when A.					% Really index.
parser$gtAtom(Src, V, b.TL, ')')	-->	% current close bracket
%	{parser$close(')', b)},
	!,
	parser$gtAtomClose(Src, V, TL, ')').
parser$gtAtom(Src, V, f.TL, ')')	-->	% current close bracket
%	{parser$close(')', f)},
	!,
	parser$gtAtomClose(Src, V, TL, ')').
parser$gtAtom(Src, V, s.TL, ']')	-->	% current close bracket
%	{parser$close(']', s)},
	!,
	parser$gtAtomClose(Src, V, TL, ']').
parser$gtAtom(Src, V, p.TL, ']')	-->	% current close bracket
%	{parser$close(']', p)},
	!,
	parser$gtAtomClose(Src, V, TL, ']').
parser$gtAtom(Src, V, c.TL, '}')	-->	% current close bracket
%	{parser$close('}', c)},
	!,
	parser$gtAtomClose(Src, V, TL, '}').
parser$gtAtom(Src, V, TL, ')') -->	% unmatched close bracket
%	{parser$close(')')},
	!,
	parser$gtAtomUnmatchedCB(Src, V, TL, ')').
parser$gtAtom(Src, V, TL, ']') -->	% unmatched close bracket
%	{parser$close(']')},
	!,
	parser$gtAtomUnmatchedCB(Src, V, TL, ']').
parser$gtAtom(Src, V, TL, '}') -->	% unmatched close bracket
%	{parser$close('}')},
	!,
	parser$gtAtomUnmatchedCB(Src, V, TL, '}').
parser$gtAtom(Src, V, TL, '(') -->	% open bracket
%	{parser$open('(', b)},
	!,
	[open('(')],
	parser$gtx(Src, V, b.TL).
parser$gtAtom(Src, V, TL, ' (') -->	% open bracket
%	{parser$open(' (', b)},
	!,
	[open(' (')],
	parser$gtx(Src, V, b.TL).
parser$gtAtom(Src, V, TL, '[') -->	% open bracket
%	{parser$open('[', s)},
	!,
	[open('[')],
	parser$gtx(Src, V, s.TL).
parser$gtAtom(Src, V, TL, '{') -->	% open bracket
%	{parser$open('{', c)},
	!,
	[open('{')],
	parser$gtx(Src, V, c.TL).
parser$gtAtom(Src, V, T.TL, ',')	-->	% middle symbol
	{parser$middle2(T)},
	!,
	[pun(m, ',')],
	parser$gtx(Src, V, T.TL).
parser$gtAtom(Src, V, s.TL, '|') -->	% | in [...]
	!,
	[pun(m, '|')],
	parser$gtx(Src, V, p.TL).
parser$gtAtom(Src, V, TL, X) -->
	[atom(X, atom, OP)],
	parser$gtQA(Src, V, TL, X, OP).

parser$gtAtomClose(Src, V, TL, A) -->
	[pun(c, A)],
	parser$gtx(Src, V, TL).

parser$gtAtomUnmatchedCB(Src, V, T._, A) -->
	=(pun(c, A).Rest),
	parser$error(unexp(T)),
	{	parser$endvars(V),
		parser$gtex(Src, Rest)
	}.

parser$gtQA(Src, V, TL, X, OP) -->
	{$opField(X, OP)},
	{parser$token(Src, Src1, Tok, Type)},
	parser$gt1(Src1, V, TL, Tok, Type).

parser$gt1(Src, V, TL, '(', atom) -->	% bracket of functor
	!,
	[open('(')],
	parser$gtx(Src, V, f.TL).
parser$gt1(Src, V, TL, Tok, Type) -->
	parser$gt(Src, V, TL, Tok, Type).

parser$gtex([], L) :-
	parser$gte([], L, '. ', end).
parser$gtex((Tok.Type).Src, L) :-
	parser$gte(Src, L, Tok, Type).

%	Handle rest of Src after an error is encountered.
?- parser$gte(_, _, _, X) when X.							% Really index.
parser$gte(_Src, [pun(c, end_of_file)], _, end_of_file).
parser$gte(_Src, [pun(c, '. ')], '. ', end).
%	:- !.
%parser$gte(_Src, pun(c, '. ').L, '. ', end) :-	% current close bracket
%%	parser$close('. '),
%	parser$gtex(Src, L).
parser$gte(Src, L, X, atom) :-
	parser$gteAtom(Src, L, X).
parser$gte(Src, atom(X, quoted, []).L, X, quoted) :-
	parser$gtex(Src, L).
parser$gte(Src, junk(X).L, X, junk) :-
	parser$gtex(Src, L).
parser$gte(Src, thing(X, var).L, X, var) :-
	parser$gtex(Src, L).
parser$gte(Src, thing(X, thing).L, X, thing) :-
	parser$gtex(Src, L).
parser$gte(Src, thing(X, number).L, X, number) :-
	parser$gtex(Src, L).
parser$gte(Src, thing(X, string).L, X, string) :-
	parser$gtex(Src, L).

?- parser$gteAtom(_, _, X) when X.							% Really index.
parser$gteAtom(Src, pun(c, A).L, A) :-		% current close bracket
	parser$close(A),
	!,
	parser$gtex(Src, L).
%parser$gteAtom(Src, pun(c, ')').L, ')') :-	% current close bracket
%%	parser$close(')'),
%	!,
%	parser$gtex(Src, L).
%parser$gteAtom(Src, pun(c, ']').L, ']') :-	% current close bracket
%%	parser$close(']'),
%	!,
%	parser$gtex(Src, L).
%parser$gteAtom(Src, pun(c, '}').L, '}') :-	% current close bracket
%%	parser$close('}'),
%	!,
%	parser$gtex(Src, L).
%	BUG?  L versus LL?
parser$gteAtom(Src, open(A).L, A) :-	% open bracket
	parser$open(A),
	!,
	parser$gtex(Src, L).
parser$gteAtom(Src, atom(X, atom, []).L, X) :-
	parser$gtex(Src, L).

% used as a (undone on backtracking) and (global variable) assert
parser$lookup("_", _, _) :-
	!.
parser$lookup(Name, Var, vars(NL, VL, _, _)) :-
	parser$lookup1(Name, Var, NL, VL).

parser$lookup1(Name, Var, Name._, Var._) :-
	!.
parser$lookup1(Name, Var, _.NL, _.VL) :-
	parser$lookup1(Name, Var, NL, VL).

parser$endvars(vars(NL, VL, NL, VL)).

% ERROR CHECKING / MESSAGES

parser$error(X, Pos, err(X).Pos).

parser$is_error --> =(err(_)._).

parser$val(X, Pos, val(X).Pos).

% simple cases
parser$pulloff(item(P1, _, Arg, Term, Rest), L1, Arg, T1, P, D3, Ans) :-
															% (fx Arg) xf
	P > P1,
	!,
	parser$pulloff(Rest, L1, Term, T1, P, D3, Ans).
parser$pulloff(L1, L2, T1, T2, P, _, okay) :-				% fx (Arg xf)
	L1 = item(P1, _, _, _, _),
	P < P1,
	!,
	L1 = L2,
	T1 = T2.
parser$pulloff(L1, L2, T1, T2, P, 1, okay) :-				% fy (Arg xf)
	L1 = item(P, 0, _, _, _),
	!,
	L1 = L2,
	T1 = T2.
parser$pulloff(item(P, 1, Arg, Term, Rest), L1, Arg, T1, P, 0, Ans) :-
															% (fx Arg) yf
	!,
%	BUG?  P3?
	parser$pulloff(Rest, L1, Term, T1, P, P3, Ans).
parser$pulloff(top(P2), top(P2), T1, T2, P, _, okay) :-		% Arg yf
	P =< P2,
	!,
	T1 = T2.
% over the top
parser$pulloff(top(P1), _, _, _, _, _, prechigh3(P1)).		% Arg yf
parser$pulloff(item(P,0,_,_,_), _, _, _, P, 0, ambiguous) :-	% fy Arg yf
	!.
parser$pulloff(item(P,1,_,_,_), _, _, _, P, 1, impossible).	% fx Arg xf

parser$pullall(top(_), T) -->
	parser$val(T),
	!.
parser$pullall(item(_, _, Arg, Term, Rest), Arg) -->
	parser$pullall(Rest, Term).

%	read term
parser$r(T, Vlist) :-
	currentInput(Stream),
	parser$r(Stream, T, Vlist).

parser$r(Stream, T, NL.VL) :-
	parser$getTokens(Stream, TokenList),
	parser$read(TokenList, vars([], [], NL, VL), T).

parser$s(String, T, NL.VL) :-
	parser$getStringTokens(String, TokenList),
	parser$read(TokenList, vars([], [], NL, VL), T).

parser$t(TokenList, T, NL.VL) :-
	parser$read(TokenList, vars([], [], NL, VL), T).

parser$getTokens(Stream, TokenList) :-
	getToken(Stream, Token, Type),
	parser$getTokens(Stream, Token, Type, TokenList).

%	BUG!
parser$getTokens(_, '. ', end, []) :-
	!.
%parser$getTokens(_, '. ', atom, []) :-
%	!.
parser$getTokens(_, [], end_of_file, [([].end_of_file)]) :-
	!.
parser$getTokens(Stream, Token, Type, (Token.Type).TokenList) :-
	parser$getTokens(Stream, TokenList).

parser$getStringTokens(String, TokenList) :-
	tokenize(String, Token, Type, Rest),
	parser$getStringTokens(Rest, Token, Type, TokenList).

parser$getStringTokens(_, '. ', end, []) :-
	!.
%parser$getStringTokens(_, '. ', atom, []) :-
%	!.
parser$getStringTokens(_, [], end_of_file, []) :-
	!.
parser$getStringTokens([], Token, Type, [(Token.Type)]) :-
	!.
parser$getStringTokens(String, Token, Type, (Token.Type).TokenList) :-
	parser$getStringTokens(String, TokenList).

parser$read(Src, Vars, T) :-
	parser$gtx(Src, Vars, [t], X, Y),
	parser$pro(T, X, Y).

parser$pro(X, [pun(c, end_of_file)], _) :-
	eof(X),
	!.
parser$pro(_T, L, L1) :-
	parser$is_error(L1, _),
	parser$printe(L, L1),
	!,
	fail.
parser$pro(T, L, []) :-
	parser$fullterm(1200, T, _, L, L1),
	!,
	(parser$is_error(L1, L1)
	->	parser$printe(L, L1),
		!,
		fail
	).

parser$printe(L, err(Err).L1) :-
	parser$printmess(Err, Mess),
	nl(user_error),
	write(user_error, 'ERROR '), writeln(user_error, Mess),
	parser$findbr([], BrList, L, L1),
	parser$printbr(BrList, L, L1),
	write(user_error, ' ***HERE***>'),
	parser$printbr([], L1, []), nl(user_error),
	!.
parser$printe(L, L1) :-
	nl(user_error),
	writeln(user_error, 'PROBLEM WRITING ERROR'),
	writeln(user_error, L),
	writeln(user_error, L1).

parser$findbr(L, L1, End, End) :-
	!,
	reverse(L, L1).
parser$findbr(L, L1) -->
	=(Pos),
	[open(_)],
	!,
	parser$findbr(Pos.L, L1).
parser$findbr(_.L, L1) -->
	[pun(c, _)],
	!,
	parser$findbr(L, L1).
parser$findbr(L, L1) -->
	[_],
	parser$findbr(L, L1).

parser$printbr(_, End, End) :-
	!.
parser$printbr(Pos.L) -->
	=(Pos),
	[open(OB)],
	!,
	{write(user_error, OB)},
	parser$printbr(L).
parser$printbr(L) -->
	[open('['), pun(c, ']')],
	!,
	{write(user_error, ' []')},
	parser$printbr(L).
parser$printbr(L) -->
	[open(OB)],
	!,
	{write(user_error, OB), write(user_error, ...)},
	parser$skipbr,
	parser$printbr(L).
parser$printbr(L) -->
	parser$printtok,
	parser$printbr(L).

parser$printtok -->
	[var(_, X)],
	!,
	{putl(user_error, 0'\s.X)}.
parser$printtok -->
	[thing(X, var)],
	!,
	{putl(user_error, 0'\s.X)}.		% for things got by gte
parser$printtok -->
	[thing(X, _)],
	!,
	{write(user_error, ' '),
	write(user_error, X)}.
parser$printtok -->
	[atom(X, quoted, _)],
	!,
	{write(user_error, ' '''), write(user_error, X), write(user_error, '''')}.
parser$printtok -->
	[atom(X, atom, _)],
	!,
	{write(user_error, ' '), write(user_error, X)}.
parser$printtok -->
	[junk(ASCII)],
	!,
	{write(user_error, ascii(ASCII))}.
parser$printtok -->
	[pun(_, X)],
	!,
	{write(user_error, X)}.
parser$printtok -->
	[open(X)], !, {write(user_error, X)}.
parser$printtok -->
	[X],
	{write(user_error, ' '), write(user_error, 'STRANGE_TOKEN'(X))}.

?- parser$skipbr([], _) when ever.
?- parser$skipbr(X._, _) when X.
parser$skipbr -->
	=(pun(c, _)._),
	!.
parser$skipbr -->
	[open(_)],
	!,
	parser$skipbr,
	[_],
	parser$skipbr.
parser$skipbr -->
	[_],
	parser$skipbr.

parser$printmess(prechigh,
		'Operator precedence too high').
parser$printmess(prechigh1(M),
		'Prefix operator precedence too high, should be' - M).
parser$printmess(prechigh2(M),
		'Missing term or infix/postfix operator precedence too high to be used as atom, should be' - M).
parser$printmess(prechigh3(M),
		'Infix/postfix operator precedence too high, should be' - M).
parser$printmess(noterm, 'Missing term').
parser$printmess(insertop, 'Insert infix operator').
parser$printmess(junk(ASCII), 'Invalid character, ASCII value'=ASCII).
parser$printmess(unexp(T),
		'Unexpected punctuation, expecting'-A) :-
	parser$close(A, T).
parser$printmess(postin(TP),
		'Left argument of post/infix op. has too high a precedence number'(TP)).
parser$printmess(impossible,
		'Impossible use of operators: ?fx term xf?').
parser$printmess(ambigous,
		'Ambiguous use of operators: ?fy term yf?').
parser$printmess(X, 'UNKNOWN_ERROR'(X)).

% chrest = do rest if no errors
parser$chrest(_, _) -->
	parser$is_error,
	!.
parser$chrest(L, T) -->
	parser$rest(L, T, 0).

?- parser$rest(_, _, _, [], _) when ever.					% Really index.
?- parser$rest(_, _, _, X._, _) when X.
parser$rest(L, Term, _) -->
	=(pun(_, _)._),
	!,											% punctuation
	parser$pullall(L, Term).
parser$rest(L, Term, _) -->
	=(atom(A, _, _).open('(')._),				% functor
	!,
	parser$pullall(L, Term).
parser$rest(L, Term, TP) -->					% post/infix operator
	=(atom(_, _, op(P, _, _, Post, In, _, _, D3, D4))._),
	{integer(D3) /*D3 ~= n*/},
	!,
	( {TP > P - D3} ->							% prec of left arg too high
		parser$error(postin(TP))
	;	{parser$pulloff(L, L1, Term, T1, P, D3, Ans)},
		parser$rest1(L1, T1, Ans, Post, In, P, D4)
	).
%parser$rest(L, Term, TP) -->					% post/infix operator
%	=(atom(_, _, op(P, _, _, Post, In, _, _, D3, D4))._),
%	{integer(D3) /*D3 ~= n*/},
%	!,
%	{parser$pulloff(L, L1, Term, T1, P, D3, Ans)},
%	parser$rest1(L1, T1, Ans, Post, In, P, D4).
parser$rest(L, Term, _) -->					% other
	parser$pullall(L, Term).

parser$rest1(L1, T1, okay, y, y, Prec, D2) -->			% both, infix
	[atom(A, _, _)],
	{P2 is Prec-D2},
	parser$type1(P2),
	{T =.. [A, T1, Arg]},
	!,
	parser$term(P2, item(Prec, D2, Arg, T, L1)).
parser$rest1(L1, T1, okay, y, _, Prec, _) -->				% postfix
	[atom(A, _, _)],
	{T =.. [A, T1]},
	!,
	parser$rest(L1, T, Prec).
parser$rest1(L1, T1, okay, n, y, Prec, D2) -->			% infix operator
	[atom(A, _, _)],
	{P2 is Prec-D2},
	{T =.. [A, T1, Arg]},
	!,
	parser$term(P2, item(Prec, D2, Arg, T, L1)).
parser$rest1(_, _, Ans, _, _, _, _) -->
	parser$error(Ans).

% Read a term X, of precedence Prec
parser$fullterm(Prec, X, Pun) -->
	parser$term(Prec, top(Prec)),
	(	[val(X), pun(_, Pun)]				% correct 
	;	parser$is_error						% known error
	;	[val(X)], parser$error(insertop)	% didn't end with punctuation
	),
	!.

% Read a term into given list L of outstanding infix/prefix ops
?- parser$term(_, _, [], _) when ever.					% Really index.
?- parser$term(_, _, pun(_, _)._, _) when ever.
?- parser$term(_, _, open(X)._, _) when X.
?- parser$term(_, _, atom(_, _, _)._, _) when ever.
?- parser$term(_, _, var(_, _)._, _) when ever.
?- parser$term(_, _, thing(_, _)._, _) when ever.
parser$term(_, _L) -->
	=(pun(_, _)._),
	parser$error(noterm).
parser$term(_, L) -->
	[open('{')],
	parser$fullterm(1200, T, _),
	parser$chrest(L, {T}).
parser$term(_, L) -->
	[open(' (')],
	parser$fullterm(1200, T, _),
	parser$chrest(L, T).
parser$term(_, L) -->
	[open('(')],
	parser$fullterm(1200, T, _),
	parser$chrest(L, T).
parser$term(_, L) -->
	[open('['), pun(c, ']')],
	!,
	parser$rest(L, [], 0).
parser$term(_, L) -->
	[open('[')],
	parser$list(List), parser$chrest(L, List).
parser$term(_, L) -->
	[atom(F, _, _), open('(')],
	!,
	parser$list(Args),
	{X =.. F.Args },
	parser$chrest(L, X).
parser$term(MP, L) -->
	=(atom(A, _, op(Prec, Y1, Y2, _, _, D1, D2, _, _))._),
	!,
	parser$termop(A, Prec, Y1, Y2, D1, D2, MP, L).
parser$term(_, L) -->
	[atom(A, _, _)],
	parser$rest(L, A, 0).
parser$term(_, L)-->
	[var(X, _)],
	parser$rest(L, X, 0).
parser$term(_, L)-->
	[thing(X, _)],
	parser$rest(L, X, 0).

parser$termop(A, Prec, Y1, Y2, D1, D2, MP, L) --> % too high precedence
	{integer(D1), /*D1 ~= n,*/ Prec > MP},
	!,
	parser$error(prechigh1(MP)).	% (bin)prefix
parser$termop(A, Prec, Y1, y, D1, D2, MP, L) -->	% prefix /binary prefix
	{P1 is Prec - D1},
	[_], parser$type1(P1),
	!,
	parser$term(P1, top(P1)),
	(	parser$is_error,
		!													% known error
	;	[val(T)],
		{ P2 is Prec - D2},
		parser$type1(P2),									% binary
		!,
		{Term =.. [A, T, Arg]},
	 	parser$term(P2, item(Prec, D2, Arg, Term, L))		% got value
	;	{Y1 = y},
		!,
		[val(T)],											% unary
		{Term =.. [A, T]},
		parser$rest(L, Term, Prec)
	;	{Y1 = n},
		[_],
		parser$error(noterm)
	).
parser$termop(A, Prec, y, n, D1, D2, MP, L) -->		% prefix
	{P1 is Prec - D1},
	[_], parser$type1(P1),
	!,
	{T =.. [A, Arg]},
	parser$term(P1, item(Prec, D1, Arg, T, L)).
parser$termop(A, Prec, Y1, Y2, D1, D2, MP, L) --> % too high precedence
	{Prec > MP},
	!,
	parser$error(prechigh2(MP)).						% in/postfix or no term
parser$termop(A, Prec, Y1, Y2, D1, D2, MP, L) -->	% using op as atom
	[_],
	parser$rest(L, A, Prec).

% must do chrest after list
parser$list(X.List) -->
	parser$fullterm(1200, X, Pun),
	parser$restlist(List, Pun).

parser$restlist([], _) -->				% error
	parser$is_error,
	!.
parser$restlist(List, ',') -->			% , x ...
	!,
	parser$list(List).
parser$restlist(List, '|') -->			% | x ]
	!,
	parser$fullterm(1200, List, _).
parser$restlist([], _) -->				% ) or ]
	[].

?- parser$type1(_, [], _) when ever.					% Really index.
?- parser$type1(_, X._, _) when X.
parser$type1(_) -->
	=(open(_)._).
parser$type1(_) -->
	=(atom(_,_,_).open('(')._).
parser$type1(_) -->										% atom
	=(atom(_,_,[])._).
parser$type1(P1) -->									% lower op
	=(atom(_,_,op(P,_,_,_,_,_,_,_,_))._),
	{P =< P1}.
%parser$type1(_) -->
%	=(atom(_,_,op(P,_,_,_,_,X,_,_,_))._), {integer(X) /*X ~= n*/}. % pre / bpre
parser$type1(_) -->
	=(thing(_,_)._).
parser$type1(_) -->
	=(var(_,_)._).
