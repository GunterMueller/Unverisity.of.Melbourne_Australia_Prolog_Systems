/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog line-at-a-time IO library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

%	Read a line from the current input stream, returning it as a list of
%	character codes.  Does not include the line-terminator character.
%	Fails if end_of_file is reached before the end of line.
getLine(Line) :-
	currentInput(Stream),
	fGetLine(Stream, Line, 0'\n).

%	Read a line from the current input stream, returning it as a list of
%	character codes.  Does not include the line-terminator character in
%	Line, but returns whatever character ended the line as Terminator.
%	This may be -1 (eof).
getLine(Line, Terminator) :-
	currentInput(Stream),
	fGetLine(Stream, Line, Terminator).

%	Read a line from Stream, returning it as a list of character codes.
%	Does not include the line-terminator character.  Fails if end_of_file
%	is reached before the end of line.
fGetLine(Stream, Line) :-
	fGetLine(Stream, Line, 0'\n).

%	Read a line from Stream, returning it as a list of character codes.
%	Does not include the line-terminator character in Line, but returns
%	whatever character ended the line as Terminator.  This may be -1 (eof).
fGetLine(Stream, Line, Terminator) :-
	get0(Stream, C),
	( C =:= 0'\n or C =:= -1 ->
		Line = [],
		C = Terminator
	;	Line = C.LineT,
		fGetLine(Stream, LineT, Terminator)
	).

%	Write a list of character codes on user_output.
putChars(Chars) :-
	putl(Chars).

%	Write a list of character codes on Stream.
putChars(Stream, Chars) :-
	putl(Stream, Chars).

%	Write a list of character codes on user_output, terminating the
%	current line.
putLine(Chars) :-
	putl(Chars),
	nl.

%	Write a list of character codes on Stream, terminating the current
%	line.
putLine(Stream, Chars) :-
	putl(Stream, Chars),
	nl(Stream).

%	Skip to the end of the current line of user_input.  Equivalent to
%	fGetLine(user_input, _, _).
skipLine :-
	skipLine(user_input).

%	Skip to the end of the current line of Stream.  Equivalent to
%	getLine(_, _).
skipLine(Stream) :-
	repeat,
	get0(Stream, C),
	C =:= 0'\n or C =:= -1,
	!.
