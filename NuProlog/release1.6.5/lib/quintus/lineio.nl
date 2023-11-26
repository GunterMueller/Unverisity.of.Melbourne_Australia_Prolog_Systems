/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible line-at-a-time IO library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

?- initializing, ensureLoaded(library(lineio)).

%	Read a line from the current input stream, returning it as a list of
%	character codes.  Does not include the line-terminator character.
%	Fails if end_of_file is reached before the end of line.
get_line(Line) :-
	getLine(Line).

%	Read a line from the current input stream, returning it as a list of
%	character codes.  Does not include the line-terminator character in
%	Line, but returns whatever character ended the line as Terminator.
%	This may be -1 (eof).
get_line(Line, Terminator) :-
	getLine(Line, Terminator).

%	Read a line from Stream, returning it as a list of character codes.
%	Does not include the line-terminator character.  Fails if end_of_file
%	is reached before the end of line.
fget_line(Stream, Line) :-
	fGetLine(Stream, Line).

%	Read a line from Stream, returning it as a list of character codes.
%	Does not include the line-terminator character in Line, but returns
%	whatever character ended the line as Terminator.  This may be -1 (eof).
fget_line(Stream, Line, Terminator) :-
	fGetLine(Stream, Line, Terminator).

%	Write a list of character codes on user_output.
put_chars(Chars) :-
	putChars(Chars).

%	Write a list of character codes on user_output, terminating the
%	current line.
put_line(Chars) :-
	putLine(Chars).

%	Skip to the end of the current line of user_input.  Equivalent to
%	fget_line(user_input, _, _).
skip_line :-
	skipLine.
