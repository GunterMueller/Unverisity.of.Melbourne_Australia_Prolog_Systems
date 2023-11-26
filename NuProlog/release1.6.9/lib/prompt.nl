/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog prompted input library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

?- initializing, ensureLoaded(library(lineio)).

%	Print a Prompt at the beginning of a new line on user_output and
%	force the prompt out without starting another line.  Atoms and
%	strings are printed with ~a and ~s formats, terms of the form
%	format(Format, Args) are printed with format/2, lists of terms are
%	recursively printed in this fashion, and other terms are printed
%	with write/1.
%
%	Differs from the Quintus library(prompt) predicate of the same name.
prompt(Prompt) :-
	currentOutput(Old),
	setOutput(user_output),
	prompt$prompt(Prompt),
	flushOutput(user_output),
	setOutput(Old).

prompt$prompt(Prompt) :-
	var(Prompt),
	!,
	write(Prompt).
prompt$prompt([]) :-
	!.
prompt$prompt(X.Prompt) :-
	!,
	prompt$prompt(X),
	prompt$prompt(Prompt).
prompt$prompt(format(Format, Args)) :-
	!,
	format(Format, Args).
prompt$prompt(Prompt) :-
	isAsciiL(Prompt),
	!,
	putl(Prompt).
prompt$prompt(Prompt) :-
	write(Prompt).

%	Print a Prompt on user_output using prompt/1 and read a line from
%	user_input.  The first character read is returned as Char.
promptedChar(Prompt, Char) :-
	prompt(Prompt),
	get0(user_input, C),
	( C =\= 0'\n and C =\= -1 ->
		skipLine(user_input)
	),
	Char = C.

%	Print a Prompt on user_output using prompt/1 and read a line 
%	from user_input using getLine/1 from library(lineio).
promptedLine(Prompt, Line) :-
	prompt(Prompt),
	fGetLine(user_input, Line).

%	Print a Prompt on user_output using prompt/1 and read a line 
%	from user_input using getLine/2 from library(lineio).
promptedLine(Prompt, Line, Terminator) :-
	prompt(Prompt),
	fGetLine(user_input, Line, Terminator).
