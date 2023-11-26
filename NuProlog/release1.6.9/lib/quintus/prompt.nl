/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible prompted input library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

?- initializing, ensureLoaded(library(lineio)).

%	Print a Prompt (an term or a list of terms making up a prompt at
%	the beginning of a new line on user_output and force the prompt
%	out without starting another line.  
prompt(Prompt) :-
	currentOutput(Old),
	setOutput(user_output),
	prompt$quintusPrompt(Prompt),
	flushOutput(user_output),
	setOutput(Old).

prompt$quintusPrompt(Prompt) :-
	var(Prompt),
	!,
	write(Prompt).
prompt$quintusPrompt([]) :-
	!.
prompt$quintusPrompt(X.Prompt) :-
	!,
	prompt$quintusPrompt(X),
	prompt$quintusPrompt(Prompt).
prompt$quintusPrompt(Prompt) :-
	write(Prompt).

%	Print a Prompt on user_output using prompt/1 and read a line from
%	user_input.  The first character read is returned as Char.
prompted_char(Prompt, Char) :-
	prompt(Prompt),
	get0(user_input, C),
	( C =\= 0'\n and C =\= -1 ->
		skipLine(user_input)
	),
	Char = C.

%	Print a Prompt on user_output using prompt/1 and read a line 
%	from user_input using getLine/1 from library(lineio).
prompted_line(Prompt, Line) :-
	prompt(Prompt),
	fGetLine(user_input, Line).

%	Print a Prompt on user_output using prompt/1 and read a line 
%	from user_input using getLine/2 from library(lineio).
prompted_line(Prompt, Line, Terminator) :-
	prompt(Prompt),
	fGetLine(user_input, Line, Terminator).
