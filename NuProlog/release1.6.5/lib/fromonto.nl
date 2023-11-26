/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog IO redirection library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

?- op(800, yfx,
		[fromStream, fromFile, ontoStream, ontoFile, fromChars, ontoChars]).

%	Copy characters from the current input stream to the current output
%	stream.  Often used in something like
%		copyBytes fromFile xyzzy ontoStream Out.
copyBytes :-
	repeat,
	get0(X),
	( X \== -1 ->
		put(X),
		fail
	),
	!.

%	Execute Goal with current input from Stream.  Stream must be a valid
%	stream open for input.  It is not closed.  Succeeds if and only if
%	Goal does.  This is an infix operator.
fromStream(Goal, Stream) :-
	currentInput(Old),
	setInput(Stream),
	( call(Goal) ->
		setInput(Old)
	;	setInput(Old),
		fail
	).

%	Execute Goal with current input from File.  The stream opened on File
%	is closed.  Succeeds if and only if Goal does.  This is an infix
%	operator.
fromFile(Goal, File) :-
	currentInput(Old),
	open(File, read, Stream),
	setInput(Stream),
	( call(Goal) ->
		close(Stream),
		setInput(Old)
	;	close(Stream),
		setInput(Old),
		fail
	).

%	Execute Goal with current input from the list of ASCII codes Chars.
%	The stream opened on Chars is closed.  Succeeds if and only if Goal
%	does.  This is an infix operator.
fromChars(Goal, Chars) :-
	currentInput(Old),
	openStreamFromString(Chars, Stream),
	setInput(Stream),
	( call(Goal) ->
		close(Stream),
		setInput(Old)
	;	close(Stream),
		setInput(Old),
		fail
	).

%	Execute Goal with current output to Stream.  Stream must be a valid
%	stream open for output.  It is not closed.  Succeeds if and only if
%	Goal does.  This is an infix operator.
ontoStream(Goal, Stream) :-
	currentOutput(Old),
	setOutput(Stream),
	( call(Goal) ->
		setOutput(Old)
	;	setOutput(Old),
		fail
	).

%	Execute Goal with current output to File.  The stream opened on File
%	is closed.  Succeeds if and only if Goal does.  This is an infix
%	operator.
ontoFile(Goal, File) :-
	currentOutput(Old),
	open(File, write, Stream),
	setOutput(Stream),
	( call(Goal) ->
		close(Stream),
		setOutput(Old)
	;	close(Stream),
		setOutput(Old),
		fail
	).

%	Execute Goal collecting the current output as the list of ASCII codes Chars.
%	The stream opened on Chars is closed.  Succeeds if and only if Goal does.
%	This is an infix operator.
ontoChars(Goal, Chars) :-
	currentOutput(Old),
	openStreamToString(Stream),
	setOutput(Stream),
	( call(Goal) ->
		closeStreamToString(Stream, Chars),
		setOutput(Old)
	;	closeStreamToString(Stream, Chars),
		setOutput(Old),
		fail
	).
