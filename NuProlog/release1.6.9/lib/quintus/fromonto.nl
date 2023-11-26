/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1989 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog Quintus compatible IO redirection library.
% These routines are meant to be compatible with the analogous Quintus
% libraries, or at least their documentation, but there are some
% differences, both intended and otherwise.

?- initializing, ensureLoaded(library(fromonto)).

?- op(800, yfx,
		[from_stream, from_file, onto_stream, onto_file,
		from_chars, onto_chars]).

%	Copy characters from the current input stream to the current output
%	stream.  Often used in something like
%		copy_bytes fromFile xyzzy ontoStream Out.
copy_bytes :-
	copyBytes.

%	Execute Goal with current input from Stream.  Stream must be a valid
%	stream open for input.  It is not closed.  Succeeds if and only if
%	Goal does.  This is an infix operator.
from_stream(Goal, Stream) :-
	fromStream(Goal, Stream).

%	Execute Goal with current input from the list of ASCII codes Chars.
%	The stream opened on Chars is closed.  Succeeds if and only if Goal
%	does.  This is an infix operator.
from_chars(Goal, Chars) :-
	fromChars(Goal, Chars).

%	Execute Goal with current input from File.  The stream opened on File
%	is closed.  Succeeds if and only if Goal does.  This is an infix
%	operator.
from_file(Goal, File) :-
	fromFile(Goal, File).

%	Execute Goal with current output to Stream.  Stream must be a valid
%	stream open for output.  It is not closed.  Succeeds if and only if
%	Goal does.  This is an infix operator.
onto_stream(Goal, Stream) :-
	ontoStream(Goal, Stream).

%	Execute Goal collecting the current output as the list of ASCII codes Chars.
%	The stream opened on Chars is closed.  Succeeds if and only if Goal does.
%	This is an infix operator.
onto_chars(Goal, Chars) :-
	ontoChars(Goal, Chars).

%	Execute Goal with current output to File.  The stream opened on File
%	is closed.  Succeeds if and only if Goal does.  This is an infix
%	operator.
onto_file(Goal, File) :-
	ontoFile(Goal, File).
