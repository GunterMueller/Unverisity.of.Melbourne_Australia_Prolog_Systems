
%I just wrote a fix for the following bugs in man/1:
%	- man [Functor1, Functor2, ...] doesn't work
%	- man(delete) and man(man) don't work, because the code
%	  for handling operators doesn't work
%	- there is no equivalent to "man -k" or "apropos" in unix
%	- the "More ?" prompt is not displayed until after you have entered
%	  a response
%
%I have added two new options to the man/1 command
%	man apropos(Functor):
%		Like the unix "apropos", this prints a list of all the manual
%		pages which seem appropriate for Functor.
%	man search(Functor):
%		This is the same as man apropos(Functor), except that it
%		in addition to the searching the manual entry names and
%		the short descriptions, it also searches the actual manual
%		pages, so this may find more entries - and take more time
%		searching - than using man apropos(Functor).
% man/1 - Lookup the on-line manual.
%
% Rewritten by Fergus Henderson, Oct 11 1992
% All local predicates have names '$man ...'.

%##?- initializing, dbCons('LIB/man.NUVERSION').
?- dbCons('/mip/db').

man(Pred) :-
	( var(Pred) ->
		'$man man'(Pred)
	; Pred = P.Ps ->
		(man(P);true), !, man(Ps)
	; Pred = apropos(What) ->
		'$man apropos'(What)
	; Pred = search(What) ->
		'$man search'(What)
	;	'$man man'(Pred)
	).

'$man man'(Pred) :-
	atomToString(Pred, PredOp),
	(	currentOp(_, _, Pred),
		append( 0'(.PredOp, ")", PredL)
	;	PredL = PredOp
	),
	man(PredL, ManText, _),
	putl(ManText),
	write('More ? '),
	flushOutput(user),
	get0(Ans),
	( Ans =\= 0'\n ->
		skip(0'\n)
	),
	Ans = 0'n.

'$man apropos'(What) :-
	atomToString(What,Text),
	format("Searching manual titles and descriptions",[]),
	( ground(Text) ->
		format(" for '~s'...~n",[Text])
	;	format("~n",[])
	),
	% find all occurences of Text in a manual Title or Description.
	man(Title, _, Description),
	( (   '$man occurs'(Text, Title)
	  ;   '$man occurs'(Text, Description)
	  ) ->
		format("----> ~s~n~s~n", [Title, Description]),
		fail
	;	fail
	).

'$man search'(What) :-
	atomToString(What,Text),
	format("Searching entire manual",[]),
	( ground(Text) ->
		format(" for '~s'...~n",[Text])
	;	format("~n",[])
	),
	% find all occurences of Text in a manual Title, Description, or ManPage
	man(Title, ManPage, Description),
	( (   '$man occurs'(Text, Title)
	  ;   '$man occurs'(Text, Description)
	  ;   '$man occurs'(Text, ManPage)
	  ) ->
		format("----> ~s~n~s~n", [Title, Description]),
		fail
	;	fail
	).

	% succeeds if 'What' is a substring of 'Where'.
'$man occurs'(What, Where) :-
	append(What, _, What_X),
	append(_, What_X, Where).


%	Original man/1 implementation.
%man(Pred) :-
%	(	nonvar(Pred),
%		(if currentOp(_, _, Pred) then
%			atomToString(Pred, PredOp),
%			append(PredOp, ")", P1),
%			PredL = 0'(.P1
%		else
%			atomToString(Pred, PredL)
%		)
%	;	var(Pred)
%	),
%	man(PredL, X, Y),
%	putl(X),
%	write('More ? '),
%	flushOutput(user_output),
%	get0(Ans),
%	( Ans =\= 0'\n ->
%		skip(0'\n)
%	),
%	Ans = 0'n.
