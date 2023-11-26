%	$Header: ndsysneg.pl,v 1.1 85/11/25 19:35:35 lee Exp $

% Low level negation facilities - $not/2, $if/4, $some/2, $noteq/3
% $solutions/4 (could be improved) for systems which dont have delaying.
% Where delaying would occur, an error message is printed.
%
% Could have two versions of this - one for safe uses, which
% has a faster cut implementation or an extra call to clobber trail.
%
% Lee 8/9/85

%==================================================================
	% Check globals are ground then use same method as ->
	% Can use more efficient form of cut for safe uses.
$if(Glob, Cond, Then, Else) :-
	( \+ ground(Glob) ->
		write('Error: should delay '),
		write($if(Glob, Cond, Then, Else)),
		nl
		% , abort
	),
	(Cond, !, Then ; Else).

%==================================================================
	% Version which returns all solutions to Cond.
	% Could be as above with ! replaced by soft cut in
	% systems where soft cut is available
$if_soft(Glob, Cond, Then, Else) :-
	( \+ ground(Glob) ->
		write('Error: should delay '),
		write($if_soft(Glob, Cond, Then, Else)),
		nl
		% , abort
	),
	( \+ Cond ->
		Else
	;
		Cond,
		Then
	).

%==================================================================
	% IF the globals are ground then call Goal followed by cut.
	% Otherwise call Goal without cut.  Basically, a safe version
	% of once/1.
$some(Glob, Goal) :-
	ground(Glob),
	!,
	Goal,
	!.
$some(Glob, Goal) :-
	Goal.

%==================================================================
% utilities from ground.pl (portable versions)
	% T has no vars at the time of the call (non logical)
ground(T) :-
	var(T),
	!,
	fail.
ground(T) :-
	atomic(T),
	!.
ground(T) :-
	functor(T, F, N),
	$ground1(N, T).

	% calls ground for each arg
$ground1(0, T) :-
	!.
$ground1(N, T) :-
	arg(N, T, A),
	ground(A),
	N1 is N-1,
	$ground1(N1, T).

	% make a term ground (all vars are bound to [])
make_ground([]) :-
	!.
make_ground(A.B) :-
	!,
	make_ground(A),
	make_ground(B).
make_ground(N) :-
	atomic(N),
	!.
make_ground(C) :-
	C =.. F.A,
	make_ground(A).

?- ['noteq.pl', 'solutions.pl'].
