%	$Header: sysneg.pl,v 1.3 85/09/23 18:58:28 lee Exp $

% Sound low level negation facilities - $if/4, $if_soft/4, $some/2
% $is_eq/4 will be in system and $solutions/4 is in solutions.pl
%
% Will be simplified if "ground" is incorporated into whens.
% May make two versions of this - one for safe uses, which
% has a faster cut implementation or an extra call to clobber trail.
%
% Needs ground.pl
%
% Lee 8/9/85


	% $if/4 - used to implement if and not
	% Wait until globals are ground then use standard way.
	% This should be open coded.
	% Could have seperate version of $if/4 which uses a more
	% efficient form of cut which assumes the call has no
	% global vars.
?- use_if muprolog.
	$if(Glob, Cond, Then, Else) :-
		$nground(Glob).
	$if(Glob, Cond, Then, Else) :-
		(Cond, !, Then ; Else).
?- use_else.
	%?- $if(Glob, Cond, Then, Else) when ground(Glob). % not implemented
	$if(Glob, Cond, Then, Else) :-
		when_ground(Glob, (Cond, !, Then ; Else)).
?- use_end.

	% version with soft cut (needed when then part shares quantified vars)
?- use_if muprolog.
	$if_soft(Glob, Cond, Then, Else) :-
		$nground(Glob).
	$if_soft(Glob, Cond, Then, Else) :-
		Cond,
		$soft_cut,
		Then.
	$if_soft(Glob, Cond, Then, Else) :-
		 Else.
?- use_else.
	%?- $if_soft(Glob, Cond, Then, Else) when ground(Glob). % not imp
	$if_soft(Glob, Cond, Then, Else) :-
		when_ground(Glob, (Cond, $soft_cut, Then ; Else)).
			% if soft cut is not available:
		%when_ground(Glob, (\+Cond, !, Else ; Cond, Then)).
?- use_end.

	% IF the globals are ground then call Goal followed by cut.
	% Otherwise call Goal without cut.  Basically, a safe version
	% of some.
$some(Glob, Goal) :-
	ground(Glob),
	!,
	Goal,
	!.
$some(Glob, Goal) :-
	Goal.
