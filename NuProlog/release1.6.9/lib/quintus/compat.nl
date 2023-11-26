/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985,1986 by it.
 * 
 * All rights are reserved.
 */

% Nepolog Quintus compatibility library.

atom_chars(X, Y) :-
	atomToString(X, Y).

current_atom(Atom) :-
	currentAtom(Atom).				% Modules?

current_predicate(Pred, Term) :-
	currentPredicate(Pred, Arity),
	functor(Term, Pred, Arity).

current_op(X, Y, Z) :-
	currentOp(X, Y, Z).

current_key(Key) :-
	currentKey(Key).

current_stream(X, Y, Z) :-
	currentStream(X, Y, Z).

current_input(X) :-
	currentInput(X).

current_output(X) :-
	currentOutput(X).

set_input(X) :-
	setInput(X).

set_output(X) :-
	setOutput(X).

character_count(S, X) :-
	characterCount(S, X).

line_count(S, X) :-
	lineCount(S, X).

line_position(S, X) :-
	linePosition(S, X).

stream_position(S, Old, New) :-
	streamPosition(S, Old, New).

flush_output(X) :-
	flushOutput(X).

predicate_property(Spec, Property) :-
	functor(Spec, Pred, Arity),
	predicateProperty(Pred, Arity, Property).

absolute_file_name(RelFileSpec, AbsFileSpec) :-
	absoluteFileName(RelFileSpec, AbsFileSpec).

source_file(FileSpec) :-
	sourceFile(FileSpec).

source_file(PredSpec, FileSpec) :-
	functor(PredSpec, Pred, Arity),
	sourceFile(Pred, Arity, FileSpec).

prolog_flag(FlagName, Value) :-
	compat$prolog_flags(FlagName, RealFlagName),
	prologFlag(RealFlagName, Value).

prolog_flag(FlagName, OldValue, NewValue) :-
	once compat$prolog_flags(FlagName, RealFlagName),
	prologFlag(RealFlagName, OldValue, NewValue).

compat$prolog_flags(character_escapes, characterEscapes).
compat$prolog_flags(fileerrors, fileErrors).
compat$prolog_flags(K, K).

fileerrors :-
	fileErrors.

nofileerrors :-
	noFileErrors.

keysort(L, M) :-
	keySort(L, M).

halt :-
	exit(0).

false :-
	fail.

otherwise.

incore(Goal) :-
	call(Goal).

gc.

%	BUG!  This might still break programs.
gcguide(_, _, _).

nogc.

write_canonical(X) :-
	writeCanonical(X).

write_canonical(S, X) :-
	writeCanonical(S, X).

trimcore.

statistics(Key, Value) :-
	statistics(S),
	compat$statisticsKey(Key, RealKey),
	member(RealKey = Value, S).

compat$statisticsKey(Key, Key).				% Use all the NU-Prolog keys too.
compat$statisticsKey(runtime, time).
compat$statisticsKey(core, memory).
compat$statisticsKey(heap, program).
compat$statisticsKey(global_stack, global).
compat$statisticsKey(local_stack, local).

%   File   : unix.nl
%   Author : Richard A. O'Keefe
%   Updated: %G%
%	Fiddled with by: Jeff Schultz
%   Purpose: Addition for NU-Prolog Quintus compatibility package.

/*  The NU-Prolog lib(compat) package does not define the unix/1 predicate.
    NU Prolog does, however, have most of the operations we need.  The
    main thing which is missing is unix(argv(ListOfConstants)) -- Quintus
    made that part of the unix interface rather than part of the "how to
    call a Prolog top level" interface because Quintus Prolog was meant
    to be portable to VMS, CMS, and MVS, which do command line interfaces
    differently from UNIX and from each other.  NU Prolog currently exists
    only in UNIX versions.

    unix/1 in Quintus Prolog reports errors rather better than this version
    does, but that can be left to others to take care of.

    Note that there is no
	unix(cwd(Dir))
    query.  You can find out the absolute file name of the current directory
	by calling
	absolute_file_name('', Dir)
    and this should work if there are directories at all.
*/

unix(Arg) :-
	nonvar(Arg),		% report error if not callable(Arg)
	compat$unix(Arg).

compat$unix(cd) :-
	getenv('HOME', Dir),
	chdir(Dir).
compat$unix(cd(Dir)) :-
	atom(Dir),					% report error if not atom(Dir)
	chdir(Dir).
compat$unix(shell) :-
	system('${SHELL:/bin/sh}').
compat$unix(shell(Cmd)) :-
	atom(Cmd),					% report error if not atom(Cmd)
	system('${SHELL:/bin/sh}', ['-c',Cmd], _).
compat$unix(shell(Cmd, Status)) :-	% this one comes from SICStus Prolog
	atom(Cmd),					% report error if not atom(Cmd)
	system('${SHELL:/bin/sh}', ['-c',Cmd], Status).
compat$unix(system(Cmd)) :-	
	atom(Cmd),					% report error if not atom(Cmd)
	system(Cmd).
compat$unix(system(Cmd, Status)) :-	% this one comes from SICStus Prolog
	atom(Cmd),					% report error if not atom(Cmd)
	system(Cmd, Status).
compat$unix(argv([])).			% can't be done properly
compat$unix(exit(Status)) :-	% this one comes from SICStus Prolog
	integer(Status),			% report error if not integer(Status)
	exit(Status).
compat$unix(access(Path,Mode)) :-	% this one comes from SICStus Prolog
	atom(Path),					% report error if not atom(Path)
	integer(Mode),				% report error if not integer(Mode)
	access(Path, Mode).			% Note: real wizards know _not_ to use access()

copy_term(T1, T2) :-
	duplicate(T1, T2).
