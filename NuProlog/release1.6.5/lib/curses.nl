%	Copyright (C) 1986, The University of Melbourne
%
%	NU-Prolog interface to the Unix curses library
%
%	Author: Giles Lean, February 1988

:- useIf prologFlag(machine, sgi).
foreignFile('curses.o', [nu_curscr, nu_stdscr, def_term, my_term, nu_ttytype,
	lines, cols, err, ok, nu_true, nu_false,
	waddch, waddstr, box,
	wclear, nu_clearok, wclrtobot,
	wclrtoeol, wdelch, wdeleteln, werase,
	idlok, winsch, winsertln,
	wmove, overlay, overwrite, wrefresh,
	wstandout, wstandend, nu_cbreak, nu_nocbreak,
	nu_echo, nu_noecho, wgetch,
	nu_wgetstr, nu_raw, nu_noraw, nu_baudrate, delwin, endwin,
	nu_erasechar, nu_getyx, nu_winch, nu_init_curses,
	nu_killchar, nu_leavok, nu_longname, nu_fullname, mvwin, newwin,
	nu_nlon, nu_nonl, nu_scrollok, touchline, touchwin,
	subwin, nu_unctrl, gettmode, mvcur, scroll, nu_savetty, nu_resetty,
	setterm]).
:- useElse.
foreignFile('curses.o', [nu_curscr, nu_stdscr, def_term, my_term, nu_ttytype,
	lines, cols, err, ok, nu_true, nu_false,
	waddch, waddstr, box,
	wclear, nu_clearok, wclrtobot,
	wclrtoeol, wdelch, wdeleteln, werase,
	nu_flushok, idlok, winsch, winsertln,
	wmove, overlay, overwrite, wrefresh,
	wstandout, wstandend, nu_cbreak, nu_nocbreak,
	nu_echo, nu_noecho, wgetch,
	nu_wgetstr, nu_raw, nu_noraw, nu_baudrate, delwin, endwin,
	nu_erasechar, getcap, nu_getyx, nu_winch, nu_init_curses,
	nu_killchar, nu_leavok, nu_longname, nu_fullname, mvwin, newwin,
	nu_nlon, nu_nonl, nu_scrollok, touchline, touchoverlap, touchwin,
	subwin, nu_unctrl, gettmode, mvcur, scroll, nu_savetty, nu_resetty,
	setterm, tstp]).
:- useEnd.

foreign(nu_curscr, curscr([- pointer])).
foreign(nu_stdscr, stdscr([- pointer])).
foreign(def_term, def_term([- string])).
foreign(my_term, $my_term([- integer])).
foreign(set_my_term, $set_my_term(+ integer)).
foreign(nu_ttytype, ttytype([- string])).
foreign(lines, lines([- integer])).
foreign(cols, cols([- integer])).
foreign(err, err([- integer])).
foreign(ok, ok([- integer])).
foreign(nu_true, true([- integer])).
foreign(nu_false, false([- integer])).
foreign(waddch, $waddch(+ pointer, + integer, [- integer])).
foreign(waddstr, $waddstr(+ pointer, + string, [- integer])).
foreign(box, box(+ pointer, + integer, + integer)).
foreign(wclear, wclear(+ pointer)).
foreign(nu_clearok, clearok(+ pointer, + integer)).
foreign(wclrtobot, wclrtobot(+ pointer)).
foreign(wclrtoeol, wclrtoeol(+ pointer)).
foreign(wdelch, wdelch(+ pointer)).
foreign(wdeleteln, wdeleteln(+ pointer)).
foreign(werase, werase(+ pointer)).
foreign(idlok, idlok(+ pointer, + integer)).
foreign(winsch, $winsch(+ pointer, + integer, [- integer])).
foreign(winsertln, winsertln(+ pointer)).
foreign(wmove, $wmove(+ pointer, + integer, + integer, [- integer])).
foreign(overlay, overlay(+ pointer, + pointer)).
foreign(overwrite, overwrite(+ pointer, + pointer)).
foreign(wrefresh, $wrefresh(+ pointer, [- integer])).
foreign(wstandout, wstandout(+ pointer)).
foreign(wstandend, wstandend(+ pointer)).
foreign(nu_cbreak, cbreak).
foreign(nu_nocbreak, nocbreak).
foreign(nu_echo, echo).
foreign(nu_noecho, noecho).
foreign(wgetch, $wgetch(+ pointer, [- integer])).
foreign(nu_wgetstr, $wgetstr(+ pointer, - string, [- integer])).
foreign(nu_raw, raw).
foreign(nu_noraw, noraw).
foreign(nu_baudrate, baudrate([- integer])).
foreign(delwin, delwin(+ pointer)).
foreign(endwin, endwin).
foreign(nu_erasechar, erasechar([- integer])).
foreign(nu_getyx, getyx(+ pointer, - integer, - integer)).
foreign(nu_winch, winch(+ pointer, [- integer])).
foreign(nu_init_curses, $init_curses([- integer])).
foreign(nu_killchar, killchar([- integer])).
foreign(nu_leavok, leaveok(+ pointer, + integer)).
foreign(nu_longname, $longname(- string, [- integer])).
foreign(nu_fullname, $fullname(- string, [- integer])).
foreign(mvwin, $mvwin(+ pointer, + integer, + integer, [- integer])).
foreign(newwin,
	newwin(+ integer, + integer, + integer, + integer, [- pointer])).
foreign(nu_nlon, nlon).
foreign(nu_nonl, nonl).
foreign(nu_scrollok, scrollok(+ pointer, + integer)).
foreign(touchline, touchline(+ pointer, + integer, + integer, + integer)). 
foreign(touchwin, touchwin(+ pointer)).
foreign(subwin, subwin(
		+ pointer, + integer, + integer,
		+ integer, + integer, [- pointer])). 
foreign(nu_unctrl, unctrl(+ integer, [- string])).
foreign(gettmode, gettmode).
foreign(mvcur, mvcur(+ integer, + integer, + integer, + integer)).
foreign(scroll, scroll(+ pointer)).
foreign(nu_savetty, savetty).
foreign(nu_resetty, resetty).
foreign(setterm, setterm(+ string)).

:- useIf \+ prologFlag(machine, sgi).
foreign(getcap, getcap(+ string, [- string])).
foreign(touchoverlap, touchoverlap(+ pointer, + pointer)).
foreign(tstp, tstp).
foreign(nu_flushok, flushok(+ pointer, + integer)).
:- useEnd.

% ---------------- %
% Curses Constants %
% ---------------- %

my_term(V) :-
	( var(V)
	->	$my_term(V)
	;	$set_my_term(V)
	).

% ---------------- %
% Output Functions %
% ---------------- %

addch(Ch) :-
	stdscr(Win),
	waddch(Win, Ch).

waddch(Win, Ch) :-
	ok(OK),
	$waddch(Win, Ch, OK).

addstr(Str) :-
	stdscr(Win),
	waddstr(Win, Str).

waddstr(Win, Ch.Str) :-
	ok(OK),
	$waddch(Win, Ch, OK),
	waddstr(Win, Str).
waddstr(_Win, []).

clear :-
	stdscr(Win),
	wclear(Win).

clrtobot :-
	stdscr(Win),
	wclrtobot(Win).

clrtoeol :-
	stdscr(Win),
	wclrtoeol(Win).

delch :-
	stdscr(Win),
	wdelch(Win).

deleteln :-
	stdscr(Win),
	wdeleteln(Win).

erase :-
	stdscr(Win),
	werase(Win).

insch(C) :-
	stdscr(Win),
	winsch(Win, C).

winsch(Win, C) :-
	ok(OK),
	$winsch(Win, C, OK).

insertln :-
	stdscr(Win),
	winsertln(Win).

move(Y, X) :-
	stdscr(Win),
	wmove(Win, Y, X).

wmove(Win, Y, X) :-
	ok(OK),
	$wmove(Win, Y, X, OK).

% printw and wprintw are not included -- variable number of arguments
% versions of format and write are.

refresh :-
	stdscr(Win),
	wrefresh(Win).
	
wrefresh(Win) :-
	ok(OK),
	$wrefresh(Win, OK).

standout :-
	stdscr(Win),
	wstandout(Win).


standend :-
	stdscr(Win),
	wstandend(Win).

% --------------- %
% Input Functions %
% --------------- %

crmode :-
	cbreak.
nocrmode :-
	crmode.

getch(Ch) :-
	stdscr(Win),
	wgetch(Win, Ch).

wgetch(Win, Ch) :-
	$wgetch(Win, Ch),
	err(ERR),
	Ch \== ERR.

getstr(Str) :-
	stdscr(Win),
	wgetstr(Win, Str).

wgetstr(Win, Str) :-
	ok(OK),
	$wgetstr(Win, Str, OK).

% scan and wscan not included -- variable number of arguments
% getstr still is.

% ----------------------- %
% Miscellaneous Functions %
% ----------------------- %

inch(Ch) :-
	stdscr(Win),
	winch(Win, Ch).

initscr :-
	$init_curses(Ret),
	Ret \== 0.

longname(Name) :-
	ok(OK),
	$longname(Name, OK).

fullname(Name) :-
	ok(OK),
	$fullname(Name, OK).

mvwin(Win, Y, X) :-
	ok(OK),
	$mvwin(Win, Y, X, OK).

% ------- %
% Details %
% ------- %

:- initializing,
	libdirectory(Dir),
	name(Dir, DirName),
	append(DirName, "curses.o", FileName),
	prologFlag(machine, Machine),
	( member(Machine, [sgi, mips, dec]) ->
		Libraries = ['-lcurses_G0', '-ltermcap_G0']
	;	Libraries = ['-lcurses', '-ltermcap']
	),
	loadForeignFiles([FileName], Libraries).

% --------------- %
% write and stuff %
% --------------- %

writew(Term) :-
	termToString(Term, String),
	stdscr(Win),
	waddstr(Win, String).

writew(Win, Term) :-
	termToString(Term, String),
	waddstr(Win, String).

writelnw(Term) :-
	stdscr(Win),
	writew(Win, Term),
	nlw(Win).

writelnw(Win, Term) :-
	writew(Win, Term),
	nlw(Win).

writevw(Flags, Term) :-
	termToString(Flags, Term, String),
	stdscr(Win),
	waddstr(Win, String).

writevw(Win, Flags, Term) :-
	termToString(Flags, Term, String),
	waddstr(Win, String).

nlw :-
	stdscr(Win),
	waddch(Win, 0'\n).

nlw(Win) :-
	waddch(Win, 0'\n).

formatw(Format, Args) :-
	stdscr(Win),
	sformat(Format, Args, String),
	waddstr(Win, String).

formatw(Win, Format, Args) :-
	sformat(Format, Args, String),
	waddstr(Win, String).

% -------------------- %
% mv prefixed variants %
% -------------------- %

mvaddch(Y, X, Ch) :-
	stdscr(Win),
	wmove(Win, Y, X),
	waddch(Win, Ch).

mvwaddch(Win, Y, X, Ch) :-
	wmove(Win, Y, X),
	waddch(Win, Ch).

mvgetch(Y, X, Ch) :-
	stdscr(Win),
	wmove(Win, Y, X),
	wgetch(Win, Ch).

mvwgetch(Win, Y, X, Ch) :-
	wmove(Win, Y, X),
	wgetch(Win, Ch).

mvgetstr(Y, X, Str) :-
	stdscr(Win),
	wmove(Win, Y, X),
	wgetstr(Win, Str).

mvwgetstr(Win, Y, X, Str) :-
	wmove(Win, Y, X),
	wgetstr(Win, Str).

mvaddstr(Y, X, Str) :-
	stdscr(Win),
	wmove(Win, Y, X),
	waddstr(Win, Str).

mvwaddstr(Win, Y, X, Str) :-
	wmove(Win, Y, X),
	waddstr(Win, Str).

mvwritew(X, Y, Term) :-
	stdscr(Win),
	wmove(Win, X, Y),
	writew(Win, Term).

mvwritew(Win, X, Y, Term) :-
	wmove(Win, Y, X),
	writew(Win, Term).

mvwriteln(X, Y, Term) :-
	stdscr(Win),
	wmove(Win, Y, X),
	writelnw(Win, Term).

mvwriteln(Win, X, Y, Term) :-
	wmove(Win, Y, X),
	writelnw(Win, Term).

mvformatw(Y, X, Format, Args) :-
	stdscr(Win),
	wmove(Win, Y, X),
	formatw(Win, Format, Args).

mvformatw(Win, Y, X, Format, Args) :-
	wmove(Win, Y, X),
	formatw(Win, Format, Args).

mvinch(Y, X, Ch) :-
	stdscr(Win),
	wmove(Win, Y, X),
	winch(Win, Ch).

mvwinch(Win, Y, X, Ch) :-
	wmove(Win, Y, X),
	winch(Win, Ch).

mvdelch(Y, X) :-
	stdscr(Win),
	wmove(Win, Y, X),
	wdelch(Win).

mvwdelch(Win, Y, X) :-
	wmove(Win, Y, X),
	wdelch(Win).

mvinsch(Y, X, Ch) :-
	stdscr(Win),
	wmove(Win, Y, X),
	winsch(Win, Ch).

mvwinsch(Win, Y, X, Ch) :-
	wmove(Win, Y, X),
	winsch(Win, Ch).

