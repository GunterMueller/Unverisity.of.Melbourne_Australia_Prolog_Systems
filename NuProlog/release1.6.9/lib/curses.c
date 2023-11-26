/*
 *	Copyright (C) 1986, The University of Melbourne
 *
 *	NU-Prolog interface to the Unix curses library
 *
 *	Author: Giles Lean, February 1988
 */

#ifdef sgi
#undef BSD4
#endif /* sgi */

#include <curses.h>
#include "public.h"

static char	buffer[256];

/********************************/
/*	curses constants	*/
/********************************/

WINDOW *
nu_curscr()
{
	return(curscr);
}

WINDOW *
nu_stdscr()
{
	return(stdscr);
}

char *
def_term()
{
#ifdef BSD4
	return(Def_term);
#else /* BSD4 */
	warning("curses: Def_term not supported.");
	return((char *) NULL);
#endif /* BSD4 */
}

my_term()
{
#ifdef BSD4
	return((Word) My_term);
#else /* BSD4 */
	warning("curses: My_term not supported.");
	return(ERR);
#endif /* BSD4 */
}

set_my_term(Boolf)
{
#ifdef BSD4
	My_term = Boolf;
#else /* BSD4 */
	warning("curses: My_term not supported.");
#endif /* BSD4 */
}

char *
nu_ttytype()
{
	return(ttytype);
}

lines()
{
	return((Word) LINES);
}

cols()
{
	return((Word) COLS);
}

err()
{
	return((Word) ERR);
}

ok()
{
	return((Word) OK);
}

nu_true()
{
	return((Word) TRUE);
}

nu_false()
{
	return((Word) FALSE);
}

/****************************************/
/*	handle curses macros here	*/
/****************************************/

nu_clearok(scr, boolf)
WINDOW	*scr;
bool	boolf;
{
	clearok(scr, boolf);
}

#ifndef MACHINE_SGI
nu_flushok(win, boolf)
WINDOW	*win;
bool	boolf;
{
	flushok(win, boolf);
}
#endif /* MACHINE_SGI */

nu_standout()
{
	standout();
}

nu_standend()
{
	standend();
}

nu_cbreak()
{
	crmode();
}

nu_nocbreak()
{
	nocrmode();
}

nu_echo()
{
	echo();
}

nu_noecho()
{
	noecho();
}

nu_raw()
{
	raw();
}

nu_noraw()
{
	noraw();
}

nu_getyx(win, y, x)
WINDOW	*win;
Word	*y, *x;
{
	getyx(win, *y, *x);
}

Word
nu_winch(win)
WINDOW	*win;
{
	return(winch(win));
}

nu_leavok(win, boolf)
WINDOW	*win;
bool	boolf;
{
	leaveok(win, boolf);
}

nu_nlon()
{
	nl();
}

nu_nonl()
{
	nonl();
}

nu_scrollok(win, boolf)
WINDOW	*win;
bool	boolf;
{
	scrollok(win, boolf);
}

char *
nu_unctrl(ch)
char	ch;	
{
	return(unctrl(ch));
}

nu_savetty()
{
	savetty();
}

nu_resetty()
{
	resetty();
}

/********************************/
/*	Miscellaneous Stuff	*/
/********************************/

Word
nu_wgetstr(win, str)
WINDOW	*win;
char	**str;
{
	int r;

	buffer[255] = '\0';
	r = wgetstr(win, buffer);
	*str = buffer;
	return(r);
}

nu_longname(name)
char	**name;
{
	int	r;
	char	tcapbuf[1024];

	extern longname(char *, char *);
	buffer[255] = '\0';
	r = tgetent(tcapbuf, getenv("TERM"));
	if (r == 1) {
		longname(tcapbuf, buffer);
		*name = buffer;
		return((Word) 1);
	} else
		return((Word) 0);
}

nu_fullname(name)
char	**name;
{
	return(nu_longname(name));
}

#ifdef BSD4
#include <sgtty.h>
#else /* BSD4 */
#include <termio.h>
#endif /* BSD4 */

static char erase_char, kill_char;
static char baud_rate;
/* static char werase_char, eof_char; */

static void
init_chars()
{
#ifdef BSD4
	struct sgttyb ttya;
	register int succ;
/*
	struct ltchars ttyb;
	struct tchars ttyc;
*/

	succ = (ioctl(0, (int) TIOCGETP, (char *) &ttya) >= 0);
/*
	succ = succ && (ioctl(0, (int) TIOCGLTC, (char *) &ttyb) >= 0);
	succ = succ && (ioctl(0, (int) TIOCGETC, (char *) &ttyc) >= 0);
*/
	if(!succ) {
		warning("NU-Prolog curses: can't find terminal characteristics");
		return;
	}

	erase_char = ttya.sg_erase;
	kill_char = ttya.sg_kill;
	baud_rate = ttya.sg_ospeed;
/*
	werase_char = ttyb.t_werasc;
	eof_char = ttyc.t_eofc;
*/
#else /* BSD4 */
	struct termio ttya;
	register int succ;

	succ = (ioctl(0, TCGETA, &ttya) >= 0);
	if(!succ) {
		warning("NU-Prolog curses: can't find terminal characteristics");
		return;
	}

	erase_char = ttya.c_cc[VERASE];
	kill_char = ttya.c_cc[VKILL];
	baud_rate = ttya.c_cflag & CBAUD;
/*
	werase_char = CTRL(w);
	eof_char = CTRL(d);
*/
#endif /* BSD4 */
}

Word
nu_init_curses()
{
	Word retcode;
	WINDOW *initsrc;

	retcode = (initscr() == NULL);
	init_chars();
	return(retcode);
}

Word
nu_baudrate()
{
	return((Word)baud_rate);
}

Word
nu_erasechar()
{
	return((Word)erase_char);
}

Word
nu_killchar()
{
	return((Word)kill_char);
}
