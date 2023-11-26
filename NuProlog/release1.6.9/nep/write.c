/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include <ctype.h>
#include "mu.h"
#include <setjmp.h>

/*
 * Character c can be part of an unquoted atom whose first character
 * is of charType first.
 */
#define canFollow(c, first) \
	(	first == charTypes[c] \
	||	first == 'a' && (charTypes[c] == '0' || charTypes[c] == 'A'))

static void termToString(), showString();

static int
needsQuotes(atom, s)
int atom;					/* s is being printed as an atom. */
register char *s;
{
	register int first;

	/*
	 * Don't quote ',', ';', or '!' when printed as operators or functors.
	 * Don't quote '[]' when printed as an atom.
	 */
	if(s[0] == '\0')
		return(1);
	else if(s[1] == '\0') {
		if(!atom && (s[0] == ',' || s[0] == ';') || s[0] == '!')
			return(0);
	} else if(s[2] == '\0') {
		if(atom && s[0] == '[' && s[1] == ']')
			return(0);
	}
	if(*s <= 0 || *s > MAXCHAR)
		return(1);
	first = charTypes[*s];

	switch(first) {
	when 'a':
	case '=':
		do {
			s++;
			if(*s < 0 || *s > MAXCHAR)
				return(1);
		} while(canFollow(*s, first));
		return(*s != '\0');

	break;
	default:
		return(1);
	}
}

/*
 * Returns the index of the first character that cannot be part
 * of a printed string written with characterEscapes specified by
 * esc.  Otherwise returns 0.
 */
static int
isNotPrintableString(esc, x)
register int esc;
register Object x;
{
	register int i;

	esc = esc ? 0 : 3;

	for(i = 1; ; i++) {
		DeRef(x);
		if(IsNIL(x))
			return(0);
		else if(IsString(x)) {
			register char *s;

			for(s = (char *) eRef(x); *s != '\0'; s++, i++)
				if(*s <= 0 || *s > MAXCHAR || isPrint[*s] <= esc)
					return(i);
			return(0);
		} else if(IsList(x)) {
			register Object c, *l;

			l = eRef(x);
			c = l[0];
			x = l[1];
			if(!IsSmallInt(c))
				return(i);
			c = eSmallInt(c);
			if(c <= 0 || c > MAXCHAR || isPrint[c] <= esc)
				return(i);
		} else
			return(i);
	}
}

/*
 * Pointers to the base of the string being built by p_termToString
 * and to the end of the memory area available for it.
 */
static char *BOS, *TOS;

/* Flags, vars, base and character escapes for termToString. */
static int flagList, flagString, flagQuote, flagQuoteAll, flagOps;
static int vars, base, esc;

#define showChar(c) { if(BOS >= TOS) return; *BOS++ = c; }

static char *
intToDigits(b, base, n)
char *b;
register Word base, n;
{
	register char *bp;
	register int shift;

	bp = b;
	switch(base) {
	when 2: shift = 1;
	when 4: shift = 2;
	when 8: shift = 3;
	when 16: shift = 4;
	when 32: shift = 5;
	break;
	default:
		do {
			*bp++ = n % base;
			n /= base;
		} while(n > 0);
		return(bp);
	}

	base -= 1;
	do {
		*bp++ = n & base;
		n >>= shift;
	} while(n > 0);
	return(bp);
}

/*
 * BUG!  - maxint - 1 blows up of course, but as it isn't a
 * representable Prolog integer, this is not yet a problem.
 */
static void
showInt(base, n)
register Word base, n;
{
	char b[256];
	register char *bp;

	if(n < 0) {
		register int i;

		showChar('-');
		i = n % base;			/* Handle -maxint-1 */
		if(i < 0)				/* Who knows what all those C compilers do? */
			i = -i;
		n /= base;
		b[0] = i;
		if(n < 0) {
			n = -n;
			bp = intToDigits(b + 1, base, n);
		} else
			bp = b + 1;
	} else
		bp = intToDigits(b, base, n);

	n = bp - b;
	do {
		register int c;

		c = *--bp;
		showChar(c + (c < 10 ? '0' : 'A' - 10));
	} while(--n > 0);
}

static void
showVar(n)
register Word n;
{
	register char *bp;
	char b[256];

	showChar('_');

	bp = intToDigits(b, 26, n);
	n = bp - b;
	do {
		showChar(*--bp + 'A');
	} while(--n > 0);
}

static void
showNumberedVar(x)
register Object x;
{
	register Object y;

	y = eRef(x)[1]; DeRef(y);
	if(IsInt(y)) {
		register int n;

		n = eInt(y);
		showChar(n % 26 + 'A');
		if(n >= 26)
			showInt(10, n / 26);
	} else if(IsString(y)) {
		register char *s;

		s = (char *) eRef(y);
		while(*s != '\0')
			showChar(*s++);
	} else if(IsList(y) && !isNotPrintableString(0, y)) {
		/* BUG?
		 * !IsNotPrintableString(0, y) is not quite what we want here,
		 * but if people want "variable names" that aren't syntactically
		 * variables, then why stop them.
		 */
		register Object *l;

		for( ; ; ) {
			l = eRef(y);
			showChar(eSmallInt(deRef(l[0])));
			y = l[1]; DeRef(y);
			if(IsNIL(y))
				break;
			else if(IsString(y)) {
				register char *s;

				s = (char *) eRef(y);
				while(*s != '\0')
					showChar(*s++);
				break;
			}
		}
	} else if(IsAtom(y)) {
		register char *s;

		s = eCharStar(((Atom *) eRef(y))->a_pname);
		while(*s != '\0')
			showChar(*s++);
	} else {
		showString('\'', 0, eCharStar(eFunctor(*eRef(x))->a_pname));
		showChar('(');
		termToString(999, y);
		showChar(')');
	}
}

static void
showEscChar(quote, esc, c)
register int quote, esc, c;
{
	if(c == quote)
		showChar(quote);
	if(esc) {
		switch(isPrint[c]) {
		when 0:
			showChar('\\');
			showChar('^');
			showChar((c & 037) + '@');
		when 1:
		case 2: {
			register int d;

			switch(c) {
			when 8: d = 'b';
			when 10: d = 'n';
			when 11: d = 'v';
			when 12: d = 'f';
			when 13: d = 'r';
			when 27: d = 'e';
			when 127: d = 'd';
			break;
			default:
				panic("Impossible character in showEscChar()");
			}
			showChar('\\');
			showChar(d);
		}
		when 3:
		case 4:
			if(c == '\\')
				showChar('\\');
			showChar(c);
		}
	} else
		showChar(c);
}

static void
showQuotedString(quote, s)
register int quote;
register char *s;
{
	showChar(quote);
		while(*s != '\0')
			showEscChar(quote, esc, *s++);
	showChar(quote);
}

static void
showString(quote, atom, s)
int quote, atom;
register char *s;
{
	switch(quote) {
	when '\'':
		if(flagQuoteAll || flagQuote && needsQuotes(atom, s))
			showQuotedString('\'', s);
		else
			while(*s != '\0')
				showChar(*s++);
	when '"':
		showQuotedString('"', s);
	break;
	default:
		panic("Impossible quote character in showString()");
	}
}

/*
 * Returns:
 * 		0 if x is not an operator
 * 		1 if x has a prefix operator declaration
 * 		2 if x has a binary prefix operator declaration
 * 		3 if x has a postfix operator declaration
 * 		4 if x has an infix operator declaration
 * and assigns the precedence and left and right flags to
 * prec, left, and right.
 * No error checking!
 */
static int
isOp(x, i, prec, left, right)
Atom *x;
register int i;
int *prec, *left, *right;
{
	register Object *op;

	if(x->a_prec == NIL)
		return(0);
	op = eRef(x->a_prec);
	*prec = eSmallInt(op[1]);
	while(i < 4) {
		i++;
		if(op[1 + i] == StarToAtom(&SymY)) {
			if(i == 2 || i == 4) {
				*left = eSmallInt(op[4 + i]);
				*right = eSmallInt(op[5 + i]);
			} else
				*left = eSmallInt(op[5 + i]);
			return(i);
		}
	}
	return(0);
}

/*
parser$convert2(fx,  Prec, op(Prec, y, _, _, _, 1, _, _, _)).
parser$convert2(fy,  Prec, op(Prec, y, _, _, _, 0, _, _, _)).
parser$convert2(fxx, Prec, op(Prec, _, y, _, _, 1, 1, _, _)).
parser$convert2(fyx, Prec, op(Prec, _, y, _, _, 0, 1, _, _)).
parser$convert2(fxy, Prec, op(Prec, _, y, _, _, 1, 0, _, _)).
parser$convert2(xf,  Prec, op(Prec, _, _, y, _, _, _, 1, _)).
parser$convert2(yf,  Prec, op(Prec, _, _, y, _, _, _, 0, _)).
parser$convert2(xfx, Prec, op(Prec, _, _, _, y, _, _, 1, 1)).
parser$convert2(yfx, Prec, op(Prec, _, _, _, y, _, _, 0, 1)).
parser$convert2(xfy, Prec, op(Prec, _, _, _, y, _, _, 1, 0)).
*/

/*
 * Show a Structure with functor/arity and arguments at s.
 *
 * This verbose way of passing the arguments is to avoid having
 * to convert cons cells into Structures.  To do otherwise would
 * require the recursive allocation of space for the conversion.
 * On some machines auto variables are in a different area of memory
 * to the rest of the NU-Prolog areas, making finding space for
 * the conversion difficult.
 *
 * Note that because of difficulties with the '. ' term terminator,
 * real numbers, characters like '+', and the the fact that '.'
 * conventionally has no surrounding spaces when used as an operator,
 * showOp() is very conservative about printing '.' as an operator.
 */
static void
showOp(inprec, arity, functor, s)
int inprec;
int arity;
Atom *functor;
register Object *s;
{
	register int dot;
	int i, prec, left, right;

	i = 0;
	if(flagOps && arity <= 2) {
		do {
			i = isOp(functor, i, &prec, &left, &right);
		} while(i > 0 && ((i ^ arity) & 01) != 0);
	}
	dot = (!flagQuoteAll && strcmp(".", eCharStar(functor->a_pname)) == 0);
	if(i == 0 || dot && i != 4) {
		if(functor == &SymComma && flagQuote && inprec < 1000)
			showQuotedString('\'', ",");
		else
			showString('\'', 0, eCharStar(functor->a_pname));
		showChar('(');
		for(i = 0; i < arity; i++) {
			if(i > 0) {
				showChar(','); showChar(' ');
			}
			termToString(999, *s++);
		}
		showChar(')');
	} else {
		char *OLDBOS, *OLDBOS2;

		OLDBOS = BOS;			/* If dot, may need to re-do as a term. */
		if(prec > inprec)
			showChar('(');
		switch(i) {
		when 1:			/* prefix */
			showString('\'', 0, eCharStar(functor->a_pname));
			showChar(' ');
			termToString(prec - left, *s);
		when 2:			/* binary prefix */
			showString('\'', 0, eCharStar(functor->a_pname));
			showChar(' ');
			termToString(prec - left, s[0]);
			showChar(' ');
			termToString(prec - right, s[1]);
		when 3:			/* postfix */
			termToString(prec - left, *s);
			showChar(' ');
			showString('\'', 0, eCharStar(functor->a_pname));
		when 4:			/* infix */
			termToString(prec - left, s[0]);
			if(		dot
				&&	(	charTypes[BOS[-1]] == '='
					||	charTypes[BOS[-1]] == '0')) {
				BOS = OLDBOS;
				showChar('.'); showChar('(');
				termToString(999, s[0]);
				showChar(','); showChar(' ');
				termToString(999, s[1]);
				showChar(')');
				return;
			}
			if(functor != &SymComma && functor != &SymSemiColon && !dot)
				showChar(' ');
			showString('\'', 0, eCharStar(functor->a_pname));
			if(dot)
				OLDBOS2 = BOS;		/* Where the second arg begins. */
			else
				showChar(' ');
			termToString(prec - right, s[1]);
			if(dot && (charTypes[OLDBOS2[0]] == '=' || OLDBOS2[0] == '(')) {
				BOS = OLDBOS;
				showChar('.'); showChar('(');
				termToString(999, s[0]);
				showChar(','); showChar(' ');
				termToString(999, s[1]);
				showChar(')');
				return;
			}
		}
		if(prec > inprec)
			showChar(')');
	}
	return;
}

static void
termToString(inprec, x)
int inprec;
register Object x;
{
	DeRef(x);

start: 

	switch(eType(x)) {
	when tREF:
	case tDEL:
		showVar((Word) eRef(x) / sizeof(Word));
	when tICN:
		switch(eCType(x)) {
		when ctINT:
			showInt(base, eSmallInt(x));
		when ctATM: {
			register Atom *a;
			register int op;
			int prec, left, right;

			a = (Atom *) eRef(x);
			op = flagOps && isOp(a, 0, &prec, &left, &right) && prec > inprec;
			if(op)
				showChar('(');
			showString('\'', 1, eCharStar(a->a_pname));
			if(op)
				showChar(')');
		}
		when ctBLK:
			showChar('<');
			showInt(16, (Word) eRef(x));
			showChar('>');
		break;
		default:
			panic("Impossible constant type in termToString()");
		}
	when tUCN:
		switch(eCType(x)) {
		when ctI32:
			showInt(base, eInt32(x));
		when ctFLT: {
			char buf[50];
			register char *b;
			register int point;	/* Set when distinguished from an integer. */
			double f;

			f = eFloat(x);
			sprintf(buf, "%.15g", f);
			b = buf;
			if(*b == '-')
				showChar(*b++);
			if(*b == '.')
				showChar('0');
			if(isalpha(*b))
				point = 1;		/* Don't add ".0" to things like "NaN". */
			else
				point = 0;
			while(b[0] != '\0') {
				showChar(b[0]);
				if(b[0] == '.') {
					point = 1;
					if(!isdigit(b[1]))
						showChar('0');
				} else if(b[0] == 'e')
					point = 1;
				b++;
			}
			if(!point) {
				showChar('.');
				showChar('0');
			}
		}
		break;
		default:
			panic("Impossible UCN constant type in termToString()");
		}
	when tLST: {
		register Object *l;

		if(flagString && !isNotPrintableString(esc, x)) {
			showChar('"');
			for( ; ; ) {
				l = eRef(x);
				x = l[0]; DeRef(x);
				showEscChar('"', esc, eSmallInt(x));
				x = l[1]; DeRef(x);
				if(IsNIL(x))
					break;
				else if(IsString(x)) {
					register char *s;

					s = (char *) eRef(x);
					while(*s != '\0')
						showEscChar('"', esc, *s++);
					break;
				}
			}
			showChar('"');
		} else if(flagList) {
			showChar('[');
			l = eRef(x);
			termToString(999, l[0]);
			x = l[1]; DeRef(x);
			while(IsList(x)) {
				showChar(','); showChar(' ');
				l = eRef(x);
				termToString(999, l[0]);
				x = l[1]; DeRef(x);
			}
			if(!IsNIL(x)) {
				showChar('|');
				termToString(999, x);
			}
			showChar(']');
		} else
			showOp(inprec, 2, &SymCons, eRef(x));
	}
	when tCHR:
		if(flagString && !isNotPrintableString(esc, x))
			showString('"', 0, (char *) eRef(x));
		else if(flagList) {
			register char *s;

			showChar('[');
			s = (char *)eRef(x);
			showInt(base, *s);
			for(s++; *s != '\0'; s++) {
				showChar(','); showChar(' ');
				showInt(base, *s);
			}
			showChar(']');
		} else {
			register char *s;
			static Object dummy[2];

			s = (char *)eRef(x);
			dummy[0] = MakeSmallInt(*s);
			s++;
			if(*s == '\0')
				dummy[1] = NIL;
			else
				dummy[1] = StarToString(s);
			showOp(inprec, 2, &SymCons, dummy);
		}
	when tSTR: {
		register Object *s;

		s = eRef(x);
		if(		(vars && s[0] == StrHeader1_VAR)
			||	s[0] == StrHeader1Write_Var)
				showNumberedVar(x);
		else if(s[0] == StrHeader1Parentheses) {
			showChar('{');
			termToString(1200, s[1]);
			showChar('}');
		} else
			showOp(inprec, eNArgs(s[0]), eFunctor(s[0]), s + 1);
	}
	when tBMV:
		showChar(':');
		showInt(base, (Word) eRef(x));
		showChar(':');
	}
}

int
p_termToString(flags, x, o)
register Object flags, x;
Object *o;
{
	int initialPrecedence;
	register char *s;

	BOS = (char *)CMR->mr_h;
	TOS = (char *)heapMax - 1;

	DeRef(flags);
	if(!IsInt(flags))
		panic("Integer flags expected in p_termToString()");
	flags = eInt(flags);
	base = eBase(flags);
	initialPrecedence = ePrec(flags);
	if(initialPrecedence == 0)
		initialPrecedence = 1200;
	flags = eFlags(flags);
	flagList = flags & (1 << wflgLIST);
	flagString = flags & (1 << wflgSTRING);
	flagQuote = flags & (1 << wflgQUOTE);
	flagQuoteAll = flags & (1 << wflgQUOTEALL);
	flagOps = flags & (1 << wflgOPS);

	vars = testFlagOn(flgVARS);
	esc = testFlagOn(flgCHAR);

	s = BOS;
	termToString(initialPrecedence, x);
	*BOS++ = '\0';
	if(BOS >= TOS)
		return(0);
	CMR->mr_h = (Object *) ALIGNW(BOS);
	*o = StarToString(s);
	return(1);
}

#define BUF (8 * 1024)

void
writeTerm3(fp, flags, x)
FILE *fp;
register int flags;
Object x;
{
	char buffer[BUF];
	int initialPrecedence;
	register char *b;

	BOS = buffer;
	TOS = buffer + BUF - 1;

	base = eBase(flags);
	initialPrecedence = ePrec(flags);
	if(initialPrecedence == 0)
		initialPrecedence = 1200;
	flags = eFlags(flags);
	flagList = flags & (1 << wflgLIST);
	flagString = flags & (1 << wflgSTRING);
	flagQuote = flags & (1 << wflgQUOTE);
	flagQuoteAll = flags & (1 << wflgQUOTEALL);
	flagOps = flags & (1 << wflgOPS);

	vars = 1;
	esc = 1;

	termToString(initialPrecedence, x);
	if(BOS >= TOS) {
		warning("Out of memory in writeTerm3() -- term too big to write");
		return;
	}
	fwrite(buffer, 1, TOS - BOS, fp);		/* BUG!  Should be putcStream() */
}

void
writeTerm(fp, x)
FILE *fp;
Object x;
{
	writeTerm3(	
		fp,
		MakeFlags(10, 1200, (1<<wflgLIST) || (1<<wflgSTRING) || (1<<wflgOPS)),
		x);
}
