/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

/*
io.c - functions for reading and writing terms
Most of this code was adapted from the C-PROLOG interpreter (see below).
The authors of C-PROLOG are not responsible for the mess I have made, any
bugs I have introduced or any bugs which were in the early version of the
code which I obtained. The current C-PROLOG code may now have no resemblance,
in form or function, to the code below.  If you ever want to convince someone
that C is a horrible language - just show them this file.  I would have run
cb on it, but it was so bad that cb dumped core (joke copyright I. Balbin).
*/
/**********************************************************************
*                                                                     *
*                  C Prolog                                           *
*                  ========                                           *
*                                                                     *
*  By Fernando Pereira, July 1982.                                    *
*  EdCAAD, Dept. of Architecture, University of Edinburgh.            *
*                                                                     *
*  Based on the Prolog system written in IMP by Luis Damas            *
*  for ICL 2900 computers, with some contributions by                 *
*  Lawrence Byrd.                                                     *
*                                                                     *
*  Copyright (C) 1982 Fernando Pereira, Luis Damas and Lawrence Byrd  *
*                                                                     *
**********************************************************************/
#include <signal.h>
#include <setjmp.h>
#include "types.h"
#include "dict.h"
#include "pred.h"
char *strcpy();

#define TRUE 1
#define FALSE 0
#define Words(c)	(((c)+sizeof(Int)-1)/sizeof(Int))
#define MAXWLEV	200	/* max depth of terms to be printed - to avoid */
			/* segmentation violations on silly machines   */

jmp_buf intrbuf;
int doljump;
extern Int intflag;
extern Int vcopy();

/* entry is read's variable dictionary */

typedef struct VarEntry {
	struct VarEntry *NextVar;
	Ptr VarValue;
    } VarEntry;

typedef VarEntry *VarP;

#define WDISP	1	/* display in prefix */
#define WQUOTE	2	/* quotes around funny functors */
#define	WSTRING	4	/* write strings where possible */
#define	WVARNUM	8	/* write numbers after variables */
#define	WDOT	16	/* write lists with dots */
#define WPAR	32	/* parentheses around non-alphanumeric consts */

#define Wdisp	(wflags & WDISP)
#define Wquote	(wflags & WQUOTE)
#define	Wstring	(wflags & WSTRING)
#define Wvarnum	(wflags & WVARNUM)
#define Wdot	(wflags & WDOT)
#define Wpar	(wflags & WPAR)

Int wflags = 12;

#define Get() ((*nextc)(arg) & 127)

static int (*nextc)(),		/* (*nextc)(arg) returns the next char. eg */
	**arg;			/* sget(&stringvar) or fget(file) */

#define Put(c)	 	if(!intflag) putc(c,outfile)
#define PutString(x)	if(!intflag) fprintf(outfile, "%s", x)
#define PutInt(i)       if(!intflag) fprintf(outfile, "%ld", (long)(i))

static FILE *outfile;

#define CtrlZ 26

Ptr lsp, lsz;
VarP varchain;
int isop(), NumberString();

#define PREFIX	0
#define INFIX	1
#define POSTFIX	2

#define Isatoz(atom) ('a' <= dname((int)atom)[0] && \
                          dname((int)atom)[0] <= 'z')
static
display(t, l)
	Ptr t;
	levtype l;
{
	int arg, m, m1;

	if(intflag)
		return;
	findbind(t, l, &t, &l);
	if(IsVar(t))
		pvar(t, l);
	else if(IsInt(t)) {
		PutInt(XtrInt(t));
	} else if(IsAtom(t))
		if (isop((int)atdict(t), PREFIX, &m, &m1, &m1) && m > 1000) {
			Put('(');
			patom(t);
			Put(')');
		} else
			patom(t);
	else if(ctdict(t) == Ddot && Wstring && IsString(t, l))
		wstring(t, l);
	else {
		patom((Ptr)*t);
		Put('(');
		for(arg = 1; arg <= tnargs(t); arg++) {
			display((Ptr)targ(arg, t), l);
			if(arg < tnargs(t)) {
				Put(',');
			}
		}
		Put(')');
	}
}

Ptr
tcopy(t, l)		/* copy a term - uses vcopy to copy vars */
	Ptr t;
	levtype l;
{
	register int i;
	register int n;
	register Ptr *c;

	Ptr retval;

	c = &retval;

	for(;;) {
		if(l > gbindings(goalstk))
			findbind(t, l, &t, &l);
		if(IsAtomic(t)) {
			*c = t;
			break;
		} else if(IsVar(t)) {
			*c = (Ptr) vcopy(t, l);
			break;
		} else {
			n = tnargs(t);
			*c = newmem(n+1);
			thead(*c) = thead(t);
			for(i = 1; i < n; i++)
				targ(i, *c) = (Int) tcopy(targ(i, t), l);
			c = (Ptr *) (n + *c);	/* Should be &targ(n, *c) */
			t = (Ptr) targ(n, t);
		}
	}
	return(retval);
}

/* decrease left priority flag */

#define dlprflg	0x2000

/* decrease rigth priority flag */

#define drprflg	0x1000

/* priority field */

#define mskprty 0x0fff

/* Character types for tokeniser */

#define UC	1		/* Upper case alphabetic */
#define UL	2		/* Underline */
#define LC	3		/* Lower case alphabetic */
#define N	4		/* Digit */
#define QT	5		/* Single quote */
#define DC	6		/* Double quote */
#define SY	7		/* Symbol character */
#define SL	8		/* Solo character */
#define BK	9		/* Brackets & friends */
#define BS	10		/* Blank space */
#define EM	11		/* End of medium */

static char chtyp[] = {
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, EM, BS, BS, BS, BS, BS,

/*  sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    BS, SL, DC, SY, LC, SL, SY, QT, BK, BK, SY, SY, BK, SY, SY, SY,

/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
    N,  N,  N,  N,  N,  N,  N,  N,  N,  N, SY, SL, SY, SY, SY, SY,

/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
   SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, BK, SY, BK, SY, UL,

/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
   SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  del */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, BK, BK, BK, SY,  BS };

Ptr term();

Ptr fentry(atom,arity)
Ptr atom; int arity;
{
	return((Ptr) find(arity+1, dname(atdict(atom))));
}

Ptr apply(at,n,args)
Ptr at; Ptr args; int n;
{
    Ptr s; register Ptr r;
	s =
	r = newmem(n+1);
	*s++ = ConsFunc(fentry(at, n), n);
	while (n-- > 0)
		*s++ = *args++;
	return r;
}

Ptr makelist(n,elms)
int n; Ptr elms;
{
    Ptr  f; register int i; register Ptr p;

	p = (Ptr) &f;
	for(i = 1; i < n; i++) {
		*p = (Int) newmem(3);
		*(Int*)*p = ConsFunc(Ddot, 2);
		*((Int*)*p + 1) = *elms++;
		p = ((Int*)*p + 2);
	}
	*p = *elms;
	return f;
}

static int isop(adict,optype,p,lp,rp)
int adict; int optype, *p, *lp, *rp;
{
    short oe;

    switch (optype) {
	case PREFIX:
	    oe = dprprec(adict);
	    break;
	case INFIX:
	    oe = dinprec(adict);
	    break;
	case POSTFIX:
	    oe = dpsprec(adict);
	    break;
	default:
	    return FALSE;
    }
    if (oe < 0) return FALSE;
    *p = oe&mskprty;
    *lp = !(oe&dlprflg) ? *p : *p-1;
    *rp = !(oe&drprflg) ? *p : *p-1;
    return TRUE;
}

op(pr,ops,at)
int pr;
char *ops;
Int at;
/*  processes op declarations */
{
    int c, i, typ;
    static char
    *OpTypes[] = { "xfx", "xfy", "yfx",    "xf",    "yf",   "fx",   "fy" };
    static int
    optyp[] =    { INFIX, INFIX, INFIX, POSTFIX, POSTFIX, PREFIX, PREFIX };


    if (pr > 1200 || pr <= 0) return FALSE;
    c = -1;
    for (i = 0;i < 7; i++)
	if (!strcmp(ops,OpTypes[i])) {
	    c = i;
	    break;
	}
    if (c < 0) return FALSE;
    typ = optyp[c];
    c = 1<<(c+1);
    if (c&0x16) pr |= dlprflg;
    if (c&0x4a) pr |= drprflg;
	switch (typ) {
	    case PREFIX:
		d_prprec(find(2, dname(at))) =
		d_prprec((int)at) = pr;
		break;
	    case INFIX:
		d_inprec(find(3, dname(at))) =
		d_inprec((int)at) = pr;
		break;
	    case POSTFIX:
		d_psprec(find(2, dname(at))) =
		d_psprec((int)at) = pr;
	}
    return TRUE;
}

static legalatom(s)
char *s;
{
    char c;

    c = chtyp[*s];
    if (c == LC) {
	while (c = chtyp[*s], *s++) if (c > N) return FALSE;
	return TRUE;
    }
    if (c == SL && !*(s+1) && strcmp(s,"%")!=0) return TRUE;
	if(!strcmp(s, "[]") || (!Wdisp && !strcmp(s, ","))) return TRUE;
    if (c != SY) return FALSE;
    if (!strcmp(s,"%")
	|| contains_comment(s)
	/*|| !strcmp(s,".")*/)
	return FALSE;
    while (c = chtyp[*s], *s++) if (c != SY) return FALSE;
    return TRUE;
}

static
contains_comment(s)
char *s;
{
	for ( ; *s; s++)
		if (*s == '/' && *(s+1) == '*')
			return TRUE;
	return FALSE;
}

static patom(at)
Ptr at;
{
    char ch, *s;
    s = dname(atdict(at));
    if (!Wquote || legalatom(s)) {
	PutString(s);
#ifdef PARAM
	if(IsParam(at)) {
		Put('[');
		PutInt(atdict(at));
		Put(']');
		return;
	}
#endif
	if(dhide(atdict(at)))
		Put('_');
	return;
    }
    Put('\'');
    while ((ch = *s++)) {
	if (ch == '\'') Put(ch);
	Put(ch);
    }
    Put('\'');
#ifdef PARAM
	if(IsParam(at)) {
		Put('[');
		PutInt(atdict(at));
		Put(']');
		return;
	}
#endif
	if(dhide(atdict(at)))
		Put('_');
	return;
}

static
pvar(t, l)
	Ptr t;
	levtype l;
{
	int i;

	PutString(dname(vtdict(t)));
	if(Wvarnum) {
		if(vtdict(t) == D_)
			i = ((Ptr)l-(Ptr)gbindings(goalstk))/2+tvnum(t);
		else
			i = ((Ptr)l-(Ptr)gbindings(goalstk))/7;
		if(i) {
			Put('_');
			PutInt(i);
		}
	}
}

IsString(t, l)
	Ptr t;
	levtype l;
{
	int n;
	Ptr t1;
	levtype l1;

	while(IsComp(t) && ctdict(t) == Ddot) {
		findbind((Ptr)targ(1, t), l, &t1, &l1);
		if(!IsInt(t1))
			return(0);
		n = XtrInt(t1);
		if((n < 32 || n > 127) && n != '\t')
			return(0);
		findbind((Ptr)targ(2, t), l, &t, &l);
	}
	if(!IsAtom(t) || atdict(t) != Dnil)
		return(0);
	return(1);
}

static
wstring(t, l)
	Ptr t;
	levtype l;
{
	Ptr t1;
	levtype l1;
	char c;

	Put('"');
	while(IsComp(t) && ctdict(t) == Ddot) {
		findbind((Ptr)targ(1, t), l, &t1, &l1);
		if ((c = XtrInt(t1)) == '"')
			Put(c);
		Put(c);
		findbind((Ptr)targ(2, t), l, &t, &l);
	}
	Put('"');
}

/*  pwrite - write a prolog term  */

pwrite(lev, t,g,p)
int lev;	/* level of term nesting */
Ptr t; levtype g; int p;
/*  write term t in context of priority p
    with global frame g
*/
{
    int i, m, mr, ml, a;

	if(intflag)
		return;
	if(lev > MAXWLEV) {
		Put('?');
		return;
	}
	findbind(t, g, &t, &g);
    if (IsInt(t)) {
	    PutInt(XtrInt(t));
	    return;
    }
    if (IsAtom(t)) {
	a = atdict(t);
	if (isop(a, PREFIX, &m, &ml, &mr) && m > p
			|| Wpar && !Isatoz(a) && a != Dnil
				&& chtyp[*dname(a)] != SL
				&& legalatom(dname(a)))
		/* if (dname(a)[0] == ',') /* why not workee? *//*
			Put('\'');
		else
			Put('(');
		*/
		if (dname(a)[0] == ',') {
			Put('\'');
			patom(t);
			Put('\'');
		} else {
			Put('(');
			patom(t);
			Put(')');
		}
	else
		patom(t);
	return;
    }
	if(IsVar(t)) {
		pvar(t, g);
	return;
    }
	if(ctdict(t) == Ddot)
		if(Wstring && IsString(t, g)) {
			wstring(t, g);
			return;
    } else if (!Wdot) {
	Put('[');
	do {
	    if (intflag)
		return;
	    pwrite(lev+1, (Ptr)targ(1, t),g,999);
	    t = (Ptr) targ(2, t);
		 findbind(t, g, &t, &g);
	    if (!IsComp(t) || tdict(t) != Ddot)
			break;
	    Put(',');
	    Put(' ');
	} while(1);
	if (!IsAtom(t) || atdict(t) != Dnil) {
	    Put('|');
	    pwrite(lev+1, t,g,999);
	}
	Put(']');
	return;
    }
    if (tdict(t) == Dbrace){
	Put('{');
	pwrite(lev+1, (Ptr)targ(1, t),g,1200);
	Put('}');
	return;
    }
    a = ctdict(t);
    i = tnargs(t);
    if (i == 1) {
	if (isop(a,PREFIX,&m,&ml,&mr)) {
	    if (m > p) Put('(');
		if (dname(a)[0] == ',')
		Put('\'');
	    patom((Ptr)*t);
		if (dname(a)[0] == ',')
		Put('\'');
	    Put(' ');
	    pwrite(lev+1, (Ptr)targ(1, t),g,mr);
	    if (m > p) Put(')');
	    return;
	}
	if (isop(a,POSTFIX,&m,&ml,&mr)) {
	    if (m > p) Put('(');
	    pwrite(lev+1, (Ptr)targ(1, t),g,ml);
	    Put(' ');
		if (dname(a)[0] == ',')
		Put('\'');
	    patom((Ptr)*t);
		if (dname(a)[0] == ',')
		Put('\'');
	    if (m > p) Put(')');
	    return;
	}  
    }
    if (i == 2 && isop(a,INFIX,&m,&ml,&mr)) {
	if (m > p) Put('(');
	pwrite(lev+1, (Ptr)targ(1, t),g,ml);
	if (a != Dcomma && a != Ddot) Put(' ');
	patom((Ptr)*t);
	if(a != Ddot)
		Put(' ');
	pwrite(lev+1, (Ptr)targ(2, t),g,mr);
	if (m > p) Put(')');
	return;
    }
	if (dname(a)[0] == ',')
	Put('\'');
    patom((Ptr)*t);
	if (dname(a)[0] == ',')
	Put('\'');
    Put('(');
    for(i=0; i < tnargs(t); i++) {
	pwrite(lev+1, (Ptr)targ(i+1, t),g,999);
	if (i < tnargs(t)-1) {
		Put(',');
		Put(' ');
	}
    }
    Put(')');
    return;
}

/*  lookup - intern a string to get a prolog atom */

Ptr lookup(id)
char *id;
{
    return (Ptr) ConsFunc(find(1, id), 0);
}

/*-------------------------------------------------------------------

   pread - read a prolog term
   (this function has lots of auxiliary functions, which are listed first)

*/

static Int e;

/* Token types */

#define NAME		1
#define PRIMITIVE	2
#define VAR		3
#define STRING		4
#define PUNCTUATION	5
#define FULLSTOP	6

static int retoken, tokentype, rechar, chtype, errflg;
static char *line, *lp, *lpmax, ch;
static char nam[IDLEN];
static Ptr slsp;

union {
    Ptr		AsPtr;
    char	AsChar;
    char *	AsString;
} tokeninfo;

/*  next character from input buffer (in read)
/*  allows for single char lookahead */

static char nextchar()
{
    if (rechar) {
	rechar = FALSE;
	return chtype;
    }
    chtype = chtyp[ch = *++lp];
    if (lp >= lpmax) lp = lpmax-2;
    return chtype;
}


/*  look up variable name in variable table (in read) */

Ptr lookupvar(id)
char *id;
{
    VarP r, q;
	int d;

	d = find(0, id);
    if (d == D_)
	return((Ptr) ConsVar(d, nvars++));
    for (q = varchain, r = NULL; q; r = q, q = q->NextVar)
	if (atdict(q->VarValue) == d) return q->VarValue;
    q = (VarP) newmem(Words(sizeof(VarEntry)));
    if (r)
	r->NextVar = q;
    else
	varchain = q;
    q->NextVar = NULL;
    return(q->VarValue = (Ptr) ConsVar(d, nvars++));
}


/*  report a syntax error and wind things up (in read) */

static SyntaxError(mess)
    char *mess;
{
    char *i;

    rechar = FALSE; retoken = FALSE;
    if (errflg) {
	lp = lpmax-2;
	return;
    }
    fprintf(stderr,"\n***Syntax error:%s***\n",mess);
    for (i = line; i <= lpmax; i++) {
	if (i == lp)
	    fprintf(stderr,"\n**here?**\n");
	putc(*i,stderr);
    }
    lp = lpmax-2; errflg = TRUE;
}


/*  token - tokeniser (in read) */

static int token()
{
    int v, l;

    if (retoken) {
	retoken = FALSE;
	return tokentype;
    }
    start:
    switch (nextchar()) {
	case BS: goto start;

	case UC:		/* uppercase letter */
	    v = 1; goto id;

	case UL:		/* underline */
	    v = 1; goto id;
	case LC:		/* lowercase letter */
	    v = 0;
	id: 		/* common to both variables and atoms */
	    rechar = TRUE; l = 0;
	    while (nextchar() <= N) {
		nam[l++]=ch;
	    }
	    nam[l] = 0;
	    rechar = TRUE;
	    if (v) {
		tokentype = VAR;
		tokeninfo.AsPtr = lookupvar(nam);
		return VAR;
	    }
	    tokentype = NAME;
	    tokeninfo.AsPtr = lookup(nam);
	    return NAME;
	case N: 	/* digit */
	    if (*(lp+1) == '\'') {
		lp++; v = ch-'0'; l = 0;
		while (nextchar() == N)
		    l = l*v+ch-'0';
		tokeninfo.AsPtr = (Ptr) ConsInt(l);
		rechar = TRUE;
		return tokentype = PRIMITIVE;
	    }
	if (!NumberString(&lp,(Ptr)&tokeninfo.AsPtr,TRUE))
	    SyntaxError("not a Number String");
	    return tokentype = PRIMITIVE;
	case QT:		/* single quote */
	    v = QT; goto quoted;

	case DC:		/* double quote */
	    v = DC;
	quoted:
	    l = 0;
	    while (nextchar() != v || nextchar() == v) {
		nam[l] = ch;
		if (l >= IDLEN) SyntaxError("too long id");
		else l++;
	    }
	    nam[l] = 0;
	    rechar = TRUE;
	    if (v == QT) {
		tokentype = NAME; tokeninfo.AsPtr = lookup(nam);
		return NAME;
	    }
	    tokentype = STRING; tokeninfo.AsString = nam;
	    return STRING;

	case SY:		/* symbol char */
	    if (ch =='/' && *(lp+1) == '*') {	/* I think this is impossible */
						/* since comments are removed */
						/* earlier */
		do
		    chtype = nextchar();
		while (ch != '*' || *(lp+1) != '/');
		lp++;
		*lpmax++ = ' ';		/* treat comment as a space */
		goto start;
	    }
	    l = 1; nam[0] = ch;
	    if (ch == '.') {		/* full stop is a special case */
		if (nextchar() == BS) {
		    tokentype = FULLSTOP; lp--;
		    return FULLSTOP;
		}
		rechar = TRUE;
	    }
	    while (nextchar() == SY)
		nam[l++] = ch;
	    nam[l] = 0;
	    rechar = TRUE;
	    tokentype = NAME; tokeninfo.AsPtr = lookup(nam);
	    return NAME;

	case SL:		/* solo char */
	    nam[0] = ch; nam[1] = 0;
	    tokentype = NAME; tokeninfo.AsPtr = lookup(nam);
	    return NAME;

	case BK:                /*  punctuation char */
	    if (ch == '[' && *(lp+1) == ']') {
		tokentype = NAME; lp++;
		tokeninfo.AsPtr = (Ptr) ConsFunc(Dnil, 0);
		return NAME;
	    }
	    tokentype = PUNCTUATION; tokeninfo.AsChar = ch;
	    return PUNCTUATION;
    }
}		/* end of tokeniser */

/*  readargs - parse arguments of a term (in read) */

static Ptr readargs(atom)
Ptr atom;
{
    Ptr savelsp, e; int a;

    savelsp = lsp; a = 0;
    chtype = nextchar();		/* pass over ( */
    do {
	*lsp++ = (Int) term(999); a++;
    } while (token() == PUNCTUATION && tokeninfo.AsChar == ',');
    if (tokentype != PUNCTUATION || tokeninfo.AsChar != ')')
	SyntaxError("punctuation");
    e = apply(atom,a,savelsp);
    lsp = savelsp;
    return e;
}


/*  stringtolist - string to list of chars (in read) */

static Ptr stringtolist()
{
    Ptr savelsp; int n; register char c, *l;

    savelsp = lsp;
    for (l = nam; c = *l; l++)
	*lsp++ = ConsInt(c);
    n = l-nam;
    *lsp = ConsFunc(Dnil, 0); lsp = savelsp;
    return makelist(n+1,savelsp);
}

/*  readlist - parse a prolog list (in read) */

Ptr readlist()
{
    Ptr savelsp, e; int n;

    savelsp = lsp; n = 1;
    while (TRUE) {
	*lsp++ = (Int) term(999); n++;
	if (token() == PUNCTUATION && tokeninfo.AsChar == ',') {
	    if (token() == NAME && !strcmp(nam,"..")) {
		e = term(999);
		break;
	    }
	    else retoken = TRUE;
	}
	else {
	    if (tokentype == PUNCTUATION && tokeninfo.AsChar == '|')
		e = term(999);
	    else {
		e = (Ptr) ConsFunc(Dnil, 0); retoken = TRUE;
		}
	    break;
	}
    }
    *lsp = (Int) e;
	lsp = savelsp;
    return makelist(n,savelsp);
}


/* term - parse token stream to get term (in read) */

static Ptr term(n)
int n;
{
    int m, m1, ml, mr; Ptr e[2], s;

    if (errflg) return NULL;
    m = 0;
    switch (token()) {
	case NAME:			/* a name */
	    if (*lp == '(') {
		e[0] = readargs(tokeninfo.AsPtr);
		break;
	    }
	    if (isop((int)atdict(tokeninfo.AsPtr),PREFIX,&m,&ml,&mr)) {
		e[0] = s = tokeninfo.AsPtr;
		if (token() == PUNCTUATION && 
                    (tokeninfo.AsChar != '(' &&
		    tokeninfo.AsChar != '[')
		    || tokentype == FULLSTOP) {
		    if (m > n) SyntaxError("punctuation 2");
		    retoken = TRUE;
		    break;
		}
		retoken = TRUE;
		e[0] = term(mr);
		e[0] = apply(s,1,(Ptr)e);
		break;
	    }
	    e[0] = tokeninfo.AsPtr;
	    break;
	
	case PRIMITIVE:		/* a primitive type (already encoded) */
	    e[0] = tokeninfo.AsPtr;
	    break;
	    
	case VAR:			/* a variable */
	    e[0] = tokeninfo.AsPtr;
	    break;
	
	case STRING:			/* a string */
	e[0] = stringtolist();
	break;
	
	case PUNCTUATION:                       /*  punctuation char */
	    if (tokeninfo.AsChar == '(') {
		e[0] = term(1200);
		if (token() != PUNCTUATION || tokeninfo.AsChar != ')')
			SyntaxError("missing )");
		break;
	    }
	    if (tokeninfo.AsChar == '[') {
		e[0] = readlist();
		if (token() != PUNCTUATION ||
		    tokeninfo.AsChar != ']') SyntaxError("missing ]");
		break;
	    }
	    if (tokeninfo.AsChar == '{') {
		e[0] = term(1200);
		if (token() != PUNCTUATION || tokeninfo.AsChar != '}')
		    SyntaxError("missing }");
		e[0] = apply((Ptr)ConsFunc(Dbrace, 1),1,(Ptr)e);
		break;
	    }
	
	case FULLSTOP:		/*  other puctuation chars or fullstop */
	    SyntaxError("fullstop"); return NULL;

    }
    on:
    if (errflg) return NULL;
    if (token() == NAME) {
	if (isop((int)atdict(tokeninfo.AsPtr),INFIX,&m1,&ml,&mr)) {
	    if (m1 <= n && ml >= m) {
		s = tokeninfo.AsPtr;
		e[1] = term(mr);
		e[0] = apply(s,2,(Ptr)e);
		m = m1;
		goto on;
	    }
	}
	if (isop((int)atdict(tokeninfo.AsPtr),POSTFIX,&m1,&ml,&mr)) {
	    if (m1 <= n && ml >= m) {
		s = tokeninfo.AsPtr;
		e[0] = apply(s,1,(Ptr)e);
		m = m1;
		goto on;
	    }
	}
	retoken = TRUE;
	return e[0];
    }
    
    if (tokentype == FULLSTOP) {
	retoken = TRUE;
	return e[0];
    }
    if (tokentype != PUNCTUATION  ||
        tokeninfo.AsChar == '(' || tokeninfo.AsChar == '[') {
	SyntaxError("extra parenthesis");
	return NULL;
    }
    if (tokeninfo.AsChar == ',' &&  n >= 1000 && m <= 999) {
	e[1] = term(1000);
	e[0] = apply((Ptr)ConsFunc(Dcomma, 2),2,(Ptr)e);
	m = 1000;
	if (m < n) goto on;
	return e[0];
    }
    retoken = TRUE;
    return e[0];

}		/* end of term */

onlybs(s)	/* true if string s contains only blanks then a newline */
	char *s;
{
	while(*s == ' ')
		s++;
	return(*s == '\n');
}

/* the read predicate */

Ptr pread()
{
    varchain = NULL; errflg = FALSE; nvars = 0;
    lpmax = line = (char*)lsp;
    slsp = lsp;
    *lpmax++ = ' ';

	if (setjmp(intrbuf)) {			/* if interrupt in read */
		doljump = 0;
		lpmax = line + 1;		/* make it look like EOF */
		ch = CtrlZ;
		goto end;
	}
	doljump = 1;
    loop:
    ch = Get();
    l1:
    chtype = chtyp[ch];
	if(chtype == EM)
		goto end;
    if (chtype == BS) {
	do ch = Get(); while(chtyp[ch] == BS);
	*lpmax++ = ' ';
	goto l1;
    }
    if (ch == '%') {
	ch = Get();
	if (ch == '(') {
	    ch = '{'; goto l1;
	}
	if (ch == ')') {
	    ch = '}'; goto l1;
	}
	while (ch != '\n' && chtyp[ch] != EM) ch = Get();
	*lpmax++ = ' ';
    if (*(lpmax-2) == '.' && chtyp[*(lpmax-3)] != SY) goto end;
	goto loop;
    }
    *lpmax++ = ch;
    if (chtype == QT && chtyp[*(lpmax-2)] == N && chtyp[*(lpmax-3)] != N)
	chtype = N;
    if (ch == '*' && lpmax-line > 1 && *(lpmax-2) == '/') {
	lpmax -= 2;
	do {
	    while (ch != '*' && chtyp[ch] != EM)
		ch = Get();
	    ch = Get();
	} while (ch != '/' && chtyp[ch] != EM);
	if(chtyp[ch] == EM) {
		errflg = 1;
		goto end;
	}
	*lpmax++ = ' ';
    if (*(lpmax-2) == '.' && chtyp[*(lpmax-3)] != SY) goto end;
	goto  loop;
    }
    if ((((char*)lsz)-lpmax) < IDLEN) {
	fprintf(stderr,"\n** Text too long **\n");
	errflg = 1;
	goto end;
	/*Event(ABORT);*/
    }
    if (chtype == QT || chtype == DC) {
	do {
	    ch = Get(); *lpmax++ = ch;
	} while(chtyp[ch] != chtype && chtyp[ch] != EM);
	if(chtyp[ch] == EM) {
		errflg = 1;
		goto end;
	}
	goto loop;
    }
    if (ch == '.' && chtyp[*(lpmax-2)] != SY) {
	ch = Get();
	if (chtyp[ch] == BS) goto end; else goto l1;
    }
    goto loop;
    
    end:
	doljump = 0;				/* end of read interrupt trap */
    *lpmax = '\n'; *(lpmax+1) = 0;
    lp = line-1;
    rechar = retoken = FALSE;
	if(chtyp[ch] == EM)
		if(onlybs(line)) {
			strcpy(line, " ?-(end).\n");
			lpmax = line+9;
			lsp += Words(10);
		} else {
			fprintf(stderr, "*** unexpected end of file ***\n");
			errflg = 1;
		}
	if(errflg){
		SyntaxError("EOF");
		e = NULL;
	} else {
		lsp += Words(lpmax-line+1);
    		e = (Int) term(1200);
		if (token() != FULLSTOP) SyntaxError("no fullstop");
    		if (errflg) {
			dispterm((Ptr)e);
			e = NULL;
    		}
	}
    lsp = slsp;
	if(varchain)
		displist(Words(sizeof(VarEntry)), (Ptr)varchain);
    return (Ptr)e;
}

int NumberString(s,p,free)
/* Scans the string *s to see if it is a number. If yes, *p takes
   the constructed number, and TRUE is returned, otherwise FALSE.
   If free is FALSE, the number must reach the end of the string
   to be accepted. In all cases, *s is updated to point to the
   last character used for the number (oddity courtesy of nextchar())
*/

char **s; Ptr p; int free;
{
    int i; char *t, *u, c;
    
    u = t = *s;
    if (*t == '-' || *t == '+') t++;
    if (!digits(&t)) goto no;
    if (!*t && free == FALSE) goto no;
    *s = t-1;
    c = *t;
    *t = ' ';
    sscanf(u,"%d",&i);
    *t = c;
	*p = ConsInt(i);
    return TRUE;
    no:
    *s = t-1;
    return FALSE;
}

static digits(s)
char **s;
{
    char *t;
    
    t = *s;
    if (chtyp[*t] != N) return FALSE;
    while (chtyp[*++t] == N);
    *s = t;
    return TRUE;
}

fget(f)
	FILE *f;
{
	int c;

	c = getc(f);
	if (c == EOF) {
		c = CtrlZ;
		if(f == stdin)		/* so we can still keep reading */
			rewind(stdin);	/* after someone type EOF character */
	}
	return(c);
}

#define	RBUFLEN 4096
static char rbuf[RBUFLEN];

Ptr
getterm(n, a)
	int (*n)(), **a;
{
	Ptr t;

	nextc = n;
	arg = a;
	lsp = (Ptr) rbuf;
	lsz =(Ptr) (rbuf + RBUFLEN);
	if(!(t = pread())) {
		if (intflag)
			return(stot("?-(end). "));
	}
	return(t);
}

printt(f, t, l)
	FILE *f;
	Ptr t;
	levtype l;
{
	outfile = f;
	if(Wdisp)
		display(t, l);
	else
		pwrite(0, t, l, 1200);
}

/*
 * DKH, courtesy jas & rao
 */
#define sPutString(x,s)	if(!intflag) { while (*x) *s++ = *x++; }
#define sPutInt(i,s)    if(!intflag) { sprintf(s, "%ld", (long)(i)); while (*s) s++; }

char *
sdisplay(t, l, s)
	Ptr t;
	levtype l;
	char *s;
{
	int arg, m, m1;
	char *spvar(), *spatom(), *swstring();

	if(intflag)
		return;
	findbind(t, l, &t, &l);
	if(IsVar(t))
		s = spvar(t, l, s);
	else if(IsInt(t)) {
		sPutInt(XtrInt(t),s);
	} else if(IsAtom(t))
		if (isop(atdict(t), PREFIX, &m, &m1, &m1) && m > 1000) {
			*s++ = '(';
			s = spatom(t, s);
			*s++ = ')';
		} else
			s = spatom(t, s);
	else if(ctdict(t) == Ddot && Wstring && IsString(t, l))
		s = swstring(t, l, s);
	else {
		s = spatom(*t, s);
		*s++ = ('(');
		for(arg = 1; arg <= tnargs(t); arg++) {
			s = sdisplay(targ(arg, t), l, s);
			if(arg < tnargs(t)) {
				*s++ = (',');
			}
		}
		*s++ = (')');
	}
	return(s);
}

char *
spvar(t, l, s)
	Ptr t;
	levtype l;
	char *s;
{
	int i;
	char *vname;

	vname = dname(vtdict(t));
	sPutString(vname,s);
	if(Wvarnum) {
		if(vtdict(t) == D_)
			i = ((Ptr)l-(Ptr)gbindings(goalstk))/2+tvnum(t);
		else
			i = ((Ptr)l-(Ptr)gbindings(goalstk))/7;
		if(i) {
			*s++ = ('_');
			sPutInt(i,s);
		}
	}
	return(s);
}

char *spatom(at,ss)
Ptr at;
char *ss;
{
    char ch, *s;
    s = dname(atdict(at));
    if (!Wquote || legalatom(s)) {
	sPutString(s,ss);
#ifdef PARAM
	if(IsParam(at)) {
		*ss++ =('[');
		sPutInt(atdict(at),ss);
		*ss++ =(']');
		return(ss);
	}
#endif
	if(dhide(atdict(at)))
		*ss++ =('_');
	return(ss);
    }
    *ss++ =('\'');
    while ((ch = *s++)) {
	if (ch == '\'') *ss++ =(ch);
	*ss++ =(ch);
    }
    *ss++ =('\'');
#ifdef PARAM
	if(IsParam(at)) {
		*ss++ =('[');
		sPutInt(atdict(at),ss);
		*ss++ =(']');
		return(ss);
	}
#endif
	if(dhide(atdict(at)))
		*ss++ =('_');
	return(ss);
}

char *
swstring(t, l, s)
	Ptr t;
	levtype l;
	char *s;
{
	Ptr t1;
	levtype l1;
	char c;

	*s++ = ('"');
	while(IsComp(t) && ctdict(t) == Ddot) {
		findbind(targ(1, t), l, &t1, &l1);
		if ((c = XtrInt(t1)) == '"')
			*s++ = (c);
		*s++ = (c);
		findbind(targ(2, t), l, &t, &l);
	}
	*s++ = ('"');
	return(s);
}
