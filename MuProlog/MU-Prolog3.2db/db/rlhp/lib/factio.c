#include "stdio.h"
#include "rel.h"

factput(stream, fact)
FILE *stream;
FACT *fact;
{
	register int i;

	fprintf(stream, "%s", fact->fname);
	putc('\0', stream);
	for (i = 0; i < fact->nkeys; i++) {
		fprintf(stream, "%s", fact->field[i]);
		putc('\0', stream);
	}
	putc('\0', stream);
	
}

FACT *factget(stream)
FILE *stream;
{
	register int i;
	register char *b;

	static FACT fact;
	static char buf[BUFSZ];

	i = 0;
	fact.fname = b = &buf[0];
	while ((*b++ = getc(stream)) != '\0')
		if (feof(stream))
			return(NULL);
	do {
		fact.field[i] = b;
		while ((*b++ = getc(stream)) != '\0')
			if (feof(stream))
				return(NULL);
		fact.fsz[i] = b - fact.field[i];
	} while (*(fact.field[i++]) != '\0');
	fact.nkeys = i - 1; /* -1 unnecc if above loop written better */
	return(&fact);
}

/*
	<function> ::=	<identifier> (( <term> { ,, <term> } ))
	<term> ::=	<variable> | <constant> | <function>
	<variable> ::=	<upper-case letter> { <letter or digit or underscore> }
	<constant> ::=	<identifier> | <integer> | '' <character> { <character> } '' | "" { <character> } ""
	<identifier>::=	<lower-case letter> { <letter or digit or underscore> } | <special character> { <special characters> } | <solo character>
*/

factout(stream, fact)
FILE *stream;
FACT *fact;
{
	register i;

	fprintf(stream, "%s(", fact->fname);
	for (i = 0; i < fact->nkeys; i++) {
		if (i > 0)
			putc(',', stream);
		fprintf(stream, "%s", fact->field[i]);
	}
	fprintf(stream, ").\n");
}

struct {
	short toktype;
	char buf[BUFSZ];
} tok;

#define TEOF		0
#define STRING		1
#define CONSTANT	2
#define SOLO		3
#define SYMBOL		4
#define INTEGER		5
#define VARIABLE	6
#define IDENTIFIER	7

char *factb;	/* index into fact->buf[] */

FACT *factin(stream)
FILE *stream;
{
	static FACT fact;
	static char buf[BUFSZ];

	if (token(stream) == TEOF)
		return(NULL);
	factb = &buf[0];
	ffunc(stream, &fact);
	return(&fact);
}

ffunc(stream, fact)
FILE *stream;
FACT *fact;
{
	if (tok.toktype != IDENTIFIER)
		rerror("ident_expected");
	fact->fname = factb;
	factb += strlen(strcpy(factb, tok.buf)) + 1;
	fact->nkeys = 0;
	if (token(stream) == SOLO && tok.buf[0] == '(') {
		do {
			token(stream);
			fterm(stream, fact);
			fact->nkeys++;
		} while (tok.buf[0] == ',');
		if (tok.buf[0] != ')')
			rerror("no_closing_bracket");
		token(stream);
	}
}

fterm(stream, fact)
FILE *stream;
FACT *fact;
{
	fact->field[fact->nkeys] = factb;
	factb += fact->fsz[fact->nkeys] = strlen(strcpy(factb, tok.buf)) + 1;
	token(stream);
}


/* Character types for tokeniser */

#define UC	1		/* Upper case alphabetic and underscore */
#define LC	2		/* Lower case alphabetic */
#define N	3		/* Digit */
#define SQ	4		/* Single quote */
#define DQ	5		/* Double quote */
#define SY	6		/* Symbol character */
#define SL	8		/* Solo character */
#define BS	9		/* Blank space */

static char chtype[] = {
/* nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/* dle dc1 dc2 dc3 dc4 nak syn etb can  em sub esc  fs  gs  rs  us */
    BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS, BS,

/*  sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    BS, SL, DQ, SY, LC, SL, SY, SQ, SL, SL, SY, SY, SL, SY, SY, SY,

/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ? */
    N,  N,  N,  N,  N,  N,  N,  N,  N,  N, SY, SL, SY, SY, SY, SY,

/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O */
   SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,

/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _ */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, SL, SY, SL, SY, UC,

/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o */
   SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,

/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  del */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, SL, SL, SL, SY,  BS };

# define next(c,stream)	c = getc(stream); \
			if (feof(stream) || ferror(stream)) \
				return(TEOF)

token(stream)
FILE *stream;
{
	int i = 0;
	int c;

fprintf(stderr,"jat's token\n");
start:	
	next(c,stream);
	switch(chtype[c]) {
			/* white space */
		case BS:
			goto start;

			/* string */
		case DQ:
			dq:
			tok.toktype = STRING;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while ((char) c != '"');
			tok.buf[i++] = (char) c;
			next(c,stream);
			if ((char) c == '"')
				/* not finished */
				/* pair of double quotes = double quote */
				goto dq;
			break;

			/* constant */
		case SQ:
			sq:
			tok.toktype = CONSTANT;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while ((char) c != '\'');
			tok.buf[i++] = (char) c;
			next(c,stream);
			if ((char) c == '\'')
				/* not finished */
				/* pair of single quotes = single quote */
				goto sq;
			break;

			/* solo characters */
		case SL:
			tok.toktype = SOLO;
			tok.buf[i++] = c;
			next(c,stream);
			break;

			/* symbol */
		case SY:
			tok.toktype = SYMBOL;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while (chtype[c] == SY);
			break;

			/* integer */
		case N:
			tok.toktype = INTEGER;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while (chtype[c] == N);
			break;

			/* upper-case */
		case UC:
			tok.toktype = VARIABLE;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while (chtype[c] == UC);
			break;
			
			/* lower-case */
		case LC:
			tok.toktype = IDENTIFIER;
			do {
				tok.buf[i++] = (char) c;
				next(c,stream);
			} while ((chtype[c] == LC) || (chtype[c] == UC));
			break;

		default:
			fprintf(stderr, "illegal character \\%o\n", c);
			exit(1);
	}
	tok.buf[i++] = '\0';
	ungetc(c, stream);
	return(tok.toktype);
}


ground(fact, fld)
FACT *fact;
int fld;
{
	return(chtype[fact->field[fld][0]] != UC);
}
