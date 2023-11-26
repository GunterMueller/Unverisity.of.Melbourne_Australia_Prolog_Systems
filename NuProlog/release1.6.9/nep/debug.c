/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <ctype.h>

static int termCounter;				/* count amount of term displayed */

static void displayTerm2(), displayIConstant(), displayUConstant();
static void displayStruct(), displayList();

void
displayT(Object s)
{
	displayTerm(stdout, s);
	fprintf(stdout, "\n");
}

void
writeT(Object s)
{
	writeTerm(stdout, s);
	fprintf(stdout, "\n");
}

void
displayTerm(fp, s)
FILE *fp;
Object s;
{
#ifdef DEBUG
	if(s == 0) {
		warning("0 given to displayTerm()");
		return;
	}
#endif
	termCounter = eSmallInt(Flags[flgPRINTDEPTH]) * 10;
	displayTerm2(fp, s);
}

static void
displayTerm2(fp, s)
FILE *fp;
register Object s;
{
	if(termCounter-- <= 0) {
		fprintf(fp, "**Too many terms printed**");
		return;
	}

	start:	/* Your friendly neighbourhood label */
	switch(eType(s)) {
	when tREF:
		s = (Object)eRef(s);
		if(IsUnbound(s)) {
			fprintf(fp, "_%x", s);
		} else {
			fprintf(fp, "%x->", s);
			s = *(Object *)s;
			goto start;
		}
	when tDEL:
		if(IsUnbound(s)) {
			fprintf(fp, "*%x", (Object) eRef(s));
		} else {
			fprintf(fp, "*%x->", (Object) eRef(s));
			s = *eRef(s);
			goto start;
		}
	when tICN:
		displayIConstant(fp, s);
	when tUCN:
		displayUConstant(fp, s);
	when tSTR:
		displayStruct(fp, s);
	when tLST:
		displayList(fp, s);
	when tCHR:
		fprintf(fp, "\"%s\"", (char *)eRef(s));
	when tBMV:
		fprintf(fp, ":%d:", eCValue(s));

	break;
	default:
		panic("Impossible Type in displayTerm2");
	}
}

static void
displayIConstant(fp, s)
FILE *fp;
Object s;
{
	switch(eCType(s)) {
	when ctATM:
		fprintf(fp, "%s", eCharStar(((Atom *)eRef(s))->a_pname));
	when ctINT:
		fprintf(fp, "%d", eSmallInt(s));
	when ctBLK:
		fprintf(fp, "<%x>", eRef(s));

	break;
	default:
		panic("Impossible constant type in displayIConstant");
	}
}

static void
displayUConstant(fp, s)
FILE *fp;
Object s;
{
	switch(eCType(s)) {
	when ctI32:
		fprintf(fp, "%d", eInt32(s));
	when ctFLT:
		fprintf(fp, "%20e", eFloat(s));

	break;
	default:
		panic("Impossible constant type in displayUConstant");
	}
}

static void
displayStruct(fp, s)
FILE *fp;
Object s;
{
	register int n;
	register Word *ss;

	ss = eRef(s);
	fprintf(fp, "%s@%x(", eCharStar(eFunctor(*ss)->a_pname), (Word) ss);
	n = eNArgs(*ss); ss++;	/* Vax c2 BUG */
	if(n == 0)
		n = *ss++;
	do {
		displayTerm2(fp, *ss);
		if(n > 1)
			fprintf(fp, ", ");
		ss++;
	} while(--n > 0);
	fprintf(fp, ")");
}

static void
displayList(fp, s)
FILE *fp;
Object s;
{
	register Word *ss;

	ss = eRef(s);
	fprintf(fp, ".@%x(", (Word) ss);
	displayTerm2(fp, *ss);
	ss++;
	fprintf(fp, ", ");
	displayTerm2(fp, *ss++);
	fprintf(fp, ")");
}

int
displayInstruction(fp, base, loc)
register FILE *fp;
register Program base, loc;
{
	register int op, ot, offset;

	op = eOpCode(*loc);
	ot = bytecodes[op].op_type;
	offset = 0;
	fprintf(fp, "%x\t%s\t", loc /*- base*/, bytecodes[op].op_name);
	if(ot & otNARGS)
		fprintf(fp, "%d", eArg1(*loc));
	if((ot & otNARGS) > 1)
		fprintf(fp, ",%d", eArg2(*loc));
	if((ot & otNARGS) > 2)
		fprintf(fp, ",%d", eArg3(*loc));
	if(ot & otNARGS && ot & (otLABEL | otIMMED))
		fprintf(fp, ",");
	if(ot & otIMMED) {
		loc++; offset = 1;
		if(ot & otPROC) {
			Functor *f;

			f = *(Functor **)loc;
			fprintf(fp,
				"&%s/%d",
				eCharStar(f->f_functor->a_pname),
				f->f_arity);
		} else if(ot & otSTRUC) {
			fprintf(fp, "$%s/%d",
				eCharStar((eFunctor(*loc)->a_pname)),
				eNArgs(*loc));
		} else if(op == cPUSHI || op == cAPUSHI)
			fprintf(fp, "$$%d", *loc);
		else if(IsInt(*loc))
			fprintf(fp, "$%d", eInt(*loc));
		else if(IsAtom(*loc))
			fprintf(fp, "$%s", eCharStar((((Atom *)eRef(*loc))->a_pname)));
		else if(IsNIL(*loc))
			fprintf(fp, "$[]");
		else if(IsFloat(*loc))
			fprintf(fp, "$%g", eFloat(*loc));
		else if(ot & otTABLE) {
			register int i;
			
			offset = 4;
			for(i = 0; i < 4; i++) {
				fprintf(fp, "&%x", (Word *) *loc++ /*- base*/);
				if(i < 3)
					fprintf(fp, ",");
			}
		}
		else {
			fprintf(fp, "$");
			displayTerm(fp, (Object) *loc);
		}
	}
	if(ot & otLABEL) {
		if(ot & (otNARGS | otIMMED))
			fprintf(fp, ",");
		loc++; offset = 1;
		fprintf(fp, "&%x", (Word *) *loc /*- base*/);
	}
	fprintf(fp, "\n");

	return(offset);
}

#ifdef DEBUG3
void
displayCall(fp, format, arg, n, X)
register FILE *fp;
char *format, *arg;
register int n;
Object *X;
{
	register int i;

	fprintf(fp, format, arg);
	for(i = 0; i < n; i++) {
		displayTerm(fp, X[i]);
		if(i < n - 1)
			fprintf(fp, ", ");
	}
	fprintf(fp, ")\n");
}
#endif /* DEBUG3 */

int
p_printNumber(X1, X2, X3, X4)
Object X1, X2, X3, *X4;
{
	register double x;
	register char *s, *format;
	register int p;

	DeRef(X1);

	DeRef(X2);
	if(IsSmallInt(X2))
		p = eSmallInt(X2);
	else {
		warning("Integer expected in p_printNumber()");
		return(0);
	}
	if(p < 0) {
		warning("Positive integer precision expected in p_printNumber()");
		return(0);
	} else if(p > 512) {
		warning("Field too big in p_printNumber()");
		return(0);
	}

	DeRef(X3);
	if(IsInt(X3))
		x = eInt(X3);
	else if(IsFloat(X3))
		x = eFloat(X3);
	else {
		warning("Number expected in p_printNumber()");
		return(0);
	}

	/*
	 * We don't worry about heap overflow because there is guaranteed
	 * to be enough to hold any reasonable representation of a number.
	 */
	s = (char *)(CMR->mr_h);
	switch(X1) {
	when MakeSmallInt('e'):
		format = "%.*e";
	when MakeSmallInt('E'):
		format = "%.*E";
	when MakeSmallInt('f'):
		format = "%.*f";
	when MakeSmallInt('g'):
		format = "%.*g";
	when MakeSmallInt('G'):
		format = "%.*G";

	break;
	default:
		warning("Undefined format character to p_printNumber()");
		return(0);
	}

	sprintf(s, format, p, x);
	CMR->mr_h += NWORDS(strlen(s) + 1);
	*X4 = StarToString(s);
	return(1);
}

#define	listHead(x)	((Object)(eRef(x)))
#define	listTail(x)	((Object)*((x)+1))

int
p_printf(X1, X2, X3)
register Object X1, X2, X3;
{
	register Object a, aa, *ap;
	register Stream *st;
	register char *c, *ac, *fc, *item;
	unsigned char *h, *H, alisttype;
	char *format, save;
	char prbuf[BUFSIZ];

	st = validateStream(X1, 'w');
	if(st == (Stream *) NULL) {
		warning("Invalid stream to PRINTF");
		return(0);
	}
	X2 = deRef(X2);
	if(IsNIL(X2))
		format = "";
	else if(IsString(X2))
	format = (char *) eRef(X2);
	else {
		warning("Invalid format string to PRINTF");
		return(0);
	}
	X3 = deRef(X3);
#if 0
displayTerm2(stderr,X3);putc('\n',stderr);
#endif
	switch(alisttype = eType(X3))
	{
	when tICN:
		if (IsNIL(X3)) {
			sprintf(prbuf, format);
			for (c = prbuf; *c != '\0'; c++)
				(*st->s_put)(*c, st);
			return(1);
		}
		else {
			warning("Invalid args list to PRINTF");
			return(0);
		}
	when tCHR:
		ac = (char *) eRef(X3);
	when tLST:
		H = (unsigned char *)(CMR->mr_h);
		a = X3;
	break;default:
		warning("Invalid args list to PRINTF");
		return(0);
	}

	/*
	 * Scan format string; print arg for each format item
	 */
	for (fc = format; *fc != '\0'; fc++)
	{
		if (*fc != '%')
			(*st->s_put)(*fc, st);
		else if (*(fc+1) == '%') {
			(*st->s_put)(*fc, st);
			fc++;
		} else { /* process next format item */
			/*
			 * Set up format string (i.e. %....d)
			 */
			item = fc;
			while (!islower(*fc) && *fc != 'l' && *fc != '\0')
				fc++;
			save = *(fc+1);
			*(fc+1) = '\0';

			/*
			 * Obtain and print next argument in arg list
			 */
			if (alisttype == tCHR)
PrintStringArgs:
				sprintf(prbuf, item, (Word)*ac++);
			else {
				a = deRef(a);
				if (eType(a) == tCHR) {
					ac = (char *) eRef(a);
					alisttype = tCHR;
					goto PrintStringArgs;
				}
				a = listHead(a);
				ap = eRef(a);
				aa = deRef(*ap);
				switch (eType(aa))
				{
				when tICN:
					switch(eCType(aa)) {
					when ctATM:
						sprintf(prbuf, item,
							eCharStar(((Atom *)eRef(aa))->a_pname));
					when ctINT:
						sprintf(prbuf, item, eSmallInt(aa));
					break;default:
						warning("Unprintable object to printf");
						return(0);
					}
				when tUCN:
					switch(eCType(aa)) {
					when ctI32:
						sprintf(prbuf, item, eInt32(aa));
					when ctFLT:
						sprintf(prbuf, item, eFloat(aa));
					break;default:
						warning("Unprintable object to printf");
						return(0);
					}
				when tCHR:
					sprintf(prbuf, item, ((char *)eRef(aa)));
				when tLST:
					/*
					 * Strings which are represented as
					 * lists of small numbers are converted
					 * to proper 'C' strings and placed
					 * temporarily on the Heap
					 */
					h = (unsigned char *)(CMR->mr_h);
					CMR->mr_h = (Word *)listToString(aa, h, heapMax, 1);
					sprintf(prbuf, item, h);
				break;default:
					warning("Unprintable object to printf");
					return(0);
				}
				a = listTail(ap);
			}
			for (c = prbuf; *c != '\0'; c++)
				(*st->s_put)(*c, st);
			*(fc+1) = save;
		}
	}
	/*
	 * Free up Heap space used for strings
	 */
	CMR->mr_h = (Word *)H;

	return(1);
}

/*
 * Determine the size in Words of an instruction.
 */
static int
wordsOfInstruction(op)
register int op;
{
	register int type, size;

	type = bytecodes[op].op_type;
	size = NWORDS(1 + (type & otNARGS));
	if(type & otTABLE)
		return(size + 4);
	else if(type & otLABEL && type & otIMMED)
		return(size + 2);
	else if(type & (otLABEL|otIMMED|otSTRUC|otPROC))
		return(size + 1);
	else
		return(size);
}

#define OpCode(x) (* (unsigned char *) (x))
#define Arg1(x) (* (1 + (unsigned char *) (x)))
#define Arg2(x) (* (2 + (unsigned char *) (x)))

int
p_spy(X1, X2)
Object X1, X2;
{
	register Program code, oldcode;
	Program first;
	int n;
	Functor *f;
	register int op;

	X2 = deRef(X2);
	n = eSmallInt(X2);
	X1 = deRef(X1);
	f = enterFunctor(n, (Atom *) eRef(X1));
	first
		= oldcode
		= f->f_code;
	if(f->f_codeType != fCOMPILED)
		panic("Setting spypoint on inappropriate type of code");
	f->f_codeType = fSPYPT;
	code = (Program) Talloc((4 + 1) * sizeof(Instruction));
	f->f_code = code;
	*code++ = MakeIns0(cSPYPT);
#define SPYENTRY 1
	*code++ = (Instruction) oldcode;
	*code++ = MakeIns1(cEXEC, 2);
	*code++ = (Instruction) enterFunctor(2, &SymSpy_Debug);

	for( ; ; ) {
		op = OpCode(oldcode);
		switch(op) {
		when cEXECSOT:
			if(oldcode != first &&
				Arg1(first) == Arg2(oldcode) &&
				!bcmp(
					(char *) (oldcode + 1), (char *) (first + 1),
					4 * sizeof(Instruction))) {
				/* Arg1 is already present, leave Arg2 alone. */
				OpCode(oldcode) = cEXEC;
				oldcode[1] = (Instruction) f;
			}
			oldcode += 5;
		when cMKDEL:
			OpCode(oldcode) = cSPYMDEL;
			oldcode += 2;
		when cSPYPT:	case cSPYMDEL:
			/* We probably wouldn't get this far anyway. */
			panic("Spying on a spy-point");
		when cLAST:
			return(1);
		break;
		default:
			oldcode += wordsOfInstruction(op);
		}
	}
}

int
p_nospy(X1, X2)
Object X1, X2;
{
	register Program code, first;
	register int op;
	int n;
	register Functor *f;

	X2 = deRef(X2);
	n = eSmallInt(X2);
	X1 = deRef(X1);
	f = enterFunctor(n, (Atom *) eRef(X1));
	if(f->f_codeType != fSPYPT)
		panic("Trying to remove non-existant spypoint");
	f->f_codeType = fCOMPILED;
	f->f_code
		= first
		= code
		= ((Program *) f->f_code)[SPYENTRY];

	for( ; ; ) {
		op = OpCode(code);
		switch(op) {
		when cEXEC:
			if(code != first &&
					OpCode(first) == cSOT &&
					OpCode(code) == cEXEC &&
					code[1] == (Instruction) f) {
				/*
				 * Copy the SOT table from the head of the prelude,
				 * but leave Arg1 and Arg2 alone
				 */
				bcopy((char *) (first + 1), (char *) (code + 1),
					4 * sizeof(Instruction));
				OpCode(code) = cEXECSOT;
				code += wordsOfInstruction((int)OpCode(first));
			} else
				code += 2;
		when cSPYMDEL:
			OpCode(code) = cMKDEL;
			code += 2;
		when cMKDEL:
			/* We probably wouldn't get this far anyway. */
			panic("Nospying on a non-spy-point");
		when cLAST:
			return(1);
		break;
		default:
			code += wordsOfInstruction(op);
		}
	}
}
