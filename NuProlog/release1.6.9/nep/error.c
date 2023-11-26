/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

char *errorMessages[4] = {
	/*   0 */	"Undefined/unspecified error\n",
	/*   1 */	"Undefined predicate: ",
	/*   2 */	"Abort\n",
};

#ifdef DEBUG
void
dumpPastPCs()
{
	register int i;

	for(i = NextPCSlot; i < NPASTPCS; i++) {
		printf("%8x %8x ", PastPCs[i], PastBs[i]);
		fflush(stdout);
		displayInstruction(stdout, 0, PastPCs[i]);
	}
	for(i = 0; i < NextPCSlot; i++) {
		printf("%8x %8x ", PastPCs[i], PastBs[i]);
		fflush(stdout);
		displayInstruction(stdout, 0, PastPCs[i]);
	}
/*
	for(i = NextPCSlot - 1; i >= 0; i--) {
		fprintf(stderr, "%8x ", PastPCs[i]);
		if((NextPCSlot - i) % 8 == 0)
			fprintf(stderr, "\n");
	}
	for(i = NPASTPCS - 1; i >= NextPCSlot; i--) {
		fprintf(stderr, "%8x ", PastPCs[i]);
		if((NextPCSlot + NPASTPCS - i) % 8 == 0)
			fprintf(stderr, "\n");
	}
*/
}
#endif /* DEBUG */

void
panic(mesg)
char *mesg;
{
	(void) fflush(stdout);
	fprintf(stderr, "\nPanic: %s\n\n", mesg);
#ifdef DEBUG
	dumpPastPCs();
#endif /* DEBUG */
	if(coreflg)
		abort();
	exit(1);
}

void
warning(mesg)
char *mesg;
{
	(void) fflush(stdout);
	fprintf(stderr, "\nWarning: %s\n", mesg);
}

void
warning2(format, a1)
char *format;
char *a1;
{
	(void) fflush(stdout);
	fprintf(stderr, "\nWarning: ");
	fprintf(stderr, format, a1);
	fprintf(stderr, "\n");
}

void
arithError(term)
register Object term;
{
	register char *mesg;

	switch(ArithError) {
	when areOK:
		mesg = "No error";
	when areNON:
		mesg = "Non-arithmetic expression";
	when areDIVZERO:
		mesg = "Divide by zero";
	when areINT:
		mesg = "Integer expected";
	when areNONGROUND:
		mesg = "Expression must be ground when used as the condition of ->";
	when areMISC:
		mesg = "Unspecified  error";
	break;
	default:
		mesg = "Unknown error";
	}
	(void) fflush(stdout);
	fprintf(stderr, "\nWarning: %s in evaluating ", mesg);
	writeTerm(stderr, term);
	fprintf(stderr, "\n");
}

void arithErrorN(f, n, t1, t2)
Atom *f;
int n;
Object t1, t2;
{
	/*
	 * Take care with these -- putting tags on things on the C function
	 * stack often isn't possible because of the layout of memory by the
	 * operating system.
	 */
	static Object term[3];

	if(n > 2)
		panic("Too many sub-terms in arithErrorN()");

	term[0] = StarToStrHeader(n, f);
	term[1] = t1;
	term[2] = t2;

	arithError(StarToStr(term));
}
