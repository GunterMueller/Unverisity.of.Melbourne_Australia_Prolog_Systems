/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

Instruction WIsEq[] = {
	MakeIns0(cWISEQ)
};

/* Unique objects to use as place markers in ie_eq() */
static Atom LocalSymX, LocalSymY;

#define MAXGLOBALS 1024	/* Maximum number of new globals found in is_eq() */

/*
 * Attempt to wake an is_eq/3 builtin.
 *
 * Returns
 *	0 for failure,
 *	1 for success,
 *	2 for sleep again.
 *
 * In the last case, it arranges to wait on any further globals
 * that have become relevant, and re-uses the Delay record
 * much like the arithmetic predicates do.
 */
int
is_eq(X, CD)
register Object *X;
Delay *CD;
{
	register Object *low, *high;
	register TrailRecord *t, *TRB;
	register Object w;
	TrailRecord *TR;
	Object *HSave, *HBSave, WSave;
	Choice *BSave;
	Object *boundGlobals[MAXGLOBALS];	/* array of new globals found */
	Object **newGlobals;				/* top of boundGlobals */

#ifdef DEBUG4
	printf("ENTERING is_eq()\n");
	displayTerm(stdout, X[1]); printf("\n");
	displayTerm(stdout, X[2]); printf("\n");
	displayTerm(stdout, X[3]); printf("\n");
	printf("0x%6x 0x%6x\n",
		(Object *) eSmallInt(X[4]), (Object *) eSmallInt(X[5]));
	printf("ENTERED is_eq()\n");
#endif
	low = (Object *) eSmallInt(X[4]);
	high = (Object *) eSmallInt(X[5]);
	TRB = CMR->mr_tr;
	HSave = CMR->mr_h;
	HBSave = CMR->mr_hb;
	BSave = CMR->mr_b;
	WSave = CMR->mr_w;

	CMR->mr_b = (Choice *) CMR->mr_a;		/* Force unify to trail bindings */
	CMR->mr_hb = CMR->mr_h;					/* Force unify to trail bindings */

	w = unify(X[1], X[2]);
	CMR->mr_h = HSave;	/* Forget anything that unify() put on the Heap */
	CMR->mr_hb = HBSave;
	CMR->mr_b = BSave;
	CMR->mr_w = WSave;

	if(w) {
		/*
		 * Clean up the trail.
		 */
		newGlobals = boundGlobals;
		TR
			= t
			= CMR->mr_tr;
		while(t > TRB) {
			register Object *v;

			t--;
			v = t->tr_address;
			/*
			 * Unbind everything trailed, checking for newly involved
			 * global variables and collecting them to be marked later.
			 * Unbinding things and finding globals must not be
			 * allowed to interact.
			 *
			 * BUG!  (Inefficiency)
			 * Variables on which CD is already waiting may be marked
			 * for it again.
			 */
			if(v < low || v >= high) {
				register int push;

				w = *v;
           		*v = t->tr_value;
				if(!IsVar(w)) {
					*newGlobals++ = v;
					continue;
				}
				w = (Object) eRef(w);
				if((Object *)w < low || (Object *)w >= high)
					push = 1;
				else
					push = 0;
				w = deRef(*(Object *)w);
				if(!IsVar(w)) {
					*newGlobals++ = v;
					continue;
				}
				if(push)
					*newGlobals++ = v;
				w = (Object) eRef(w);
				if(low <= (Object *)w && (Object *)w < high) {
					trailOverflowCheck(CMR->mr_tr, 1);
					CMR->mr_tr->tr_address = (Object *) w;
					CMR->mr_tr->tr_value = *(Object *) w;
					CMR->mr_tr++;
					*(Object *)w = StarToAtom(&LocalSymX);
				} else {
					*newGlobals++ = (Object *) w;
					trailOverflowCheck(CMR->mr_tr, 1);
					CMR->mr_tr->tr_address = (Object *) w;
					CMR->mr_tr->tr_value = *(Object *) w;
					CMR->mr_tr++;
					*(Object *)w = StarToAtom(&LocalSymY);
				}
			} else
           		*v = t->tr_value;
		}
		/* Remove any variables marked with local Symbols. */
		t = CMR->mr_tr;
		while(t > TR) {
			t--;
			*t->tr_address = t->tr_value;
		}
		/* We've restored everything trailed now. */
		t = CMR->mr_tr;
		CMR->mr_tr = TRB;

		if(newGlobals > boundGlobals) {
			/* Mark any global variables that have become involved. */
			while(newGlobals > boundGlobals) {
#ifdef DEBUG4
fprintf(stdout, "MARKING 0x%x in is_eq()\n", *(newGlobals - 1));
#endif
				mark(*--newGlobals, CD);
			}
			/* Sleep again because some globals were bound */
			return(2);
#if 1
		}
#else
	JUNK!!! BUG!!!
		} else if(t > TRB) {
			/* We bound something that we were already waiting on. */
			return(2);
		}
#endif

		if(unify(StarToAtom(&SymTrue), X[3])) {
			return(1);
		} else
			return(0);
	} else {
		/*
		 * Clean up the trail and bind X[3] to fail.
		 */
		t = CMR->mr_tr;
		while(t > TRB) {
			t--;
           	*t->tr_address = t->tr_value;
		}
		CMR->mr_tr = TRB;
		if(unify(StarToAtom(&SymFail), X[3])) {
			return(1);
		} else
			return(0);
	}
}

#define A CMR->mr_a
#define B CMR->mr_b
#define H CMR->mr_h
#define HB CMR->mr_hb
#define TR CMR->mr_tr

void
copyVariablesToTopOfHeap(v)
register Object v;
{
	register Object *x;
	register int n;

	/* BUG!
	 * This is one of the places where the heap cannot be moved
	 */
start:				/* Tail recursion */
	DeRef(v);
	switch(eType(v)) {
	when tREF:
		x = eRef(v);
		if(x < HB || x >= H) {
			heapOverflowCheck(H, 1);
			*H = StarToUnboundRef(H);
			bind(x, StarToRef(H), TR);
			H++;
		}
		return;
	when tDEL:
		x = eRef(v);
		if(x < HB /* BUG!  Redundant!  || x >= H*/) {
			/*
			 * We can get away with simply rebinding the variable
			 * because we are binding it to a new one.
			 */
			heapOverflowCheck(H, wordsof(DelayHeader));
			((DelayHeader *) H)->h_self = (Object *) StarToUnboundDel(H);
			((DelayHeader *) H)->h_marks = ((DelayHeader *)x)->h_marks;
			bindH2(x, StarToRef(H), TR);
			H += wordsof(DelayHeader);
		}
		return;
	when tICN:
	case tUCN:
	case tCHR:
	case tBMV:
		return;
	when tLST:
		x = eRef(v);
		copyVariablesToTopOfHeap(x[0]);
		v = x[1];
		goto start;
	when tSTR:
		x = eRef(v);
		n = eNArgs(x[0]);
		for( ; n > 1; n--)
			copyVariablesToTopOfHeap(x[n]);
		v = x[1];
		goto start;
	}
}
