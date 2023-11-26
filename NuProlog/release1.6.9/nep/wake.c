/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

Delay *
makeDelay(nvars, resume)
int nvars;
Program resume;
{
	register Machine *cmr;
	register Delay *CD;
	register int i;

	cmr = CMR;

#define X cmr->mr_x
#define B cmr->mr_b
#define H cmr->mr_h
#define HB cmr->mr_hb

	heapOverflowCheck(cmr->mr_h, 256 + wordsof(Delay));

	CD = (Delay *) H;
#ifdef DEBUG
	i = nvars + 1;
#else /* DEBUG */
	i = nvars;
#endif /* DEBUG */
	cmr->mr_h
		= H
		= (Word *) (CD + 1) + i;
	CD->d_sleeping = 1;
	CD->d_n = i;
	CD->d_p = resume;
	CD++;
		/*
		 * The presence of CUT forces us to globalize all variables
		 * on the stack -- taking up trail space as a consequence.
		 */
	for(i = nvars - 1; i >= 0; i--) {
		register Object v;

#ifdef NO_LOCAL_VARS
		v = X[i]; DeRefPure(v);
		((Object *)CD)[i] = v;
#else /* NO_LOCAL_VARS */
		v = X[i];
		DeRefPure(v);
		if(IsRef(v) && (Word *)eRefStar(v) > (Word *) H) {	/* Grunge */
			register Object *ev;

			ev = ((Object *) CD + i);
			*ev = StarToUnboundRef(ev);
			trail(eRefStar(v), cmr->mr_tr);
			*eRefStar(v) = StarToRef(ev);
		} else
			*((Object *)CD + i) = v;
#endif /* NO_LOCAL_VARS */
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "X[%d] = ", i);
	displayTerm(stdout, X[i]); fprintf(stdout, "\n");
}
#endif /* DEBUG */
	}

#ifdef DEBUG
if(traceflg) {
	printf("Made delay <%x>\n", CD);
}
#endif /* DEBUG */

	return(CD);
}
#undef X
#undef B
#undef H
#undef HB

void
mark(v, CD)
register Object v;
Delay *CD;
{
	register Word *H;
	register Machine *cmr;

	cmr = CMR;

#define B cmr->mr_b
	H = cmr->mr_h;
#define HB cmr->mr_hb

	heapOverflowCheck(cmr->mr_h, wordsof(DelayHeader) + wordsof(Mark));

	DeRefPure(v);
	if(IsRef(v)) {
		((DelayHeader *) H)->h_self = (Object *) StarToUnboundDel(H);
		((DelayHeader *) H)->h_marks = StarToBlock(CD - 1);
		trail(eRefStar(v), cmr->mr_tr);
		*eRefStar(v) = StarToRef(H);
		cmr->mr_h = H + wordsof(DelayHeader);
	} else if(IsDel(v)) {
		register Object *ev;

			/* BUG.  Could check that CD is not already waiting on v */
/*
 * Note that it is very important that we keep v locked while we modify
 * its event list as otherwise some other process can wake the list before
 * we have added the new event.
 */
		ev = &((DelayHeader *)eRef(v))->h_marks;
		trail2(*ev, ev, cmr->mr_tr);
		((Mark *)H)->m_delay = StarToBlock(CD - 1);
		((Mark *)H)->m_nextMark = *ev;
		*ev = StarToList(H);
#ifdef DEBUG
printf("Added Mark %x with Delay %x to\n", H, StarToBlock(CD - 1));
displayTerm(stdout, ((DelayHeader *)eRef(v))->h_marks); printf("\n");
#endif /* DEBUG */
		cmr->mr_h = H + wordsof(Mark);
	} else
		warning("MARKing a non-variable");	/* Maybe */
}

Program WakeUp, PureWakeUp;
Program WakeDebug;
