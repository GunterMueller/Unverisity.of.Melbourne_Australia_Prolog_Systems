/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

TrailRecord *
trimTrail(TR, B, HB)
register TrailRecord *TR;
register Choice *B;
register Word *HB;
{
	register TrailRecord *TRB;
	register Word *sb;
	register TrailRecord *t;

	TRB = (B-1)->c_tr;

	sb = stackBase;

	for(t = TRB; t < TR; t++) {
		register Word *s;

		s = t->tr_address;
		if(IsCFunctNumber(s))
			(*cFunctTable[s - cFunctDummy])(t->tr_value);
		else if((s < (Word *)B && s >= sb) || s < HB) /* BUG!  NO_LOCAL_VARS */
			*TRB++ = *t;
	}

	return(TRB);
}

/*
 * Return the first variable found in a given Object,
 * or NIL if none are found.
 */
Object
firstVar(t)
register Object t;
{
	register Object s;
	register int n;

	start:						/* Tail Recursion */
	DeRef(t);
	switch(eType(t)) {
	when tREF:
	case tDEL:
		return(MakeIndirect(t));
	when tICN:
	case tUCN:
	case tBMV:
	case tCHR:
		return(NIL);
	when tLST:
		s = firstVar(*eRef(t));
		if(s != NIL)
			return(s);
		t = *(eRef(t) + 1);
		goto start;
	when tSTR:
		t = (Object)eRef(t); 
		n = eNArgs(*(Structure *)t);
		t = (Object) ((Object *)t + 1);
		for( ; n > 0; n--) {
			s = firstVar(*(Object *)t);
			if(s != NIL)
				return(s);
			t = (Object) ((Object *)t + 1);
		}
		return(NIL);

	break;
	default:
		panic("Impossible type in firstVar");
	}
}

#ifndef INLINE
TrailRecord *
failure(TR, TRB)
register TrailRecord *TR, *TRB;
{

	while(TR > TRB) {
		TR--;
#ifdef DEBUG
if(
	traceflg)
	fprintf(stdout,
	    "RESTORING _%x TO 0x%x\n", (Word)TR->tr_address, TR->tr_value);
#endif /* DEBUG */
		*TR->tr_address = TR->tr_value;
	}
	return(TR);
}
#endif /* INLINE */
