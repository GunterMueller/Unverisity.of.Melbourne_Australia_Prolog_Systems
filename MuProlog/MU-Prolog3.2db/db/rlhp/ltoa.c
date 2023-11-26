#include "types.h"
#include "pred.h"

/* convert from Prolog list of ints to C array of ints */

ltoa(t, l, n, s)
	Ptr t;
	levtype l;
	int n, s[];
{
	Ptr t1;
	levtype l1;

	while(IsComp(t) && ctdict(t) == Ddot && --n) {
		findbind((Ptr)targ(1, t), l, &t1, &l1);
		if(ttype(t1) != TNUM) {
			plerror(EESTR);
			return(ERROR);
		}
		*s++ = tnum(t1);
		findbind((Ptr)targ(2, t), l, &t, &l);
	}
	if(!IsAtom(t) || atdict(t) != Dnil) {
		plerror(EELIST);
		return(ERROR);
	}
	return SUCCEED;
}
