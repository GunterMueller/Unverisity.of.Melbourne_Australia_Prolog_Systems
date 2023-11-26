/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987, 1989 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

Object
deRef(v)
register Object v;
{
#ifdef DEBUG2
	register Object var;

	var = v;
#endif /* DEBUG2 */

	while(IsIndirect(v)) {
#ifdef DEBUG2
		if(eRef(v) == (Object *) 0) {
			printf("VAR = %x, REF = %x\n", var, v);
			panic("Out of range reference in deRef");
		}
#endif /* DEBUG2 */
		v = *(Object *)eRef(v);
	}

#ifdef DEBUG2
	if(IsDel(v)) {
		register Object w, *l;
		register int dud;

		dud = 0;
		w = ((DelayHeader *)eRef(v))->h_marks;
		while(IsList(w)) {
			l = eRef(w);
			if(!IsBlock(l[0])) {
				dud = 1;
				printf("Event = %x in Mark %x\n", l[0], l);
			}
			w = l[1];
		}
		if(!IsBlock(w)) {
			dud = 1;
			printf("Event = %x in Mark %x\n", w, l);
		}
		if(dud) {
			displayTerm(stdout, var); printf("\n");
			displayTerm(stdout, ((DelayHeader *)eRef(v))->h_marks); printf("\n");
			panic("Non-block in event list");
		}
	}
#endif /* DEBUG2 */

	return(v);
}

#define pair(x, y) (eType(x) << lofType | eType(y))
#define casepair(x, y) (x << lofType | y)
#define ctpair(x, y) (eCType(x) << lofCType | eCType(y))
#define ctcasepair(x, y) (x << lofCType | y)

/*
 * Note that unify does not shift stacks.  It expects to have enough
 * heap and trail to work with and panics if it runs out.
 */
Object
unify(v1, v2)
register Object v1, v2;
{
#define B cmr->mr_b
#define H cmr->mr_h
#define HB cmr->mr_hb
	register Object v;

	DeRef(v1); DeRef(v2);

	if(v1 == v2)
		return(v1);

	switch(pair(v1, v2)) {
	when casepair(tICN, tLST):
	case casepair(tICN, tCHR):
	case casepair(tICN, tSTR):
	case casepair(tICN, tBMV):
	case casepair(tUCN, tLST):
	case casepair(tUCN, tCHR):
	case casepair(tUCN, tSTR):
	case casepair(tUCN, tBMV):
	case casepair(tLST, tICN):
	case casepair(tLST, tUCN):
	case casepair(tLST, tSTR):
	case casepair(tLST, tBMV):
	case casepair(tCHR, tICN):
	case casepair(tCHR, tUCN):
	case casepair(tCHR, tSTR):
	case casepair(tCHR, tBMV):
	case casepair(tSTR, tICN):
	case casepair(tSTR, tUCN):
	case casepair(tSTR, tLST):
	case casepair(tSTR, tCHR):
	case casepair(tSTR, tBMV):
	case casepair(tBMV, tICN):
	case casepair(tBMV, tUCN):
	case casepair(tBMV, tLST):
	case casepair(tBMV, tCHR):
	case casepair(tBMV, tSTR):
			/* Note that the null string is always represented by NIL */
		return(0);
	when casepair(tICN, tICN):
	case casepair(tBMV, tBMV):
		return(0);
	when casepair(tUCN, tUCN):
		switch(ctpair(v1, v2)) {
		when ctcasepair(ctFLT, ctFLT):
			if(eFloat(v1) == eFloat(v2))
				return(v1);
			else
				return(0);
		when ctcasepair(ctI32, ctI32):
			if(eInt32(v1) == eInt32(v2))
				return(v1);
			else
				return(0);
		break;
		default:
			panic("Impossible ucn type in unify()");
		}
	when casepair(tICN, tUCN):
		/* May change if additional UCN's are defined. */
		if(IsFloat(v2) || IsInt32(v2))
			return(0);
		else
			panic("Unknown UCN type in unify()");
	when casepair(tUCN, tICN):
		/* May change if additional UCN's are defined. */
		if(IsFloat(v1) || IsInt32(v1))
			return(0);
		else
			panic("Unknown UCN type in unify()");
	when casepair(tSTR, tSTR): {
		v = v1;
		v1 = (Object)eRef(v1);
		v2 = (Object)eRef(v2);
		if(*(Structure *)v1 == *(Structure *)v2) {
			register int i;

			i = eNArgs(*(Structure *)v1);
			do {
				v1 += sizeof(Object);
				v2 += sizeof(Object);
				if(!unify(StarToRef(v1), StarToRef(v2)))
					return(0);
			} while(--i > 0);
			return(v);
		} else
			return(0);
	}
	when casepair(tLST, tLST): {
		v = v1;
		v1 = (Object)eRef(v1);
		v2 = (Object)eRef(v2);

		if(!unify(StarToRef(v1), StarToRef(v2)))
			return(0);
		v1 += sizeof(Object);
		v2 += sizeof(Object);
		if(!unify(StarToRef(v1), StarToRef(v2)))
			return(0);

		return(v);
	}
	when casepair(tCHR, tLST): {	/* Very Lazy.  Do it properly sometime */
		unsigned char *s;

L_CHR_LST:
		v = v1;
		s = (unsigned char *) eRef(v1);

		for( ; ; ) {
			v1 = MakeSmallInt(*s);
			if(v1 == MakeSmallInt(0))
				if(unify(NIL, v2))
					return(v);
				else
					return(0);
			if(!IsList(v2))
				if(unify(StarToString(s), v2))
					return(v);
				else
					return(0);
			v2 = (Object)eRef(v2);
			if(!unify(v1, ((Object *)v2)[0]))
				return(0);
			s++;
			v2 = ((Object *)v2)[1];
			DeRef(v2);
		}
	}
	when casepair(tLST, tCHR):		/* Very Lazy.  Do it properly sometime */
		v = v1;
		v1 = v2;
		v2 = v;
		goto L_CHR_LST;
	when casepair(tCHR, tCHR):
		if(strcmp((char *) eRef(v1), (char *) eRef(v2)) != 0)
			return(0);
		else
			return(v1);
	when casepair(tREF, tICN):
	case casepair(tREF, tUCN):
	case casepair(tREF, tLST):
	case casepair(tREF, tCHR):
	case casepair(tREF, tSTR):
	case casepair(tREF, tBMV): {
		register Machine *cmr;

		cmr = CMR;
		*eRefStar(v1) = v2;
		trail(eRefStar(v1), cmr->mr_tr);
		binding(v1);
		return(v2);
	}
	when casepair(tREF, tDEL): {
		register Machine *cmr;

		cmr = CMR;
		*eRefStar(v1)
			= v
			= MakeIndirect(v2);
		trail(eRefStar(v1), cmr->mr_tr);
		binding(v1);
		return(v);
	}
	when casepair(tDEL, tICN):
	case casepair(tDEL, tUCN):
	case casepair(tDEL, tLST):
	case casepair(tDEL, tCHR):
	case casepair(tDEL, tSTR):
	case casepair(tDEL, tBMV): {
		register Machine *cmr;

		cmr = CMR;
		v1 = (Object) eRef(v1);
		bindDel(v1, v2, cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
		binding(v1);
		return(v2);
	}
	when casepair(tICN, tDEL):
	case casepair(tUCN, tDEL):
	case casepair(tLST, tDEL):
	case casepair(tCHR, tDEL):
	case casepair(tSTR, tDEL):
	case casepair(tBMV, tDEL): {
		register Machine *cmr;

		cmr = CMR;
		v2 = (Object) eRef(v2);
		bindDel(v2, v1, cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
		binding(v2);
		return(v1);
	}
	when casepair(tICN, tREF):
	case casepair(tUCN, tREF):
	case casepair(tLST, tREF):
	case casepair(tCHR, tREF):
	case casepair(tSTR, tREF):
	case casepair(tBMV, tREF): {
		register Machine *cmr;

		cmr = CMR;
		*eRefStar(v2) = v1;
		trail(eRefStar(v2), cmr->mr_tr);
		binding(v2);
		return(v1);
	}
	when casepair(tDEL, tREF): {
		register Machine *cmr;

		cmr = CMR;
		*eRefStar(v2)
			= v
			= MakeIndirect(v1);
		trail(eRefStar(v2), cmr->mr_tr);
		binding(v2);
		return(v);
	}
	when casepair(tREF, tREF): {
		register Machine *cmr;

		cmr = CMR;
		if(eRefStar(v1) < eRefStar(v2)) {
			*eRefStar(v2) = v1;
			trail(eRefStar(v2), cmr->mr_tr);
			binding(v2);
			return(v1);
		} else {
			*eRefStar(v1) = v2;
			trail(eRefStar(v1), cmr->mr_tr);
			binding(v1);
			return(v2);
		}
	}
	when casepair(tDEL, tDEL): {
		/*
		 * BUG! (well, sort of!)
		 * This works by arranging for all the goals in one of the
		 * objects to be woken.  Usually they then go back to sleep.
		 * This is an expensive operation, consuming both time and
		 * heap and trail space, but may well be the best way of
		 * making sure that things like X ~= Y, X = Y are woken when
		 * they should be.
		 */
		register Machine *cmr;

		cmr = CMR;
		if(eRef(v1) < eRef(v2)) {
			v2 = (Object) eRef(v2);
			bindDel(v2, v1,
				cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
			binding(v2);
			return(v1);
		} else {
			v1 = (Object) eRef(v1);
			bindDel(v1, v2,
				cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
			binding(v1);
			return(v2);
		}
	}

	break;
	default:
		fprintf(stderr, "Unifying types %x and %x\n", eType(v1), eType(v2));
		panic("Impossible (or not yet defined) Type in unify");
#ifdef LINT
		return(0);
#endif /* LINT */
	}
}

Object
identical(v1, v2)
register Object v1, v2;
{
	register Object v;
	register int i;

	DeRef(v1); DeRef(v2);

	if(v1 == v2)
		return(v1);

	switch(pair(v1, v2)) {
	when casepair(tREF, tREF):
	case casepair(tDEL, tDEL):
	case casepair(tREF, tICN):
	case casepair(tREF, tUCN):
	case casepair(tREF, tLST):
	case casepair(tREF, tCHR):
	case casepair(tREF, tSTR):
	case casepair(tREF, tBMV):
	case casepair(tREF, tDEL):
	case casepair(tDEL, tICN):
	case casepair(tDEL, tUCN):
	case casepair(tDEL, tLST):
	case casepair(tDEL, tCHR):
	case casepair(tDEL, tSTR):
	case casepair(tDEL, tBMV):
	case casepair(tICN, tDEL):
	case casepair(tUCN, tDEL):
	case casepair(tLST, tDEL):
	case casepair(tCHR, tDEL):
	case casepair(tSTR, tDEL):
	case casepair(tBMV, tDEL):
	case casepair(tICN, tREF):
	case casepair(tUCN, tREF):
	case casepair(tLST, tREF):
	case casepair(tCHR, tREF):
	case casepair(tSTR, tREF):
	case casepair(tBMV, tREF):
	case casepair(tDEL, tREF):
	case casepair(tICN, tICN):
	case casepair(tICN, tLST):
	case casepair(tICN, tCHR):
	case casepair(tICN, tSTR):
	case casepair(tICN, tBMV):
	case casepair(tUCN, tLST):
	case casepair(tUCN, tCHR):
	case casepair(tUCN, tSTR):
	case casepair(tUCN, tBMV):
	case casepair(tLST, tICN):
	case casepair(tLST, tUCN):
	case casepair(tLST, tSTR):
	case casepair(tLST, tBMV):
	case casepair(tCHR, tICN):
	case casepair(tCHR, tUCN):
	case casepair(tCHR, tSTR):
	case casepair(tCHR, tBMV):
	case casepair(tSTR, tICN):
	case casepair(tSTR, tUCN):
	case casepair(tSTR, tLST):
	case casepair(tSTR, tCHR):
	case casepair(tSTR, tBMV):
	case casepair(tBMV, tICN):
	case casepair(tBMV, tUCN):
	case casepair(tBMV, tLST):
	case casepair(tBMV, tCHR):
	case casepair(tBMV, tSTR):
	case casepair(tBMV, tBMV):
			/* Note that the null string is always represented by NIL */
		return(0);
	when casepair(tUCN, tUCN):
		switch(ctpair(v1, v2)) {
		when ctcasepair(ctFLT, ctFLT):
			if(eFloat(v1) == eFloat(v2))
				return(v1);
			else
				return(0);
		when ctcasepair(ctI32, ctI32):
			if(eInt32(v1) == eInt32(v2))
				return(v1);
			else
				return(0);
		break;
		default:
			panic("Impossible ucn type in identical()");
		}
	when casepair(tICN, tUCN):
		/* May change if additional UCN's are defined. */
		if(IsFloat(v2) || IsInt32(v2))
			return(0);
		else
			panic("Unknown UCN type in identical()");
	when casepair(tUCN, tICN):
		/* May change if additional UCN's are defined. */
		if(IsFloat(v1) || IsInt32(v1))
			return(0);
		else
			panic("Unknown UCN type in identical()");
	when casepair(tSTR, tSTR): {
		v = v1;
		v1 = (Object)eRef(v1);
		v2 = (Object)eRef(v2);
		if(*(Structure *)v1 == *(Structure *)v2) {
			i = eNArgs(*(Structure *)v1);
			do {
				v1 += sizeof(Object);
				v2 += sizeof(Object);
				if(!identical(StarToRef(v1), StarToRef(v2)))
					return(0);
			} while(--i > 0);
			return(v);
		} else
			return(0);
	}
	when casepair(tLST, tLST): {
		v = v1;
		v1 = (Object)eRef(v1);
		v2 = (Object)eRef(v2);

		if(!identical(StarToRef(v1), StarToRef(v2)))
			return(0);
		v1 += sizeof(Object);
		v2 += sizeof(Object);
		if(!identical(StarToRef(v1), StarToRef(v2)))
			return(0);

		return(v);
	}
	when casepair(tCHR, tLST): {	/* Very Lazy.  Do it properly sometime */
L_CHR_LST:
		v = v1;
		v1 = (Object)eRef(v1);
		v2 = (Object)eRef(v2);

		if(!identical(MakeSmallInt(*(unsigned char *)v1), StarToRef(v2)))
			return(0);
		v1 += sizeof(unsigned char);
		v2 += sizeof(Object);
		if(*(unsigned char *)v1 == (unsigned char) '\0') {
			if(!identical(NIL, StarToRef(v2))) 
				return(0);
		} else {
			if(!identical(StarToString(v1), StarToRef(v2)))
				return(0);
		}

		return(v);
	}
	when casepair(tLST, tCHR):		/* Very Lazy.  Do it properly sometime */
		v = v1;
		v1 = v2;
		v2 = v;
		goto L_CHR_LST;
	when casepair(tCHR, tCHR):
		if(strcmp((char *) eRef(v1), (char *) eRef(v2)) != 0)
			return(0);
		else
			return(v1);

	break;
	default:
		fprintf(stderr, "Types %x and %x\n", eType(v1), eType(v2));
		panic("Impossible (or not yet defined) Type in identical");
#ifdef LINT
		return(0);
#endif /* LINT */
	}
}

/*
 * Bind a dereferenced variable to a dereferenced value.
 * Special case of unify().
 */
#undef TRAILOVERFLOWSIGNAL
#define TRAILOVERFLOWSIGNAL 1
#undef HEAPOVERFLOWSIGNAL
#define HEAPOVERFLOWSIGNAL 1
void
bindVariable(var, val)
register Object var, val;
{
#define B cmr->mr_b
#define H cmr->mr_h
#define HB cmr->mr_hb
	register Machine *cmr;

#ifdef DEBUG
	if(var != deRef(var) || val != deRef(val))
		panic("Argument not de-referenced in bindVariable()");
	if(!IsVar(var))
		panic("Variable expected in bindVariable()");
#endif /* DEBUG */

	if(var == val)
		return;

	cmr = CMR;

	if(IsRef(var)) {
		if(IsRef(val)) {
			if(eRefStar(var) < eRefStar(val)) {
				*eRefStar(val) = var;
				trail(eRefStar(val), cmr->mr_tr);
				binding(val);
				return;
			} else {
				*eRefStar(var) = val;
				trail(eRefStar(var), cmr->mr_tr);
				binding(var);
				return;
			}
		} else if(IsDel(val)) {
			*eRefStar(var) = MakeIndirect(val);
			trail(eRefStar(var), cmr->mr_tr);
			binding(var);
			return;
		} else {
			*eRefStar(var) = val;
			trail(eRefStar(var), cmr->mr_tr);
			binding(var);
			return;
		}
	} else /* if(IsDel(var)) */ {
		if(IsRef(val)) {
			*eRefStar(val) = MakeIndirect(var);
			trail(eRefStar(val), cmr->mr_tr);
			binding(val);
			return;
		} else if(IsDel(val)) {
			/*
			 * BUG! (well, sort of!)
			 * This works by arranging for all the goals in one of the
			 * objects to be woken.  Usually they then go back to sleep.
			 * This is an expensive operation, consuming both time and
			 * heap and trail space, but may well be the best way of
			 * making sure that things like X ~= Y, X = Y are woken when
			 * they should be.
			 */
			if(eRef(var) < eRef(val)) {
				val = (Object) eRef(val);
				bindDel(val, var,
					cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
				binding(val);
				return;
			} else {
				var = (Object) eRef(var);
				bindDel(var, val,
					cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
				binding(var);
				return;
			}
		} else {
			var = (Object) eRef(var);
			bindDel(var, val,
				cmr->mr_h, cmr->mr_a, cmr->mr_tr, cmr->mr_w, {}, {});
			binding(var);
			return;
		}
	}

#ifdef LINT
		return;
#endif /* LINT */
}
