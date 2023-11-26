/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

static Instruction WName[] = {
	MakeIns2(cNAME, 0, 1),
	MakeIns0(cPRO)
};

/*
 * p_name() signals trail overflow.
 */
p_name(X1, X2)
register Object X1, X2;
{
	DeRefPure(X1); DeRefPure(X2);
	switch(eType(X1)) {
	when tREF:
	case tDEL: {
		Object V;

		if(	IsVar(X2)
		||	IsVar(V = tail(X2))
		||	(V == NIL && IsVar(V = firstVar(X2)))) {
			register Object *X;
			register Delay *CD;
			Object XS0, XS1;

			X = CMR->mr_x;
			XS0 = X[0]; XS1 = X[1];
			X[0] = X1;
			X[1] = X2;
			/* SaveForBuilding; */
			CD = makeDelay(2, WName);
			mark(X1, CD);
			if(IsVar(X2))
				mark(X2, CD);
			else
				mark(V, CD);
			/* RestoreAfterBuilding; */
			X[0] = XS0; X[1] = XS1;
			return(1);
		} else {
			switch(eType(X2)) {
			when tREF:
			case tDEL:
				panic("Variable in impossible position in p_name");
			when tICN:
				if(IsNIL(X2)) {
					bindVariable(X1, StarToAtom(&SymEmpty));
					return(1);
				} else
					return(0);
			when tLST: {
				register unsigned char *s;
				register Object *H;
				
				H = CMR->mr_h;
				s = listToString(
						X2,
						(unsigned char *)H,
						(unsigned char *)(heapMax),
						0);
				if(s == (unsigned char *) NULL)
					return(0);
				else if(s == (unsigned char *) heapMax)
					panic("Heap Overflow");
				else {
					X2 = StarToAtom(enterAtom((char *)H, stab, (Atom *) NULL));
					CMR->mr_h = H;	/* Throw away the string */
					bindVariable(X1, X2);	/* Lazy */
					return(1);
				}
			}
			when tCHR:
				bindVariable(X1,
					StarToAtom(enterAtom((char *) eRef(X2),
						stab, 
						(Atom *) NULL)));
				return(1);
			when tUCN:
			case tSTR:
			case tBMV:
				return(0);
			}
		}
	}
	when tICN:
		if(IsAtom(X1)) {
			X1 = ((Atom *) eRef(X1))->a_pname;
			if(IsVar(X2))
				bindVariable(X2, X1);
			else {
				X2 = unify(X2, X1);
				trailOverflowSignal(0, CMR->mr_tr);
			}
			return(X2);
		} else
			return(0);
	when tUCN:
	case tLST:
	case tCHR:
	case tSTR:
	case tBMV:
		return(0);
	}
}

/*
 * Put a new property on the property list of an atom.
 * Doesn't care about duplication or whether the property is permanent.
 */
addprop(a, key, value)
Object key, value;
Atom *a;
{
	int instance;
	Object bmt, block;
	register Object *pred, *succ;

	if(a->a_prop == NIL) {
		(void) p_makeBMT(
					StarToAtom(&Sym_Header),
					StarToAtom(a),
					&instance, &bmt, &block);
		a->a_prop = bmt;
		pred = eRef(bmt);
		pred[1] = StarToAtom(a);
		pred[5]
			= pred[6]
			= bmt;
	} else
		pred = eRef(a->a_prop);

	(void) p_makeBMT(key, value, &instance, &bmt, &block);
	succ = eRef(pred[6]);
	pred[6] 
		= succ[5]
		= bmt;
	eRef(bmt)[5] = StarToStr(pred);
	eRef(bmt)[6] = StarToStr(succ);
}

unsigned char *
listToString(X, base, top, least)
register Object X;
register unsigned char *base, *top;
register int least;
{
	register Object C;

	DeRef(X);
	while(IsList(X)) {
		register Object *L;

		if(base > top)
			return(top);
		L = eRef(X);
		C = L[0];
		DeRef(C);
		if(IsInt(C)) {
			C = eInt(C);
			if(C < least || C > MAXCHAR)
				return((unsigned char *) NULL);
			*base++ = (unsigned char) C;
		} else
			return((unsigned char *) NULL);
		X = L[1];
		DeRef(X);
	}
	if(IsNIL(X)) {
		if(base > top)
			return(top);
		*base++ = '\0';
	} else if(IsString(X)) {
		register unsigned char *end;

		X = (Object) eRef(X);
		end = base + 1 + strlen((char *) X);
		if(end > top)
			return(top);
		(void) strcpy((char *)base, (char *)X);
		base = end;
	} else
		return((unsigned char *) NULL);
	
	return(base);
}

/*
 * Convert a list to a string.
 * This is a one-way predicate.  It simply fails if X1 is not a list of
 * valid character-codes or if X2 is a list.
 */
int
p_listToString(X1, X2)
register Object X1, X2;
{
	register unsigned char *s;
	register Object *H;
	register Object R;

	H = CMR->mr_h;
	s = listToString(
			X1,
			(unsigned char *)H,
			(unsigned char *)heapMax,
			0);
	if(s == (unsigned char *) NULL)
		return(0);
	else if(s == (unsigned char *) heapMax)
		panic("Heap Overflow");
	else if(*(char *)H == '\0')
		R = NIL;
	else {
		CMR->mr_h = (Object *)ALIGNW(s);	/* In case unify wakes anything */
		R = StarToString((unsigned char *) H);
	}
	DeRef(X2);
	if(IsVar(X2)) {
		bindVariable(X2, R);
		return(1);
	} else if(X2 == R) {	/* NIL */
		return(1);
	} else if(strcmp((unsigned char*)eRef(X2), (unsigned char*)eRef(R)) == 0) {
		return(1);
	} else {
		CMR->mr_h = H;
		return(0);
	}
}
