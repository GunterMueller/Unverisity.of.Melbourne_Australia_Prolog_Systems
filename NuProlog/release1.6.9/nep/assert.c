/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

int
p_makeBMT(key, x, instance, bmt, block)
register Object key;
Object x;
int *instance;			/* BUG?  Is there really any need for this? */
Object *bmt, *block;
{
	int nvars;
	register Object *b;
	
	if(IsNIL(freeList)) {
		b = (Object *) dalloc(7 * sizeof(Word));
		*instance = 0;
		b[0] = StrHeader6_BMT;
		b[1] = MakeSmallInt(0);
		b[2] = 0;
		b[3] = NIL;
		b[4] = NIL;
		b[5] = NIL;
		b[6] = NIL;
	} else {
		b = eRef(freeList);
		freeList = b[3];
		*instance = eSmallInt(b[1]);
	}

	key = copy(iTDATA, b + 4, key, &nvars);
	if(nvars != 0) {
		warning("Key not ground in makeBMT()");
		tdfreeObject(key);				/* Probably not worth bothering. */
		b[3] = freeList;
		freeList = StarToStr(b);
		return(0);
	}
	b[4] = key;			/* Used by compaction code */

	x = copy(iTDATA, b + 3, x, &nvars);
	b[2] = MakeSmallInt(nvars);
	b[3] = x;			/* Used by compaction code */

	*bmt = StarToStr(b);
	*block = StarToBlock(b);
	return(1);
}

/*
 * Link bmt in before (side == 0) or after (side != 0) the BMT spot;
 * or at the tail (side == 0) or head (side != 0) of its property list
 * if spot is an atom.
 */
int
p_linkBMT(side, bmt, spot)
int side;
Object bmt, spot;
{
	register Object *pred, *succ, *new;

	DeRef(bmt);
	new = eRef(bmt);
	DeRef(spot);
	if(IsAtom(spot)) {
		register Atom *a;

		a = (Atom *) eRef(spot);
		if(IsNIL(a->a_prop)) {
			int instance;
			Object newbmt, block;

			(void) p_makeBMT(
						StarToAtom(&Sym_Header),
						spot,
						&instance, &newbmt, &block);
			pred = eRef(newbmt);
			pred[1] = spot;
			pred[5] = newbmt;
			pred[6] = newbmt;
			a->a_prop
				= spot
				= newbmt;
		} else
			spot = a->a_prop;
	}
	if(!IsStr(bmt) || !IsStr(spot)) {
		warning("Str expected in p_linkBMT()");
		return(0);
	}
	if(new[0] != StrHeader6_BMT || eRef(spot)[0] != StrHeader6_BMT) {
		warning("$bmt/6 expected in p_linkBMT()");
		return(0);
	}
	if(side) {
		new[5] = spot;
		pred = eRef(spot);
		new[6] = pred[6];
		succ = eRef(pred[6]);
	} else {
		new[6] = spot;
		succ = eRef(spot);
		new[5] = succ[5];
		pred = eRef(succ[5]);
	}
	pred[6] = bmt;
	succ[5] = bmt;
	return(1);
}

/*
 * Make an instance of a $ref/2 or $bmt/6.
 */
int
p_instance(ref, key, value)
register Object ref;
Object *key, *value;
{
	register Object *p;
	register Object instance;
	
	DeRef(ref);
	if(!IsStr(ref)) {
		warning("Str expected in p_instance()");
		return(0);
	}
	instance = NIL;
	p = eRef(ref);
	if(p[0] == StrHeader2_Ref) {
		ref = p[1]; DeRef(ref);
		instance = p[2]; DeRef(instance);
		if(!IsBlock(ref) || !IsSmallInt(instance)) {
			warning("Impossible args to $ref/2 in p_instance()");
			return(0);
		}
		p = eRef(ref);
	}
	if(p[0] == StrHeader6_BMT) {
		if(instance != NIL && instance != p[1]) {
			return(0);
		}
		*key = uncopy(p[4], 0);
		*value = uncopy(p[3], eSmallInt(p[2]));
		return(1);
	} else {
		warning("$ref/2 or $bmt/6 expected in p_instance()");
		return(0);
	}
}

/*
 * Erase a $ref/2 or $bmt/6.
 */
int
p_erase(ref)
register Object ref;
{
	register Object *p;
	register Object instance;

	DeRef(ref);
	if(!IsStr(ref)) {
		warning("Str expected in erase()");
		return(0);
	}
	instance = NIL;
	p = eRef(ref);
	if(p[0] == StrHeader2_Ref) {
		ref = p[1]; DeRef(ref);
		instance = p[2]; DeRef(instance);
		if(!IsBlock(ref) || !IsSmallInt(instance)) {
			warning("Impossible args to $ref/2 in erase()");
			return(0);
		}
		p = eRef(ref);
	}
	if(p[0] == StrHeader6_BMT) {
		if(instance != NIL && instance != p[1]) {
			return(0);
		}
		p[1] = MakeSmallInt(eSmallInt(p[1]) + 1);
		tdfreeObject(p[3]);
		tdfreeObject(p[4]);
		p[4] = NIL;
		p[3] = freeList;
		freeList = StarToStr(p);
		eRef(p[5])[6] = p[6];
		eRef(p[6])[5] = p[5];
		p[5] = NIL;
		p[6] = NIL;
		return(1);
	} else {
		warning("$ref/2 or $bmt/6 expected in erase()");
		return(0);
	}
}

int
p_proplist(atom, key, props)
Object atom, key, *props;
{
	register Object *b, *p;
	register Word *H;
	Object header;
	int all;
	Object dummy[2];

	DeRef(atom);
	if(!IsAtom(atom)) {
		warning("Atom expected in p_proplist()");
		return(0);
	}
	header = ((Atom *) eRef(atom))->a_prop;
	if(header == NIL) {
		*props = NIL;
		return(1);
	}
	b = eRef(eRef(header)[6]);

	DeRef(key);
	all = IsVar(key);
	
	H = CMR->mr_h;
	dummy[1] = NIL;
	p = dummy;

	while(IsSmallInt(b[1])) {
		if(all || identical(b[4], key)) {
			heapOverflowCheck(6, H);
			p[1] = StarToList(H);
			H[0] = StarToStr(H + 2);
			H[1] = NIL;
			H[2] = StrHeader3_Prop;
			H[3] = b[4];
			H[4] = StarToBlock(b);
			H[5] = b[1];
			p = H;
			H += 6;
		}
		b = eRef(b[6]);							/* No DeRef() needed. */
	}

	CMR->mr_h = H;
	*props = dummy[1];
	return(1);
}

/*
 * Abolish pred/arity.
 * Assumes that the Prolog code will clean up any clauses and properties.
 *
 * Note that other code may still be using the Functor and that space is
 * not reclaimed.
 */
int
p_abolish(X1, X2)
register Object X1, X2;
{
	register Functor *f;
	register Atom *pred;
	register int arity;

	DeRef(X1);
	if(!IsAtom(X1)) {
		warning("Atom expected in p_abolish()");
		return(0);
	}
	pred = (Atom *) eRef(X1);

	DeRef(X2);
	if(!IsSmallInt(X2)) {
		warning("Integer expected in p_abolish()");
		return(0);
	}
	arity = eSmallInt(X2);

	f = lookupFunctor(arity, pred->a_functors);
	if(f != (Functor *) NULL) {
		f->f_codeType = fCOMPILED;
		f->f_sourceFile = NIL;
		f->f_code = undefinedPredicate;
	}
	return(1);
}

/*
 * A BMV counter and a variable vector pointer.
 */
static int VC;
static Object *VV;

/*
 * Base and top of the area being copied or uncopied into.
 */
static Word *BOC, *TOC;

static Object tcopy();

/*
 * Copy a term into the code space, numbering variables with BMV terms.
 * The bindings made are trailed.
 */
Object
copy(area, owner, x, nvars)
int area;
Word *owner;
Object x;
int *nvars;
{
	TrailRecord *TRB;

	VC = 0;
	TRB = CMR->mr_tr;

	DeRef(x);

	switch(area) {
	when iDATA:
		BOC = (Word *) dalloc(sizeOfTerm(x));
	when iTDATA:
		BOC = (Word *) tdalloc(sizeOfTerm(x), owner);
	break;
	default:
		panic("Illegal data area given to copy()");
	}

	/*
	 * Filter out the immediate Object types to avoid screwing up tcopy.
	 */
	switch(eType(x)) {
	when tREF:
	case tDEL:
		x = MakeBMV(VC++);
	when tICN:
	case tBMV:
		break;

	break;
	default:
		x = tcopy(x);
	}

	CMR->mr_tr = failure(CMR->mr_tr, TRB);
	*nvars = VC;
	return x;
}

static Object
tcopy(x)
register Object x;
{
	DeRef(x);
	switch(eType(x)) {
	when tREF: {
		register Object y;

		y = MakeBMV(VC++);
		forcedBind(eRef(x), y, CMR->mr_tr);
		return(y);
	}
	when tDEL: {
		register Object y;

		y = MakeBMV(VC++);
		forcedBind2(eRef(x), y, CMR->mr_tr);
		return(y);
	}
	when tICN:
	case tBMV:
		return(x);
	when tUCN:
		if(IsFloat(x)) {
			register Real *S;
	
			S = (Real *) ALIGNFLOAT(BOC);
			BOC = (Word *) (S + 1);
			*S = eFloat(x);
			return(StarToFloat(S));
		} else if(IsInt32(x)) {
			register Word *S;
	
			S = (Word *) BOC;
			BOC += wordsof(Word);
			*S = eInt32(x);
			return(StarToInt32(S));
		} else
			panic("Unknown UCN type in tcopy()");
	when tLST: {
		register Object *S, *T;

		S = BOC;
		BOC += 2;
		T = eRef(x);
		S[0] = tcopy(T[0]);
		S[1] = tcopy(T[1]);
		return(StarToList(S));
	}
	when tCHR: {
		register char *S, *T;

		S = (char *) BOC;
		T = (char *) eRef(x);
		BOC += NWORDS(1 + strlen(T));
		(void) strcpy(S, T);
		return(StarToString(S));
	}
	when tSTR: {
		register Object *S, *T;
		register int n;

		S = BOC;
		T = eRef(x);
		n = eNArgs((Structure) T[0]);
		BOC += n + 1;
		S[0] = T[0];
		for( ; n > 0; n--)
			S[n] = tcopy(T[n]);
		return(StarToStr(S));
	}
	}
}

static void tuncopy();

Object
uncopy(x, nvars)
register Object x;
register int nvars;
{
	register Object *vars;
	register Object *ux;

/* Put this back if ground objects are to be shared rather than copied.
	if(nvars == 0)
		return(x);
*/

	stackOverflowCheck(nvars, CMR->mr_a);
	VV
		= vars
		= CMR->mr_a;
	ux = CMR->mr_h;
	BOC = ux + 1;
	TOC = heapMax;
	if(BOC >= TOC)
		panic("Heap overflow");

	for(nvars-- ; nvars >= 0; nvars--)
		vars[nvars] = 0;

	tuncopy(ux, x);
	if(BOC >= TOC)
		panic("Heap overflow");
	CMR->mr_h = BOC;
	x = *ux;
	if(IsVar(x))
		x = MakeIndirect(x);
	return(x);
}

static void
tuncopy(p, x)
register Object *p, x;
{
	DeRef(x);
	switch(eType(x)) {
	when tREF:
	case tDEL:
		panic("Variable found in (t)uncopy");
	when tICN:
		*p = x;
	when tUCN:
		if(IsFloat(x)) {
			register Real *S;

			S = (Real *) ALIGNFLOAT(BOC);
			BOC = (Word *) (S + 1);
			if(BOC > TOC)
				return;
			*S = eFloat(x);
			*p = StarToFloat(S);
		} else if(IsInt32(x)) {
			register Word *S;

			S = (Word *)BOC;
			BOC += wordsof(Word);
			if(BOC > TOC)
				return;
			*S = eInt32(x);
			*p = StarToInt32(S);
		} else
			panic("Unknown UCN type in tuncopy()");
	when tLST: {
		register Object *S, *T;

		S = (Object *)BOC;
		BOC += 2;
		if(BOC > TOC)
			return;
		T = eRef(x);
		tuncopy(S + 0, T[0]);
		tuncopy(S + 1, T[1]);
		*p = StarToList(S);
	}
	when tCHR: {
/* Change if ground objects are to be copied rather than shared.
		*p = x;
*/
		register Object *S;
		register char *c;

		S = (Object *)BOC;
		c = (char *) eRef(x);
		BOC += NWORDS(1 + strlen(c));
		if(BOC > TOC)
			return;
		strcpy((char *)S, c);
		*p = StarToString(S);
	}
	when tSTR: {
		register Object *S, *T;
		register int n;

		S = BOC;
		T = eRef(x);
		n = eNArgs((Structure) T[0]);
		BOC += n + 1;
		if(BOC > TOC)
			return;
		S[0] = T[0];
		for( ; n > 0; n--)
			tuncopy(S + n, T[n]);
		*p = StarToStr(S);
	}
	when tBMV: {
		register Object *var;

		var = VV + eCValue(x);
		if(*var == 0) {
			*var = StarToRef(p);
			*p = StarToUnboundRef(p);
		} else
			*p = *var;
	}
	}
}
