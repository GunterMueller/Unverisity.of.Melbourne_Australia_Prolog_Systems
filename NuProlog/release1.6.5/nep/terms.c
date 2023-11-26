/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

static Instruction WUniv[] = {
	MakeIns2(cUNIV, 0, 1),
	MakeIns0(cPRO)
};

static Instruction WFunctor[] = {
	MakeIns3(cFUNCTOR, 0, 1, 2),
	MakeIns0(cPRO)
};

static Instruction WArg[] = {
	MakeIns3(cARG, 0, 1, 3),
	MakeIns2(cGVALX, 2, 3),
	MakeIns0(cPRO)
};

/*
 * Non-logical.
 */
int
p_occurs(X1, X2)
register Object X1, X2;
{
	register int n;

	DeRef(X1);
	switch(eType(X1)) {
	when tREF:
	case tDEL:
	case tICN:
	case tUCN:
	case tBMV:
loop:							/* I'll rot in hell for this too! */
		DeRef(X2);
		switch(eType(X2)) {
		when tREF:
		case tDEL:
		case tICN:
		case tBMV:
			return(X1 == X2);
		when tUCN:
			if(IsFloat(X1) && IsFloat(X2))
				return(eFloat(X1) == eFloat(X2));
			else if(IsInt32(X1) && IsInt32(X2))
				return(eInt32(X1) == eInt32(X2));
			else
				return(0);
		when tLST:
			X2 = (Object) eRef(X2);
			if(p_occurs(X1, ((Object *) X2)[0]))
				return(1);
			else {
				X2 = ((Object *) X2)[1];
				goto loop;
			}
		when tCHR:
			if(IsNIL(X1))
				return(1);
			else if(IsSmallInt(X1))					/* BUG! */
				return(index((char *)eRef(X2), eSmallInt(X1)) != NULL);
			else
				return(0);
		when tSTR: {
			register Object *p;

			p = eRef(X2);
			for(n = eNArgs(p[0]) - 1, p++; n > 0; n--)
				if(p_occurs(X1, *p++))
					return(1);
			X2 = *p;
			goto loop;
		}
		}
	when tLST:
	case tCHR:
	case tSTR:
		warning("Structured first argument to occurs/1");
		return(0);
	}
}

/*      Array for handling lists as the first argument of p_univ. */
static Object dummyStructure[3];

/*
 * =../2.  Just fails on inappropriate arguments.
 *
 * Signals trail overflow.
 */
int
p_univ(X1, X2)
register Object X1, X2;
{
	DeRefPure(X1); DeRefPure(X2);
	switch(eType(X1)) {
	when tREF:
	case tDEL: {
		register Object V, f;

		if(IsList(X2)) {
			f = eRef(X2)[0]; DeRef(f);
			if(IsIConst(f)) {
				if(!IsAtom(f)) {
					/*
					 * Sometimes I wonder why I didn't just write univ in
					 * Prolog.  The point of this bit of code is that
					 * X =.. 1.Y should bind X = 1, Y = [] rather than delay.
					 */
					V = eRef(X2)[1]; DeRef(V);
					if(IsVar(V))
						bindVariable(V, NIL);
					else if(!IsNIL(V))
						return(0);
					DeRef(X1);		/* consider X =.. 1.X */
					if(IsVar(X1))
						bindVariable(X1, f);
					else if(X1 != f)
						return(0);
					return(1);
				}
			} else if(!IsVar(f))
				return(0);
		} else
			f = NIL;			/* An arbitrary non-var */
		V = tail(X2);
		if(!IsVar(V)) {
			if(!IsNIL(V))
				return(0);
			V = f;
		}
		if(IsVar(V)) {
			register Object *X;
			register Delay *CD;
			Object XS0, XS1;

			X = CMR->mr_x;
			XS0 = X[0]; XS1 = X[1];
			X[0] = X1;
			X[1] = X2;
			/* SaveForBuilding; */
			CD = makeDelay(2, WUniv);
			mark(X1, CD);
			mark(V, CD);
			/* RestoreAfterBuilding; */
			X[0] = XS0; X[1] = XS1;
			return(1);
		} else
			switch(eType(X2)) {
			when tLST: {
				register Object *L;

				L = eRef(X2);
				if(IsAtom(f)) {
					register Word *H;
					register int n;

					H = CMR->mr_h;
					switch(eCType(f)) {
					when ctATM:
						break;
					when ctINT:
						X2 = L[1]; DeRef(X2);
						if(IsNIL(X2)) {
							bindVariable(X1, L[0]);
							return(1);
						} else
							return(0);

					break;
					default:
						return(0);
					}

					X2 = L[1]; DeRef(X2);
					if(IsNIL(X2)) {
						bindVariable(X1, f);
						return(1);
					}
					n = 0;
					H++;
					if(H >= heapMax)
						panic("Heap Overflow somewhere nasty in univ 1");
					while(IsList(X2)) {
						L = eRef(X2);
						X2 = L[0]; DeRefPure(X2);
						*H++ = X2;
						if(H >= heapMax)
							panic("Heap Overflow somewhere nasty in univ 2");
						n++;
						X2 = L[1]; DeRef(X2);
					}
					if(IsString(X2)) {
						register char *s;

						for(s = (char *) eRef(X2); *s != '\0'; s++) {
							*H++ = MakeSmallInt(*s);
							if(H >= heapMax)
								panic("Heap Overflow somewhere in univ 3");
							n++;
						}
						X2 = NIL;
					}
					if(n > MAXARITY) {
						warning("Creating term of too large arity in p_univ()");
						return(0);
					}
					if(IsNIL(X2)) {
						L = CMR->mr_h;
						if(n == 2 && f == StarToAtom(&SymCons)) {
							L[0] = L[1];
							L[1] = L[2];
							CMR->mr_h = H - 1;
							bindVariable(X1, StarToList(L));
							return(1);
						} else {
							L[0] = MakeStrHeader(n, f);
							CMR->mr_h = H;
							bindVariable(X1, StarToStr(L));
							return(1);
						}
					} else
						return(0);				/* Don't alter CMR->mr_h */
				} else if(IsNIL(deRef(L[1]))) {
					switch(eType(f)) {
					when tICN:
					case tUCN:
					case tBMV:
						bindVariable(X1, f);
						return(1);

					break;
					default:
						return(0);
					}
				} else
					return(0);
			}
			when tCHR: {
				register char *s;

				s = (char *) eRef(X2);
				if(s[1] == '\0') {
					bindVariable(X1, MakeSmallInt(s[0]));
					return(1);
				} else
					return(0);
			}

			break;
			default:
			return(0);
		}
	}
	when tLST:
	case tCHR:
		dummyStructure[0] = StrHeader2Cons;
		if(IsList(X1)) {
			dummyStructure[1] = eRef(X1)[0];
			dummyStructure[2] = eRef(X1)[1];
		} else {
			dummyStructure[1] = MakeSmallInt(((unsigned char *)eRef(X1))[0]);
			if(((unsigned char *)eRef(X1))[1] == '\0')
				dummyStructure[2] = NIL;
			else
				dummyStructure[2] = MakeString(((unsigned char *)eRef(X1)) + 1);
		}
		X1 = StarToStr(dummyStructure);
		/* Fall through */
	case tSTR: {
		register Word *H;
		register Object *S, L;
		register int n;

		H = CMR->mr_h;
		S = eRef(X1);
		n = eNArgs((Structure) S[0]);
		if(H + n * 2 + 2 >= heapMax)
			panic("Heap Overflow somewhere nasty in univ 3");
		/*
		 * Build a list of the arguments of X1 on the heap in reverse order,
		 * unify X2 with it, and pop off those cons cells which are not needed.
		 * Not yet fully implemented.
		 */
		for(L = NIL ; n > 0; n--) {
			H[1] = L;
			L = S[n]; DeRefPure(L);         /* Fix unbound variables. */
			H[0] = L;
			L = StarToList(H);
			H += 2;
		}
		H[0] = StarToAtom(eFunctor((Structure) S[0]));
		H[1] = L;
		L = StarToList(H);
		H += 2;
		CMR->mr_h = H;                /* Superfluous */
		if(!unify(L, X2))
			return(0);
		trailOverflowSignal(0, CMR->mr_tr);
		/*
		 * BUG!  Should pop any unneeded cons cells off the heap here.
		 */
		return(1);
   	}
	when tICN:
	case tUCN:
	case tBMV:
		switch(eType(X2)) {
		when tREF:
		case tDEL: {
			register Word *H;

			H = CMR->mr_h;
			if(H + 2 >= heapMax)
				panic("Heap Overflow somewhere nasty in univ 4");
			CMR->mr_h += 2;
			H[0] = X1;
			H[1] = NIL;
			bindVariable(X2, StarToList(H));
			return(1);
		}
		when tLST: {
			register Object *L;

			L = eRef(X2);
			if(!unify(X1, L[0]) || !unify(NIL, L[1]))
				return(0);
			trailOverflowSignal(0, CMR->mr_tr);
			return(1);
		}
		when tCHR: {
			register char *s;

			s = (char *) eRef(X2);
			if(s[1] == '\0')
				return(X1 == MakeSmallInt(s[0]));
			else
				return(0);
		}

		break;
		default:
			return(0);
		}
	}
}

/*
 * Functor/3.
 * Just fails on inappropriate arguments.
 */
int
p_functor(X1, X2, X3)
register Object X1, X2, X3;
{
	DeRefPure(X1); DeRefPure(X2); DeRefPure(X3);
	switch(eType(X1)) {
	when tREF:
	case tDEL: {
		int V;

		V = 0;
		if(IsVar(X2))
			V = 1;
		else if(!IsConst(X2))
			return(0);
		if(IsVar(X3))
			V = 1;
		else if(!IsSmallInt(X3) || eSmallInt(X3) < 0)
			return(0);

		if(V != 0) {
			register Object *X;
			register Delay *CD;
			Object XS0, XS1, XS2;

			X = CMR->mr_x;
			XS0 = X[0]; XS1 = X[1]; XS2 = X[2];
			X[0] = X1;
			X[1] = X2;
			X[2] = X3;
			/* SaveForBuilding; */
			CD = makeDelay(3, WFunctor);
			mark(X1, CD);
			if(IsVar(X2))
				mark(X2, CD);
			if(IsVar(X3))		/* Need both in case of failure. */
				mark(X3, CD);
			/* RestoreAfterBuilding; */
			X[0] = XS0; X[1] = XS1; X[2] = XS2;
			return(1);
		} else {
			register int n;

			n = eSmallInt(X3);		/* Already checked */

			if(n > MAXARITY) {
				warning("Trying to build too big a term in p_functor()");
				return(0);
			}

			if(n == 0)
				switch(eUType(X2)) {
				when utATM:
				case utINT:
				case utFLT:
					return(unify(X1, X2));      /* Lazy */

				break;
				default:
					return(0);
				}
			else if(X2 == StarToAtom(&SymCons) && n == 2) {
				register Word *H;
				register Object l;

				H = CMR->mr_h;
				if(H + 2 >= heapMax)
					panic("Heap Overflow somewhere nasty in p_functor()");
				l = StarToList(H);
				*H = StarToUnboundRef(H); H++;
				*H = StarToUnboundRef(H); H++;
				CMR->mr_h = H;
				return(unify(X1, l));   /* Lazy */
			} else {
				register Word *H;
				register Object f;

				if(!IsAtom(X2))
					return(0);
				H = CMR->mr_h;
				if(H + n + 1 >= heapMax)
					panic("Heap Overflow somewhere nasty in p_functor()");
				f = StarToStr(H);
				*H++ = MakeStrHeader(n, X2);
				for( ; n > 0; n--) {
					*H = StarToUnboundRef(H);
					H++;
				}
				CMR->mr_h = H;
				return(unify(X1, f));   /* Lazy */
			}
		}
	}
	when tLST:
	case tCHR:
		return(unify(X2, StarToAtom(&SymCons)) && unify(X3, MakeSmallInt(2)));
	when tSTR: {
		register Structure header;

		header = *eRef(X1);
		return(
			   		unify(X2, StarToAtom(eFunctor(header)))
			   &&   unify(X3, MakeSmallInt(eNArgs(header))));
		}
	when tICN:
	case tUCN:
		switch(eUType(X1)) {
		when utATM:
		case utINT:
		case utI32:
		case utFLT:
			return(unify(X2, X1) && unify(X3, MakeSmallInt(0)));

		break;
		default:
			return(0);
		}
	when tBMV:
		return(unify(X2, X1) && unify(X3, MakeSmallInt(0)));
	}
}

/*
 * Arg/3.
 * Just fails on inappropriate arguments.
 *
 * Note that elements of Blocks are converted into Blocks.
 * This will do for the moment, and hopefully forever.
 */
int
p_arg(X1, X2, X3)
register Object X1, X2;
Object *X3;
{
	DeRef(X1); DeRef(X2);

	if(IsSmallInt(X1)) {
		register int n;

		n = eSmallInt(X1);
		switch(eType(X2)) {
		when tREF:
		case tDEL:
			;
		when tLST:
			if(1 <= n && n <= 2) {
				*X3 = ((Object *)eRef(X2))[n - 1];
				return(1);
			} else
				return(0);
		when tCHR:
			if(n == 1) {
				*X3 = MakeSmallInt(*(unsigned char *)eRef(X2));
				return(1);
			} else if(n == 2) {
				X2 = (Object) (1 + (unsigned char *) eRef(X2));
				if(*(unsigned char *)X2 == (unsigned char)NULL)
					*X3 = NIL;
				else
					*X3 = StarToString(X2);
				return(1);
			} else
				return(0);
		when tSTR:
			X2 = (Object) eRef(X2);
			if(n < 1 || n > eNArgs(*(Object *) X2))
				return(0);
			else {
				*X3 = ((Object *) X2)[n];
				return(1);
			}
		when tICN:
			if(IsBlock(X2) && n >= 0) {
				*X3 = MakeBlock(eRef(X2)[n]);
				return(1);
			} else
				return(0);
		when tUCN:
		case tBMV:
			return(0);
		}
	} else if(!IsVar(X1))
		return(0);
	else if(IsConst(X2) && !IsBlock(X2) || IsBMV(X2))
		return(0);

	{
		register Object *X;
		register Delay *CD;
		Object XS0, XS1, XS2;
		Object NewVar;

		X = CMR->mr_x;
		XS0 = X[0]; XS1 = X[1]; XS2 = X[2];
		X[0] = X1;
		X[1] = X2;
		X[2]
			= NewVar
			= StarToRef(CMR->mr_h);
		*CMR->mr_h = StarToUnboundRef(CMR->mr_h);
		CMR->mr_h++;
		/* SaveForBuilding; */
		CD = makeDelay(3, WArg);
		if(IsVar(X2))
			mark(X2, CD);
		if(IsVar(X1))					/* So that arg(X,_,_), X = a, fails */
			mark(X1, CD);
		/* RestoreAfterBuilding; */
		X[0] = XS0; X[1] = XS1; X[2] = XS2;
		*X3 = NewVar;		/* Need to do this here for ARG 0,1,0 */
		return(1);
	}
}

/*
 * Return the Object at the tail of a list.
 */
Object
tail(x)
register Object x;
{
	for(;;) {
		DeRefPure(x);
		switch(eType(x)) {
		when tREF:
		case tDEL:
		case tICN:
		case tUCN:
		case tSTR:
		case tBMV:
			return(x);
		when tLST:
			x = eRef(x)[1];
		when tCHR:
			return(NIL);
		}
	}
}

/*
 * Return the length of a list.
 */
int
length(x)
register Object x;
{
	register int n;

	for(n = 0; ; n++) {
		DeRefPure(x);
		switch(eType(x)) {
		when tREF:
		case tDEL:
		case tUCN:
		case tSTR:
		case tBMV:
			return(-1);
		when tICN:
			if(IsNIL(x))
				return(n);
			else
				return(-1);
		when tLST:
			x = eRef(x)[1];
		when tCHR:
			return(n + strlen((char *) eRef(x)));
		}
	}
}

/*
 * Routines to measure the size of a term in bytes.
 * Shared sub-terms are counted multiply.
 *
 * The size of the Object refering to the term is not counted in sizeOfTerm.
 *
 * Strings are always considered to take an integral number of Words,
 * so sizeOfTerm is always a multiple of sizeof(Word).
 */
Word
sizeOfEmbeddedTerm(x)
register Object x;
{
	register Object *y;
	register int n;

	DeRef(x);
	switch(eType(x)) {
	when tREF:
	case tDEL:
	case tICN:
	case tBMV:
		return(sizeof(Word));
	when tUCN:
		switch(eCType(x)) {
		when ctI32:
			return(sizeof(Word) + sizeof(Word));
		when ctFLT:
			return(sizeof(Word) + sizeof(double));
		break;
		default:
			panic("Impossible UCN type in sizeOfEmbeddedTerm");
		}
	when tLST:
		y = eRef(x);
		return(
			sizeof(Word)
			+ sizeOfEmbeddedTerm(y[0])
			+ sizeOfEmbeddedTerm(y[1]));
	when tCHR:
		return(sizeof(Word) * (1 + NWORDS(1 + strlen((char *)eRef(x)))));
	when tSTR:
		y = eRef(x);
		x = 2 * sizeof(Word);
		n = eNArgs(*y);
		if(n == 0) {
			n = *++y;
			x += sizeof(Word);
		}
		for( ; n > 0; n--)
			x += sizeOfEmbeddedTerm(y[n]);
		return(x);
	}
}

Word
sizeOfTerm(x)
register Object x;
{
	register Word t;

	DeRef(x);
	t = sizeOfEmbeddedTerm(x);
	/*
	 * Correct t for terms that require no storage after copying.
	 */
	return t - sizeof(Word);
}

/*
 * Represent tLST and tCHR terms as tSTRs.
 *
 * Note the contortions brought about by the unrepresentability of
 * Objects on the C function stack on many machines.
 *
 * N.B. the way in which invalid Objects are returned for tLST and
 * tCHR types.  These serve simply as a place to put the tag tSTR.
 */
static Object
canonicalize(X, T, U)
register Object X, *T, **U;
{
	DeRef(X);
	switch(eType(X)) {
	when tDEL:
		return(StarToRef(eRef(X)));
	when tLST: {
		register Object *p;

		p = eRef(X);
		T[0] = StrHeader2Cons;
		T[1] = p[0];
		T[2] = p[1];
		*U = T;
		return(MakeStr(0));				/* WARNING!  Invalid Object. */
	}
	when tCHR: {
		register char *s;

		s = (char *) eRef(X);
		T[0] = StrHeader2Cons;
		T[1] = MakeSmallInt(s[0]);
		if(s[1] != '\0')
			T[2] = StarToString(s + 1);
		else
			T[2] = NIL;
		*U = T;
		return(MakeStr(0));				/* WARNING!  Invalid Object. */
	}
	when tSTR:
		*U = eRef(X);
		return(X);

	break;
	default:
		return(X);
	}
}

#define pair(x, y) (eType(x) << lofType | eType(y))
#define casepair(x, y) (x << lofType | y)

#define ifuneq(x, y) if(x < y) return(-1); else if(x > y) return(1)

int
p_compare(X1, X2)
register Object X1, X2;
{
	/*
	 * Take care with these -- putting tags on things on the C function
	 * stack often isn't possible because of the layout of memory by the
	 * operating system.
	 */
	static Object T1[3], T2[3];
	Object *U1, *U2;

start:									/* Tail recursion */
	X1 = canonicalize(X1, T1, &U1);
	X2 = canonicalize(X2, T2, &U2);

	/* Get rid of the simple cases */
	ifuneq(eSType(X1), eSType(X2));

	switch(pair(X1, X2)) {
	when casepair(tREF, tREF):
		return(((Word) eRef(X1)) - ((Word) eRef(X2)));
	when casepair(tICN, tICN):
		ifuneq(eCType(X1), eCType(X2));
		else
			switch(eCType(X1)) {
			when ctINT:
				return(eSmallInt(X1) - eSmallInt(X2));
			when ctATM:
				return(strcmp(
					eCharStar(((Atom *) eRef(X1))->a_pname),
					eCharStar(((Atom *) eRef(X2))->a_pname)));
			when ctBLK:
				return(((Word) eRef(X1)) - ((Word) eRef(X2)));
			break;
			default:
				panic("Impossible constant types in p_compare");
			}
	when casepair(tICN, tUCN):
		if(IsFloat(X2)) {
			if(IsSmallInt(X1)) {
				ifuneq(eSmallInt(X1), eFloat(X2));
				else
					return(-1);
			} else
				return(1);
		} else if(IsInt32(X2)) {
			if(IsSmallInt(X1)) {
				ifuneq(eSmallInt(X1), eInt32(X2));
				else
					return(-1);
			} else
				return(1);
		} else
			panic("Impossible constant types in p_compare");
	when casepair(tUCN, tICN):
		if(IsFloat(X1)) {
			if(IsSmallInt(X2)) {
				ifuneq(eFloat(X1), eSmallInt(X2));
				else
					return(1);
			} else
				return(-1);
		} else if(IsInt32(X1)) {
			if(IsSmallInt(X2)) {
				ifuneq(eInt32(X1), eSmallInt(X2));
				else
					return(1);
			} else
				return(-1);
		} else
			panic("Impossible constant types in p_compare");
	when casepair(tUCN, tUCN):
		if(IsFloat(X1) && IsFloat(X2)) {
			register double f1, f2;

			f1 = eFloat(X1);
			f2 = eFloat(X2);
			if(f1 < f2)
				return(-1);
			else if(f1 > f2)
				return(1);
			else
				return(0);
		} else if(IsInt32(X1) && IsInt32(X2)) {
			X1 = eInt32(X1);
			X2 = eInt32(X2);
			if(X1 < X2)
				return(-1);
			else if(X1 > X2)
				return(1);
			else
				return(0);
		} else
			panic("Impossible constant types in p_compare");
	when casepair(tSTR, tSTR): {
		register Object *p1, *p2;
		register int n1, c;

		p1 = U1;
		p2 = U2;
		n1 = eNArgs(*p1);
		ifuneq(n1, eNArgs(*p2));
		else if(c =
				strcmp(
					eCharStar(eFunctor(*p1)->a_pname),
					eCharStar(eFunctor(*p2)->a_pname)))
			return(c);
		else {
			p1++;
			p2++;
			while(--n1 > 0)
				if(c = p_compare(*p1++, *p2++))
					return(c);
			X1 = *p1;
			X2 = *p2;
			goto start;
		}
	}
	when casepair(tSTR, tBMV):
		return(-1);
	when casepair(tBMV, tSTR):
		return(1);
	when casepair(tBMV, tBMV):
		return(X1 - X2);
	break;
	default:
		panic("impossible types in p_compare");
	}
}

/*
 * Some comparison functions to give to msort().
 *
 * Note: they don't insist that arguments to keySort be of the form Key-Value.
 */

static int
compareGT(x, y)
register Object x, y;
{
	return(-p_compare(x, y));
}

static int
keyCompareLT(x, y)
register Object x, y;
{
	if(IsStr(x))
		x = eRef(x)[1];

	if(IsStr(y))
		y = eRef(y)[1];

	return(p_compare(x, y));
}

static int
keyCompareGT(x, y)
register Object x, y;
{
	if(IsStr(x))
		x = eRef(x)[1];

	if(IsStr(y))
		y = eRef(y)[1];

	return(-p_compare(x, y));
}

static void
merge(p, m, q, n, r, compare)
register Object *p, *q, *r;
int m, n;
Object (*compare)();
{
	register Object *pend, *qend;

	pend = p + m,
	qend = q + n;
	for( ; ; ) {
		if((*compare)(*p, *q) <= 0) {
			*r++ = *p++;
			if(p >= pend) {
				do {
					*r++ = *q++;
				} while(q < qend);
				break;
			}
		} else {
			*r++ = *q++;
			if(q >= qend) {
				do {
					*r++ = *p++;
				} while(p < pend);
				break;
			}
		}
	}
}

static Object *
msort(x, n, w, compare)
Object *x, *w;
register int n;
int (*compare)();
{
	register Object *u, *v;

	for(u = x, v = x + n - 1; u < v; u += 2)
		if((*compare)(u[0], u[1]) > 0) {
			register Object t;

			t = u[0]; u[0] = u[1]; u[1] = t;
		}

	{	register int i, j;

		u = w;
		v = x;
		for(j = 2; j < n; j += j) {
			{	register Object *t;

				t = u; u = v; v = t;
			}

			for(i = j; i < n; i += j + j) {
				register int k;

				k = n - i;
				merge(u + i - j, j, u + i, k < j ? k : j, v + i - j, compare);
			}
			for(i -= j; i < n; i++)
				v[i] = u[i];
		}
	}

	return(v);
}

int
p_sort(k, l, m)
Object k, l, *m;
{
	register int len;
	register Object *x;
	Object *vector;
	int ck;
	int (*compare)();

	DeRef(k);
	if(!IsSmallInt(k)) {
		warning("Integer key type expected in p_sort()");
		return(0);
	}
	ck = eSmallInt(k);
	if(ck < -2 || ck > 2) {
		warning("Integer key type in [-2, 2] expected in p_sort()");
		return(0);
	}

	DeRef(l);
	len = length(l);
	if(len < 0) {
		warning("List expected in p_sort()");
		return(0);
	} else if(len == 0) {
		*m = NIL;
		return(1);
	}
	x	= vector
		= (Object *) malloc(len * 2 * sizeof(Object));
	{	register Object q;

		for(q = l; ; ) {
			DeRef(q);
			if(IsList(q)) {
				register Object *p;

				p = eRef(q);
				q = p[0]; DeRef(q);
				*x++ = q;
				q = p[1];
			} else if(IsString(q)) {
				register char *p;

				for(p = (char *) eRef(q); *p != '\0'; )
					*x++ = MakeSmallInt(*p++);
				break;
			} else
				break;
		}
	}

	switch(ck) {
	when 1:
		compare = p_compare;
	when -1:
		compare = compareGT;
	when 2:
		compare = keyCompareLT;
	when -2:
		compare = keyCompareGT;
	}
	x = msort(vector, len, vector + len, compare);

	{	register Object *p;

		/* Could check to see if this could be represented as a string. */
		heapOverflowCheck(CMR->mr_h, 2 * len);
		for(p = CMR->mr_h; len > 0; len--) {
			p[0] = *x++;
			p[1] = StarToList(p + 2);
			p += 2;
		}
		p[-1] = NIL;
		*m = StarToList(CMR->mr_h);
		CMR->mr_h = p;
	}

	free((char *) vector);

	return(1);
}

/*
 * $replacn/3.
 */
void
p_replacn(X1, X2, X3)
register Object X1, X2, X3;
{
	register Object *S;

	DeRef(X1);
	if(!IsSmallInt(X1))
		panic("Integer expected in p_replacn()");
	X1 = eSmallInt(X1);
	DeRef(X3);
	DeRef(X2);
	S = eRef(X2);
	switch(eType(X2)) {
	when tICN:
		if(!IsBlock(X2))
			panic("Term expected in p_replacn()");
		if(X1 < 0)
			panic("Integer out of range in p_replacn()");
		/*
		 * Note that it is rare for Blocks to contain tagged objects.
		 * It complements the definition of arg/3 for Blocks.
		 * Hopefully this will be sufficient to the day.
		 */
		if(IsBlock(X3))
			S[X1] = (Object) eRef(X3);
		else
			S[X1] = X3;
	when tLST:
		if(X1 < 1 || X1 > 2)
			panic("Integer out of range in p_replacn()");
		S[X1 - 1] = X3;
	when tCHR:
		switch(X1) {
		when 1:
			if(!IsSmallInt(X3) || eSmallInt(X3) <= 0 || eSmallInt(X3) > 255)
				panic("Trying to p_replacn() a non-char in a string");
			((char *) S)[0] = eSmallInt(X3);
		when 2:
			if(!IsNIL(X3))
				panic("Trying to p_replacn() a string's cdr with non-NIL");
			((char *) S)[1] = '\0';
		break;
		default:
			panic("Integer out of range in p_replacn()");
		}
	when tSTR:
		X2 = eNArgs(S[0]);
		if(X1 < 0 || X1 > X2)
			panic("Integer out of range in p_replacn()");
		S[X1] = X3;

	break;
	default:
		panic("Term expected in p_replacn()");
	}
}

/*
 * Get the address of a word for the other replacn and setarg functions.
 */
static Object *
argn(X1, X2)
register Object X1, X2;
{
	register Object *S;

	DeRef(X1);
	if(!IsSmallInt(X1)) {
		warning("Integer expected in argn()");
		return(NULL);
	}
	X1 = eSmallInt(X1);
	DeRef(X2);
	S = eRef(X2);
	switch(eType(X2)) {
	when tICN:
		if(!IsBlock(X2)) {
			warning("Term expected in argn()");
			return(NULL);
		}
		if(X1 < 0) {
			warning("Integer out of range in argn()");
			return(NULL);
		}
		return(S + X1);
	when tLST:
		if(X1 < 1 || X1 > 2) {
			warning("Integer out of range in argn()");
			return(NULL);
		}
		return(S + X1 - 1);
	when tCHR:
		return(NULL); /* panic("Can't change a string with p_setarg()"); */
	when tSTR:
		X2 = eNArgs(S[0]);
		if(X1 < 0 || X1 > X2) {
			warning("Integer out of range in argn()");
			return(NULL);
		}
		return(S + X1);

	break;
	default:
		panic("Term expected in argn()");
	}
}

#define H CMR->mr_h
#define HB CMR->mr_hb
#define B CMR->mr_b
#define TR CMR->mr_tr

/*
 * $setarg/3.
 */
int
p_setarg(X1, X2, X3)
register Object X1, X2, X3;
{
	register Object *S;

	DeRef(X3);
	if(IsRef(X3) && (Word *)eRefStar(X3) >= H) {   /* Grunge */
		heapOverflowCheck(H, 1);
		*H = StarToUnboundRef(H);
		bindS(eRefStar(X3), StarToRef(H), TR);
		X3 = StarToRef(H);
		H++;
	}
	DeRef(X2);
	S = argn(X1, X2);
	if(S == NULL)
		return 0;
	switch(eType(X2)) {
	when tICN:
		/*
		 * Note that it is rare for Blocks to contain tagged objects.
		 * It complements the definition of arg/3 for Blocks.
		 * Hopefully this will be sufficient to the day.
		 */
		if(IsBlock(X3))
			X3 = (Object) eRef(X3);
	case tLST:							/* Fall Through! */
	case tSTR:
		bindH2(S, X3, TR);
	when tCHR:
		return 0; /* panic("Can't change a string with p_setarg()"); */

	break;
	default:
		return 0; /* panic("Term expected in p_setarg()"); */
	}
	return 1;
}

/*
 * $replacn_var/3.
 */
void
p_replacn_var(X1, X2, X3)
register Object X1, X2, *X3;
{
	register Object *S;

	S = argn(X1, X2);
	if(S == NULL)
		return;
	*S = StarToUnboundRef(S);
	*X3 = StarToRef(S);
	return;
}

/*
 * $setarg_var/3.
 */
int
p_setarg_var(X1, X2, X3)
register Object X1, X2, *X3;
{
	register Object *S;

	S = argn(X1, X2);
	if(S == NULL)
		return 0;
	bindH2(S, StarToUnboundRef(S), TR);
	*X3 = StarToRef(S);
	return 1;
}

#undef H
#undef HB
#undef B
#undef TR

int
p_aref(X1, X2, X3, flags)
Object X1, X2, *X3;
int flags;
{
	register Object *a;
	register Functor *f;
	register int n, i;
	register Object chunk;
	int index;
	int log2n;
	int s;
	int l;

	DeRef(X1);
	if(!IsInt(X1))
		return(0);
	index = eInt(X1);
	if(index < 0) {
		*X3 = NIL;
		return(flags != 0);
	}

	chunk = X2; DeRef(chunk);
	if(!IsStr(chunk))
		return(0);
	a = eRef(chunk);
	f = (Functor *) a[0];
	if(f->f_functor != &SymArray)
		return(0);
	n = f->f_arity;
	switch(n) {
	when 5: log2n = 2;
	when 9: log2n = 3;
	when 17: log2n = 4;
	when 3: log2n = 1;
	break;
	default:
		return(0);
	}
	l = eSmallInt(a[n]);
	if((l >> 17) & 01) {
		warning("Attempt to use old version of current-version-only array.");
		return(0);
	}
	s = (l >> 18) & 0x1f;
	if(index >= 4 << s) {
		*X3 = NIL;
		return(flags != 0);
	}
	for( ; s > 0; s -= log2n) {
		i = (index >> s) & (n - 2);
		if((l & (1 << i)) == 0) {
			*X3 = NIL;
			return(flags != 0);
		}
		chunk = a[i + 1]; DeRef(chunk);
		if(!IsStr(chunk))
			return(0);
		a = eRef(chunk);
		if(f != (Functor *) a[0])
			return(0);
		l = eSmallInt(a[n]);
	}
	i = index & (n - 2);
	if((l & (1 << i)) == 0) {
		*X3 = NIL;
		return(flags != 0);
	}
	chunk = a[i + 1]; DeRef(chunk);
	*X3 = chunk;
	return(1);
}

int
p_aset(X1, X2, X3, X4, flags)
Object X1, X2, X3, *X4;
int flags;
{
	register Object *a;
	register Functor *f;
	register int n, i;
	register Object chunk;
	Word *h;
	int index;
	int log2n;
	int s, l, destructive;

#define H CMR->mr_h
#define HB CMR->mr_hb
#define B CMR->mr_b
#define TR CMR->mr_tr

	DeRef(X1);
	if(!IsInt(X1))
		return(0);
	index = eInt(X1);
	if(index < 0)
		return(0);

	chunk = X2; DeRef(chunk);
	if(!IsStr(chunk))
		return(0);
	a = eRef(chunk);
	f = (Functor *) a[0];
	if(f->f_functor != &SymArray)
		return(0);
	n = f->f_arity;
	switch(n) {
	when 5: log2n = 2;
	when 9: log2n = 3;
	when 17: log2n = 4;
	when 3: log2n = 1;
	break;
	default:
		return(0);
	}
	l = eSmallInt(a[n]);
	if((l >> 17) & 01) {
		warning("Attempt to use old version of current-version-only array.");
		return(0);
	}
	s = (l >> 18) & 0x1f;
	destructive = l & 0x10000;

	DeRef(X3);				/* Get it over with. */

	h = CMR->mr_h;

	if(destructive) {
		/* Copy the header record and mark the old one destroyed. */
		for(i = n; i > 0; i--)
			h[i] = a[i];
		h[0] = (Word)f;
		trailH2(a[n], a + n, TR);
		a[n] = MakeSmallInt(eSmallInt(a[n]) | (destructive << 1));
		a = h;
		X2
			= chunk
			= StarToStr(h);
		h += n + 1;
	}

	while(index >= 4 << s) {
		for(i = n - 1; i > 1; i--)
			h[i] = NIL;
		h[1] = chunk;
		h[0] = (Word)f;
		s += log2n;
		l = (s << 18) | destructive | 1;
		h[n] = MakeSmallInt(l);
		a = h;
		X2
			= chunk
			= StarToStr(h);
		h += n + 1;
	}

	if(destructive) {
		for( ; s > 0; s -= log2n) {
			i = (index >> s) & (n - 2);
			if((l & (1 << i)) == 0) {
				trailH2(a[i + 1], a + i + 1, TR);
				trailH2(a[n], a + n, TR);
				for( ; s > 0; s -= log2n) {
					register int j;

					i = (index >> s) & (n - 2);
					for(j = n - 1; j > 0; j--)
						h[j] = NIL;
					h[0] = (Word)f;
					a[i + 1] = StarToStr(h);
					a[n] = MakeSmallInt(l | (1 << i));
					l = ((s - log2n) << 18) | destructive;
					h[n] = MakeSmallInt(l);
					a = h;
					h += n + 1;
				}
				break;
			} else {
				chunk = a[i + 1]; DeRef(chunk);
				if(!IsStr(chunk))
					return(0);
				a = eRef(chunk);
				if(f != (Functor *) a[0])
					return(0);
				l = eSmallInt(a[n]);
			}
		}
		i = index & (n - 2);
		trailH2(a[i + 1], a + i + 1, TR);		/* Lazy! */
		trailH2(a[n], a + n, TR);				/* Lazy! */
	} else {
		/* Make new header record. */
		for(i = n; i > 0; i--)
			h[i] = a[i];
		h[0] = (Word)f;
		a = h;
		X2
			= chunk
			= StarToStr(h);
		h += n + 1;

		for( ; s > 0; s -= log2n) {
			i = (index >> s) & (n - 2);
			if((l & (1 << i)) == 0) {
				register int j;

				for(j = n - 1; j > 0; j--)
					h[j] = NIL;
				h[0] = (Word)f;
				a[i + 1] = StarToStr(h);
				a[n] = MakeSmallInt(l | (1 << i));
				l = (s - log2n) << 18;
				h[n] = MakeSmallInt(l);
				a = h;
				h += n + 1;
			} else {
				chunk = a[i + 1]; DeRef(chunk);
				a[i + 1] = StarToStr(h);
				if(!IsStr(chunk))
					return(0);
				a = eRef(chunk);
				if(f != (Functor *) a[0])
					return(0);
				l = eSmallInt(a[n]);
				for(i = n; i > 0; i--)
					h[i] = a[i];
				h[0] = (Word)f;
				a = h;
				h += n + 1;
			}
		}
		i = index & (n - 2);
	}
	CMR->mr_h = h;
	a[i + 1] = X3;
	a[n] = MakeSmallInt(l | (1 << i));
	*X4 = X2;
	return(1);

#undef H
#undef HB
#undef B
#undef TR
}

/*
 * Put the elements of a Prolog list into an array of Objects.
 * The array is either base[len] if base is non-NULL, or malloced.
 * The array of Objects is terminated with a 0.
 *
 * Returns pointer to start of array.
 */
Object *
listToObjects(X, base, len)
register Object X;
Object *base;
int len;
{
	register Object *p;
	int n;

	n = length(X);
	if(n < 0)
		return((Object *)NULL);
	if(base == (Object *)NULL) {
		base = (Object *)malloc(sizeof(Object) * (n + 1));
		if(base == (Object *)NULL)
			panic("Can't malloc() space in listToObjects()");
	} else if(len <= n)
		panic("List too long in listToObjects()");

	p = base;

	while(IsList(X)) {
		register Object *L;

		L = eRef(X);

		X = L[0]; DeRef(X);
		*p++ = X;
		X = L[1]; DeRef(X);
	}
	if(IsString(X)) {
		register char *s;

		for(s = (char *) eRef(X); *s != '\0'; s++)
			*p++ = MakeSmallInt(*s);
	}
	*p = 0;

	return(base);
}
